{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Workflow
    ( WorkflowStage, prev, quantiles, waves, task, meanT, varT
    , Task(..)
    , Indices, Nodes
    , WFType(Sequential, Parallel, Complex)
    , wrap, calculateCDF, calculateCDF', distributionBounds
    , stageOrder,applyDE,applyRS,reverseEdges
    ) where
import Data.Array.Unboxed (listArray)
import Data.Complex (Complex)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector as Vector
import Data.Maybe (fromMaybe)

import Workflow.Toolset

-- | List of indices of the stages in Workflow Nodes Map
type Indices = [Int]

-- | Map structure, stores all stages of the workflow
type Nodes = Map.Map Int WorkflowStage

-- | A stage of the workflow - can be either an atomic task or a workflow
data WorkflowStage = Stage
    { prev      :: Indices          -- ^ previous stages
    , quantiles :: [Double]         -- ^ values of CDF at certain points
    , waves     :: [Complex Double] -- ^ values of characteristic functions at certain points
    , task      :: Task             -- ^ task (or sub-workflow) for execution at this stage
    , meanT     :: Double           -- ^ mean execution time estimation
    , varT      :: Double           -- ^ estimation of execution time variance
    } deriving (Eq)


-- | Representation of the task - it can be a single task or a package
data Task = AtomicTask
    { sample :: Array1D Double -- ^ adjusted data (T' = mu' + sigma' (T(X) - mu(X)) / sigma(X) )
    } | Workflow
    { nodes  :: Nodes   -- ^ list of stages and their predecessors
    , leafs  :: Indices -- ^ last tasks in the workflow
                        --   (workflow is fully executed if these tasks are finished)
    , wftype :: WFType  -- ^ whether this is primitive workflow or complex one
    } deriving (Eq)

-- | Type of the workflow.
--   Depending on the type of the workflow, distribution is calculated in different ways
data WFType = Sequential -- ^ can apply RS reduction
            | Parallel   -- ^ can apply RF reduction
            | Complex    -- ^ cannot reduce workflow
            deriving (Eq,Show)


-- | Wrap the task into workflow stage
--   Arguments: current task, and an array of previous (required) stages
wrap :: Task -> Indices -> WorkflowStage
wrap t@(AtomicTask {sample=arr}) preds = Stage
    { prev      = preds
    , task      = t
    , meanT     = mean arr
    , varT      = var arr
    , quantiles = undefined
    , waves     = undefined
    }
wrap t@(Workflow {nodes=ns,leafs=xs}) preds = Stage
    { prev      = preds
    , task      = t
    , meanT     = foldr1 max (xs >>= f meanT)
    , varT      = foldr1 max (xs >>= f varT)
    , quantiles = undefined
    , waves     = undefined
    }
  where
    f r i = g r ((Map.!) ns i)
    g r x@(Stage {prev=[]}) = [r x]
    g r x = map (+ r x) . concat . map (f r) . prev $ x

-- |
-- Calculate probability distribution of the workflow execution time
-- Arguments: workflow, bounds size (in sigmas), grid density k : n = 2^k
-- Return: ((a,b),wf); (a,b) - integration bounds, wf - resulting workflow
calculateCDF :: WorkflowStage -> Double -> Int -> ((Double,Double),WorkflowStage)
calculateCDF s q k = (bnds, ccdf (map (\x' -> x'-m*c) x, t) s) where
    grid@(x,t) = linSpace k bnds
    bnds@(a,b) = distributionBounds s q
    c = (b-a) / (fromIntegral . length . fst $ grid)
    m = fromIntegral . (round :: Double -> Int) $ meanT s / c


-- |
-- Calculate probability distribution of the workflow execution time
-- Arguments: workflow, grid
-- Return: ((a,b),wf); (a,b) - integration bounds, wf - resulting workflow
calculateCDF' :: WorkflowStage -> ([Double],[Double]) -> ((Double,Double),WorkflowStage)
calculateCDF' s grd = (bnds, ccdf (map (\x' -> x'-m*c) x, t) s) where
    grid@(x,t) = grd
    bnds@(a,b) = (head x, last x)
    c = (b-a) / (fromIntegral . length . fst $ grid)
    m = fromIntegral . (round :: Double -> Int) $ meanT s / c


-- | calculate cdf with grid as input
ccdf :: ([Double],[Double]) -> WorkflowStage -> WorkflowStage
ccdf g@(xGrid,tGrid) s = s{
    quantiles = qu,
    waves = wa,
    task  = newt
    } where
        n = length xGrid
        n2 = length tGrid
        c = (last xGrid - head xGrid) / fromIntegral (n-1)
        m = fromIntegral . (round :: Double -> Int) $ meanT s / c
        xg = map (+m*c) xGrid
        qu = df newt
        wa = cf newt
        newt = parseInner . task $ s
        parseInner t@(AtomicTask {}) = t
        parseInner t@(Workflow {nodes=ns}) = t{nodes = Map.map (ccdf g) ns}
        df (AtomicTask {sample=arr}) = distributionF arr xg
        df (Workflow {nodes=_ns,wftype=Sequential})
            = charToDist (map (+m*c) xGrid, tGrid) wa
        df (Workflow {nodes=ns,wftype=Parallel})
            = Map.fold (zipWith (*) . quantiles) (replicate n 1) ns
        df (Workflow {nodes=ns,leafs=lfs,wftype=Complex})
            = foldr (\(phi1,eX1) (f2) ->
                let m1 = min m $ fromIntegral . (round :: Double -> Int) $ eX1 / c
                    f1 = charToDist (map (+m1*c) xGrid, tGrid) phi1
                    md = round $ m - m1
                    f1' = drop md f1 ++ replicate md 1
                in zipWith (*) f2 f1'
                ) (replicate n 1) (lfs >>= f) where
                f i = gx ((Map.!) ns i)
                gx x@(Stage {prev=[]}) = [(waves x, meanT x)]
                gx x = map ((\(a,b) (e,d) -> (zipWith (*) a e,b+d)) (waves x, meanT x))
                    . concat . map f . prev $ x
        cf (AtomicTask {sample=arr}) = characteristicF arr tGrid
        cf (Workflow {nodes=ns,wftype=Sequential})
            = Map.fold (zipWith (*) . waves) (replicate n2 1) ns
        cf (Workflow {})
            = distToChar (xg, tGrid) qu


-- | Approximate bounds, such that total distribution almost fully lies in these bounds.
--   "k" stands for k-sigma rule (i.e. interval width is 2k*sigma).
--   (a,b) : 0 <= a <= b
distributionBounds :: WorkflowStage -> Double -> (Double,Double)
distributionBounds x q = (max 0 $ m - s, m + s) where
    m = meanT x
    s = q * (sqrt . varT $ x)


-- | Apply DE-reduction (delete redundant edges)
applyDE :: Task -> Task
applyDE t@(AtomicTask {}) = t
applyDE wf@(Workflow {nodes = ns}) = wf{nodes = Map.mapWithKey (\i s -> modify ((Map.!) orders i) s ) ns} where
    orders = stageOrder wf
    modify _ s@(Stage {prev=[]}) = s
    modify _ s@(Stage {prev=[_]}) = s
    modify [_] s = s
    modify ((o,_):ords) s = s{prev = fst $ foldr f ([],[]) ords} where
        f (_,is) (os,iss) = ( mergeOrderedLists os fis
                            ,foldr mergeOrderedLists iss . map (prevTillOrder (o-1)) $ fis
                            ) where fis = filter (not . flip isInOrderedList iss) is
    modify [] s = s
    prevTillOrder o i = if so <= o then [] else foldr mergeOrderedLists xs ys where
        s = (Map.!) ns i
        xs = prev s
        ys = map (prevTillOrder o) xs
        so = fst . last $ (Map.!) orders i

-- | Apply RS-reduction (Reduce Sequential stages to single stage)
applyRS :: Task -> Task
applyRS t@(AtomicTask {}) = t
applyRS wf@(Workflow {nodes = ns}) = wf{nodes = Map.union changed unchanged} where
    re = reverseEdges wf
    candidates =
        Map.filterWithKey (\i _ -> ((1/=) . length . (Map.!) re $ i) ||
        ((1>) . length . prev . (Map.!) ns . head . (Map.!) re $ i))
        . Map.filter ((1==) . length . (Map.!) re . head . prev )
        . Map.filter ((1==) . length . prev) $ ns
    appendPath Stage {prev=[]} = []
    appendPath Stage {prev=[i]} = if (1==) . length . (Map.!) re $ i then i:(appendPath $ (Map.!) ns i) else []
    appendPath Stage {} = []
    allSeqs = Map.mapWithKey (\i s -> i:(appendPath s)) candidates
    unchanged = Map.filterWithKey (\i _ -> isInOrderedList i unchKeys) ns where
        unchKeys = Map.keys ns List.\\ (concat . Map.elems $ allSeqs)
    changed = Map.map createSubWF allSeqs
    createSubWF xs = wrap Workflow {
        nodes = nnns,
        leafs = [head xs],
        wftype = Sequential
        } preds where
            nns = Map.filterWithKey (\i _ -> any (i==) xs) ns
            nnns = Map.adjust (\s -> s{prev=[]}) (last xs) nns
            preds = prev . (Map.!) ns . last $ xs

-- | helper function: return a map with tasks' execution orders
-- required to proceed with Delete Edge (DE) reduction
-- Arguments: Workflow
-- return map (Index of node -> value), where value: (order of this stage o, indices of previous stages with order o-1)
stageOrder :: Task -> Map.Map Int [(Int,Indices)]
stageOrder AtomicTask {} = Map.empty
stageOrder Workflow {leafs = lfs, nodes = ns} = List.foldl' ind' Map.empty lfs where
    ind' m i = m `seq` ind m ((Map.!) ns i) i
    ind m (Stage {prev=[]}) i = if Map.member i m then m else Map.insert i [(1, [])] m
    ind m (Stage {prev=xs}) i = if Map.member i m then m else
                Map.insert i (List.foldr insertNode [] . map createNode $ xs) cm where
                    cm = List.foldl' ind' m xs
                    createNode j = ((1+) . fst . last . (Map.!) cm $ j,[j])
    insertNode x [] = [x]
    insertNode x@(o,i) xxs@((o1,i1):xs)
        | o == o1   = (o1, mergeOrderedLists i i1):xs
        | o <  o1   = x:xxs
        | otherwise = (o1,i1):(insertNode x xs)

-- test it: giveWorkflow >>= mapM_ print . Map.toList . stageOrder . task

-- | helper function: return a map with "next" edges created from "prev" field of (next) stages
reverseEdges :: Task -> Map.Map Int Indices
reverseEdges AtomicTask {} = Map.empty
reverseEdges Workflow{nodes = ns} =
    Map.foldWithKey f (Map.fromAscList . zip (Map.keys ns) . repeat $ []) ns where
        f i Stage{prev = xs} m = foldr (Map.adjust (i:)) m xs


-- | helper function: merge two ordered lists into one ordered list - O(n+m)
mergeOrderedLists :: (Ord a) => [a] -> [a] -> [a]
mergeOrderedLists [] xs = xs
mergeOrderedLists xs [] = xs
mergeOrderedLists (x1:xs1) (x2:xs2) | x1 == x2  = x1 :(mergeOrderedLists xs1 xs2)
                                    | x1 > x2   = x2 :(mergeOrderedLists (x1:xs1) xs2)
                                    | otherwise = x1 :(mergeOrderedLists xs1 (x2:xs2))

-- | helper function: if x is a member of an ordered list - O(n)
isInOrderedList :: (Ord a) => a -> [a] -> Bool
isInOrderedList _ [] = False
isInOrderedList x [y] = x == y
isInOrderedList x (y:ys) | x == y    = True
                         | x >  y    = isInOrderedList x ys
                         | otherwise = False


instance Show Task where
    show AtomicTask {} = "AtomicTask"
    show Workflow {wftype = tp, nodes = ns, leafs = lfs} =
        show tp ++ " Workflow: leafs = " ++ show lfs ++
        "; nodes = " ++ show (Map.toList ns) ++ "."

instance Show WorkflowStage where
    show Stage {prev = [], task = (AtomicTask {})} = "AtomicTask"
    show Stage {prev = pr, task = (AtomicTask {})} = "AtomicTask (prev = " ++ show pr ++ ")"
    show Stage {prev = [], task = wf@Workflow {}} = show wf
    show Stage {prev = pr, task = (Workflow {wftype = tp, nodes = ns})} =
        show tp ++ " Workflow (size = " ++ show (Map.size ns) ++ ", prev = " ++ show pr ++ ")"


instance FromJSON Task where
    parseJSON (Object v) = do
        mleafs <- v .:? "leafs"
        mtasks <- v .:? "tasks"
        case (,) <$> mleafs <*> mtasks of
          Nothing -> AtomicTask . (\a -> listArray (1,length a) a) <$> v .: "sample"
          Just (lfs, tasks) -> return $ if Map.size tasks == 1
                               then task . head $ Map.elems tasks
                               else Workflow tasks lfs (inferWFType tasks lfs)
      where
       -- Safely escape (hopefully) impossible workflows
       inferWFType stages _ | Map.null stages = Complex
       -- check if fully sequential workflow
       inferWFType stages [oneleaf] = goChain stages oneleaf 1
       -- check if fully parallel workflow
       inferWFType stages lfs = if (length lfs == length stages)
                                    && (all (\s -> prev s == prev st) stgs)
                                then Parallel else Complex
                              where
                                st:stgs = Map.elems stages
       -- go through sequence of nodes to check if it is a sequential wf
       goChain stages lf n |   n == Map.size stages
                            && all (flip Map.notMember stages) (prev (stages Map.! lf)) = Sequential
                           |  p <- (prev (stages Map.! lf))
                              ,  n < Map.size stages
                              && length p == 1
                              && Map.member (head p) stages = goChain stages (head p) (n+1)
                           | otherwise = Complex
    parseJSON invalid = typeMismatch "Task" invalid


data IdTask = IdTask Int WorkflowStage

instance FromJSON IdTask where
    parseJSON ov@(Object v) = IdTask
        <$> v .: "id"
        <*> parseJSON ov
    parseJSON invalid = typeMismatch "(Int, Task)" invalid

instance FromJSON Nodes where
    parseJSON (Array v) = Map.fromList . map (\(IdTask i t) -> (i, t)) . Vector.toList <$> mapM parseJSON v
    parseJSON invalid = typeMismatch "AtomicTask" invalid

instance FromJSON WorkflowStage where
    parseJSON ov@(Object v) = do
      t  <- parseJSON ov
      mdeps <- v .:? "deps"
      return $ wrap t (fromMaybe [] mdeps)
    parseJSON invalid = typeMismatch "WorkflowStage" invalid


