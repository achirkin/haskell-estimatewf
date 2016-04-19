-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
--
-----------------------------------------------------------------------------

module Main (
    main
) where


--import Data.List
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
--import qualified Data.Double.Conversion.ByteString as DC
--import Workflow
import Test.WorkflowTest
--import Workflow.Toolset
--import Data.Array.Unboxed
--import Data.Complex
--import qualified Data.Map as Map

--import Test.DocTest
--
--
--
--main :: IO ()
--main = doctest ["-isrc", "src/Main.hs"]



--main = do
--    size <- readLn :: IO Int
--    let x1 = xs 100000
--        s1 = x1 `seq` if size > 0 then elems . RTESystem.Toolset.sort $ x1
--                                  else Data.List.sort . elems $ x1
--    writeCSV "data.csv" $ map (\x -> [x]) s1

main :: IO ()
main = do
    size <- readLn :: IO Int
    r <- test size 5.0
    writeCSV "data.csv" . map (\(x,y,z,u) -> [x,y,z,u]) $ r

--main = do
--    size <- readLn :: IO Int
--    let (bnds,wf) = calculateCDF wx 3.0 size
--        --st = flip (Map.!) 1 . nodes . task $ wf
--        x = fst lsp
--        t = snd lsp
--        qu = quantiles wf
--        qu' = charToDist lsp . waves $ wf
--        wa = waves wf
--        wa' = distToChar lsp . quantiles $ wf
--            lsp = linSpace size bnds
--    writeCSV "dataQ.csv" $ zipWith3 (\x a b -> [x,a,b]) x qu qu'
--    writeCSV "dataW.csv" $ zipWith3 (\t a b -> [show t,sh a,sh b]) t wa wa'
--        where sh (x:+y) = show x ++ (if y < 0 then " - " ++ show (-y) else " + " ++ show y) ++ "i"
    --g <- newStdGen --getStdGen
    --let vals = take size (randoms g :: [Double])
    --let vals = take size (randoms g :: [Double])
    --print vals
    --let distr = zipWith3 (\a b c -> [a,b,c]) vals (genGumbel g 2.45 6.23 size) (genWeibull g 3.0 1.8 size)
    --writeCSV "data.csv" distr
    --B.writeFile "data.csv" $ B.tail . B.tail . B.concat . map (BC.pack . (:) ';' . (:) '\n' . show) $ vals



writeCSV :: (Show a) => FilePath -> [[a]] -> IO()
writeCSV path [[]] = B.writeFile path B.empty
writeCSV path [] = B.writeFile path B.empty
writeCSV path (start:rest) = B.writeFile path $
    B.concat $ (:) (BC.pack . createline $ start) (map (BC.pack . (:) '\n' . createline) rest)
  where
    createline (x:xs) = show x ++ (concat . map ((:) ',' . show) $ xs)
    createline []     = ""


