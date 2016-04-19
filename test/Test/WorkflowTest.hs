module Test.WorkflowTest where

import Workflow
import Workflow.Toolset
import Data.Array.Unboxed
import qualified Data.Map as Map
import Test.Toolset
import System.Random
import qualified Data.List as List

-- Set up S=0..200; lambda=0..30; k=1.5..10


-- | generate Atomic tasks with the samples of size n
genAtomicTasks :: RandomGen a => a -> Int -> [(Double,Double,Double)] -> [Task]
genAtomicTasks gen n args = zipWith f (gens gen) args where
    gens x = a:(gens b) where (a,b) = split x
    f g (s,lambda,k) = AtomicTask {sample=listArray (1,n) . take n . map (s+) $ genWeibull g lambda k}

-- | generate parameters for distributions s, lambda k
genATParams :: RandomGen a => a -> [(Double,Double,Double)]
genATParams gen = zip3 (genUniform a 0 200) (genUniform b 0 30) (genUniform c 1.5 10) where
    (a,u) = split gen
    (b,c) = split u

-- | convert whole workflow into single atomic tasks
-- !!! Works only if run samples are in their original order and can be mapped one-to-one for all tasks !!!
wfToAtomic :: Task -> Task
wfToAtomic t@(AtomicTask {}) = t
wfToAtomic (Workflow {nodes=ns,leafs=lfs}) = let
    nns = Map.map (\s -> wrap (wfToAtomic . task $ s) (prev s)) ns
    nsample = foldr1 (azipWith max) (lfs >>= f)
    f i = gx ((Map.!) nns i)
    gx x | null . prev $ x = [sample . task $ x]
         | otherwise       = map (azipWith (+) (sample . task $ x)) . concat . map f . prev $ x
    in AtomicTask {sample = nsample}

sampleSize :: Int
sampleSize = 200

workflowDependencies :: [[Int]]
workflowDependencies =
    [ []
    , []
    , [1]
    , [1,2]
    , [3,4]
    , [5]
    , [3]
    , [3,6,7]
    , [6]
    , [4,6]
    , [8,9,10]
    , [10]
    ]

--workflowDependencies =
--    [ []
--    , [1]
--    , [2]
--    , [3]
--    , [4]
--    , [5]
--    , [6]
--    , [7]
--    , [8]
--    , [9]
--    , [10]
--    , [11]
--    ]

numPkg :: Int
numPkg = 12

qSigmas :: Double
qSigmas = 5.0

numPointsPower :: Int
numPointsPower = 6

test :: Int -> Double -> IO [(Double,Double,Double,Double)]
test numPP qSig = do
    rg <- newStdGen
    let atasks = take numPkg . genAtomicTasks rgl sampleSize . genATParams $ rgr
        (rgl,rgr) = split rg
        sortSample z@(AtomicTask {sample = s}) = z{sample = asort s}
        sortSample _ = undefined
        ordSampleTasks = map sortSample atasks
        oSstages = zipWith wrap ordSampleTasks workflowDependencies
        astages = zipWith wrap atasks workflowDependencies
        w' = Workflow {leafs = [11,12], wftype = Complex, nodes = Map.fromList $ zip [1..] oSstages}
        wx' = wrap w' []
        wxo = flip wrap [] . applyRS . applyDE $ w'
        wf = calculateCDF wx' qSig numPP
        wfo = calculateCDF' wxo grid
        ws = flip wrap [] . sortSample . wfToAtomic $
          Workflow {leafs = [11,12], wftype = Complex, nodes = Map.fromList $ zip [1..] astages}
        wsf = calculateCDF' ws grid
        coordsWF = fst grid
        grid = linSpace numPP . fst $ wf
        quWF = quantiles . snd $ wf
        quWSF = quantiles . snd $ wsf
        quWFO = quantiles . snd $ wfo
--    print "Precise, bounds"
--    print $ fst wsf
--    print $ "EX = " ++ show (meanT . snd $ wsf) ++ " VarX = " ++ show (varT . snd $ wsf)
--    print "Quantiles"
--    mapM_ print $ zip coordsWF $ quantiles . snd $ wsf
--    print "Estimate, bounds"
--    print $ fst wf
--    print $ "EX = " ++ show (meanT . snd $ wf) ++ " VarX = " ++ show (varT . snd $ wf)
--    print "Quantiles"
--    mapM_ print $ zip coordsWF $ quantiles . snd $ wf
    return $ List.zip4 coordsWF quWSF quWF quWFO
    --mapM_ print $ map varT . Map.elems . nodes . task . snd $ wf
    --print wf



testReal :: Int -> Double -> IO [(Double,Double,Double,Double)]
testReal numPP qSig = do
--    rg <- newStdGen
    let wxo = flip wrap [] . applyRS . applyDE $ w
        wf = calculateCDF wx qSig numPP
        wfo = calculateCDF' wxo grid
        coordsWF = fst grid
        grid = linSpace numPP . fst $ wf
        quWF = quantiles . snd $ wf
        quWSF = quantiles . snd $ wf
        quWFO = quantiles . snd $ wfo
    return $ List.zip4 coordsWF quWSF quWF quWFO


giveWorkflow :: IO WorkflowStage
giveWorkflow = do
    rg <- newStdGen
    let atasks = take numPkg . genAtomicTasks rgl sampleSize . genATParams $ rgr
        (rgl,rgr) = split rg
        sortSample z@(AtomicTask {sample = s}) = z{sample = asort s}
        sortSample _ = undefined
        ordSampleTasks = map sortSample atasks
        oSstages = zipWith wrap ordSampleTasks workflowDependencies
        w' = Workflow {leafs = [11,12], wftype = Complex, nodes = Map.fromList $ zip [1..] oSstages}
        wx' = wrap w' []
        wf = calculateCDF wx' qSigmas numPointsPower
    return $ snd wf


--uniformSample n (a,b) = n `seq` a `seq` b `seq` c `seq` listArray (1,n) $
--    [i `seq` a + c*i | i <- [0..]] :: Array1D Double
--    where c = (b-a) / fromIntegral (n-1)

--tSample = uniformSample 200 (0,1)

-- Optimized sample
--s1 = [2.2411,2.5381,2.5481,2.5731,2.9192,2.9362,3.0232,3.0392,3.0692,3.0712,3.0712,3.0842,3.1052,3.1162,3.1182,3.1192,3.1232,3.1252,3.1382,3.1562,3.1772,3.2212,3.2262,3.2742,3.2772,3.3792,3.4362,3.5702,3.5842,3.6032,3.6152,3.6512,3.6752,3.7092,3.7342,3.7342,3.7542,3.7682,3.7832,3.7832,3.8232,3.8282,3.8512,3.8522,3.8832,3.8942,3.9012,3.9632,3.9682,3.9752,3.9762,4.0002,4.0152,4.0282,4.0612,4.0702,4.1132,4.1442,4.1532,4.1732,4.2972,4.3062,4.3142,4.3272,4.4093,4.4623,4.5183,4.6733,4.7823,4.8103,4.9113,5.0153,5.0973,5.1663,5.6953] :: [Double]
--s2 = map (*1000) [0.9980,0.9997,1.0003,1.0007,1.0010,1.0015,1.0017,1.0018,1.0021,1.0021,1.0025,1.0027,1.0033,1.0037,1.0040,1.0041,1.0048,1.0061,1.0061,1.0063,1.0064,1.0068,1.0072,1.0076,1.0079,1.0083,1.0087,1.0091,1.0092,1.0093,1.0093,1.0099,1.0100,1.0106,1.0108,1.0109,1.0113,1.0121,1.0121,1.0129,1.0132,1.0135,1.0135,1.0148,1.0150,1.0152,1.0152,1.0152,1.0156,1.0159,1.0162,1.0162,1.0163,1.0165,1.0167,1.0171,1.0176,1.0177,1.0177,1.0178,1.0181,1.0183,1.0189,1.0191,1.0192,1.0195,1.0199,1.0202,1.0205,1.0205,1.0226,1.0231,1.0233,1.0245,1.0252] :: [Double]
--s3 = [253.3530,255.2852,255.5596,255.5747,255.7624,257.1405,257.4648,257.5024,257.8915,258.3903,258.4337,258.6621,258.7195,258.7418,258.7658,258.9950,259.0678,259.2254,259.3574,259.5142,259.8404,259.9796,259.9974,260.1259,260.3576,260.4081,260.5467,260.6982,261.4761,261.5946,261.8769,261.9480,262.1552,262.1986,262.4814,262.5400,262.5840,262.6018,262.8688,262.9254,262.9512,263.3166,263.4168,263.6038,263.6926,263.8585,264.0199,264.0948,264.0994,264.1358,264.2731,265.1795,265.2599,265.5416,265.9024,265.9629,266.1258,266.5203,267.0347,267.1696,267.5960,267.8415,268.1492,268.1670,268.8453,269.2714,269.8110,269.8153,270.0220,270.0673,270.1049,270.4709,270.9545,271.4325,272.4160] :: [Double]

-- Full sample
s1 :: [Double]
s1 = [0.1000,0.5710,0.5800,0.6120,0.6140,0.6160,0.6320,0.6390,0.6540,0.6560,0.6700,0.6730,0.6900,0.6960,0.7040,0.7510,0.8140,0.8610,0.8710,2.2254,2.3018,2.4929,5.1370,5.2670,5.9318,6.0541,6.2834,6.2986,6.2986,6.3980,6.5585,6.6425,6.6578,6.6655,6.6960,6.7113,6.8107,6.9482,7.1087,7.4450,7.4832,7.8500,7.8729,8.6524,9.0880,10.1121,10.2191,10.3643,10.4560,10.7311,10.9145,11.1743,11.3654,11.3654,11.5182,11.6252,11.7398,11.7398,12.0455,12.0837,12.2595,12.2671,12.5040,12.5881,12.6416,13.1154,13.1536,13.2071,13.2148,13.3982,13.5128,13.5178,13.6122,13.6568,13.6568,13.6628,13.6658,13.6678,13.6688,13.6698,13.6758,13.6768,13.6788,13.6798,13.6798,13.6838,13.6878,13.6928,13.6928,13.6948,13.6968,13.6998,13.7008,13.7008,13.7008,13.7018,13.7038,13.7078,13.7098,13.7098,13.7098,13.7098,13.7108,13.7118,13.7118,13.7118,13.7128,13.7128,13.7158,13.7158,13.7168,13.7168,13.7178,13.7178,13.7178,13.7178,13.7198,13.7198,13.7198,13.7208,13.7208,13.7208,13.7218,13.7248,13.7278,13.8643,13.8668,13.8688,13.8908,13.9118,13.9128,13.9331,13.9508,13.9638,13.9928,14.0038,14.0148,14.0298,14.0348,14.0458,14.0828,14.0878,14.0898,14.1188,14.1278,14.1398,14.1708,14.2118,14.2128,14.2228,14.2228,14.2588,14.2617,14.2688,14.4986,14.5078,14.5674,14.7203,15.6679,15.7367,15.7978,15.8971,16.5238,16.9288,17.3568,18.5413,19.3743,19.5883,20.3601,21.1549,21.7816,22.3089,26.3516] :: [Double]
s2 :: [Double]
s2 = map (*1000) [0.9241,0.9401,0.9438,0.9630,0.9855,0.9888,0.9944,1.0014,1.0015,1.0018,1.0029,1.0033,1.0040,1.0048,1.0051,1.0053,1.0054,1.0063,1.0063,1.0063,1.0063,1.0065,1.0081,1.0082,1.0082,1.0083,1.0083,1.0084,1.0085,1.0086,1.0087,1.0087,1.0089,1.0089,1.0089,1.0090,1.0092,1.0093,1.0101,1.0101,1.0101,1.0101,1.0103,1.0103,1.0103,1.0109,1.0111,1.0113,1.0118,1.0119,1.0119,1.0120,1.0137,1.0138,1.0141,1.0141,1.0142,1.0142,1.0143,1.0144,1.0159,1.0159,1.0160,1.0163,1.0174,1.0174,1.0174,1.0174,1.0175,1.0175,1.0176,1.0177,1.0177,1.0178,1.0182,1.0209,1.0210,1.0213,1.0215,1.0217,1.0218,1.0223,1.0226,1.0227,1.0230,1.0231,1.0234,1.0246,1.0247,1.0257,1.0259,1.0262,1.0275,1.0290,1.0304,1.0305,1.0305,1.0314,1.0316,1.0319,1.0324,1.0326,1.0328,1.0330,1.0330,1.0331,1.0333,1.0338,1.0338,1.0341,1.0342,1.0346,1.0349,1.0360,1.0363,1.0364,1.0374,1.0376,1.0376,1.0378,1.0382,1.0408,1.0408,1.0408,1.0408,1.0409,1.0410,1.0410,1.0410,1.0411,1.0433,1.0436,1.0442,1.0447,1.0456,1.0468,1.0477,1.0483,1.0492,1.0494,1.0494,1.0517,1.0519,1.0556,1.0558,1.0560,1.0561,1.0569,1.0570,1.0572,1.0577,1.0675,1.0706,1.0712,1.0719,1.0731,1.0736,1.0759,1.0764,1.0787,1.0788,1.0789,1.0791,1.0792,1.0792,1.0803,1.0834,1.0865,1.0882,1.0892,1.0899,1.1219,1.2211] :: [Double]
s3 :: [Double]
s3 = [245.7344,245.7808,247.8853,248.0465,248.5180,248.5349,248.9363,249.3447,249.5604,249.7914,249.8377,249.8525,249.8637,249.9452,250.1908,250.2594,250.2764,251.3724,251.4708,252.2073,252.4394,252.7775,253.3083,254.0832,255.0335,255.2750,255.5566,255.5596,255.8520,256.2759,256.4406,256.4481,257.1908,257.3649,257.3806,257.6179,257.6216,257.6309,257.6995,257.7032,257.9386,258.5567,258.7418,258.8195,258.9785,259.2733,259.4675,259.5941,259.5995,259.7070,259.7870,259.7998,259.8898,259.8926,259.9056,259.9286,260.0314,260.1259,260.1750,260.2467,260.3143,260.3662,260.5931,260.6213,260.6585,260.7293,260.7701,260.8245,261.1618,261.4192,261.5494,261.9480,262.0637,262.1424,262.3170,262.5617,262.5654,262.5840,262.6915,262.7498,262.8379,262.8542,262.9185,262.9705,263.2458,263.2897,263.5470,263.5875,263.6182,263.6377,263.6386,263.7537,263.8222,263.8494,263.9037,263.9441,263.9504,263.9547,264.1234,264.2605,264.2731,264.2940,264.6313,265.3660,265.5303,265.5479,265.8836,265.8909,265.9743,266.1406,266.1673,266.1747,266.1849,266.1922,266.2143,266.5705,266.8758,267.4985,267.5067,267.5891,268.0654,268.3141,268.5851,268.8429,268.8438,268.9391,269.0973,269.1219,269.2536,269.2619,269.2628,269.2714,269.3508,269.3659,269.4992,269.5770,269.6650,269.7419,269.8125,269.9073,270.0315,270.0560,270.3010,270.3335,270.5550,271.0360,271.4325,271.6512,271.8004,271.8189,272.1110,272.1258,272.1286,272.1314,272.1535,272.3213,272.5461,272.9351,273.0249,274.8546,274.8611,274.8639,275.2297,275.5627,275.7085,276.3215,281.1293,281.1562,281.4546,281.4824,281.5001,287.3771,298.6552] :: [Double]

t1 :: Task
t1 = AtomicTask {sample= listArray (1,length s1) s1}
t2 :: Task
t2 = AtomicTask {sample= listArray (1,length s2) s2}
t3 :: Task
t3 = AtomicTask {sample= listArray (1,length s3) s3}
--t4 = AtomicTask {sample=tSample}

x1 :: WorkflowStage
x1 = wrap t1 []
x2 :: WorkflowStage
x2 = wrap t2 [1]
x3 :: WorkflowStage
x3 = wrap t3 [2]
--x4 = wrap t4 [2,3]

w :: Task
w = Workflow {leafs=[3], nodes=Map.fromList [(1,x1),(2,x2),(3,x3)], wftype=Complex}

wx :: WorkflowStage
wx = wrap w []

--(bnds,wf) = calculateCDF wx 3.0 5
--ppp = do
--    let st = flip (Map.!) 1 . nodes . task $ wf
--    print bnds
--    print "t:"
--    mapM_ print . snd $ lsp
--    print "real waves"
--    mapM_ print $ waves st
--    print "distToChar"
--    mapM_ print $ distToChar lsp . quantiles $ st where
--        lsp = linSpace 5 bnds

