-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment (getArgs)

import Workflow
import Workflow.Toolset


main :: IO ()
main = do
    numPPs:qSigs:inputname:outputname:_ <- findArgs
    let numPP = read numPPs :: Int
        qSig  = read qSigs  :: Double
    wfFile <- B.readFile inputname
    workflow <- case Aeson.eitherDecode' wfFile of
        Left err -> fail err
        Right val -> return val
    let workflowSimplified = workflow { task = applyRS . applyDE $ task workflow}
        workflowComputed = calculateCDF workflow qSig numPP
        -- force using the same grid on simplified workflow
        grid = linSpace numPP . fst $ workflowComputed
        workflowSimpleComputed = calculateCDF' workflowSimplified grid
        -- get time coordinates
        coordsWF = fst grid
        -- compute estimates CDFs
        quWF = quantiles . snd $ workflowComputed
        quWFSimple = quantiles . snd $ workflowSimpleComputed
    putStrLn "Computed workflow"
    print workflowComputed
    putStrLn "Simplified and computed workflow"
    print workflowSimpleComputed
    writeCSV outputname . map (\(x,y,z) -> [x,y,z]) $ zip3 coordsWF quWF quWFSimple
  where
    findArgs = getArgs >>= \args -> case args of
       numPPs:qSigs:inputname:outputname:_ -> return $ numPPs:qSigs:inputname:outputname:[]
       _ -> error "\nProgram usage:\n  haskell-estimatewf {numPP::Int} {qSig::Double} {input.json::String} {output.csv::String}\
                  \\nProgram returns a csv file with three columns:\n  time,CDFvalueForFullWF,CDFvalueForSimplifiedWF\
                  \\n\nExample:\
                  \\n  haskell-estimatewf 6 3.0 testwf.json cdfs.csv\n\n"


writeCSV :: (Show a) => FilePath -> [[a]] -> IO()
writeCSV path [[]] = B.writeFile path B.empty
writeCSV path [] = B.writeFile path B.empty
writeCSV path (start:rest) = B.writeFile path $
    B.concat $ (:) (BC.pack . createline $ start) (map (BC.pack . (:) '\n' . createline) rest)
  where
    createline (x:xs) = show x ++ (concat . map ((:) ',' . show) $ xs)
    createline []     = ""
