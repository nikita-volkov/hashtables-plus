module Benchmarks.Random where

import Benchmarks.Prelude.Basic
import Benchmarks.Prelude.Strings
import qualified System.Random.MWC as MWC
import qualified Data.Char as Char

type Gen = MWC.Gen RealWorld

newGen :: IO Gen
newGen = MWC.create

generateName :: Gen -> IO StrictText
generateName gen = do
  length <- MWC.uniformR lengthRange gen
  chars <- replicateM length $ generateChar gen
  return $ cs chars
  where
    lengthRange = (1, 50)

generateChar :: Gen -> IO Char
generateChar gen = do
  scenario :: Int <- MWC.uniformR (0, 6) gen
  ord <- flip MWC.uniformR gen $ case scenario of
    0 -> upperRange
    1 -> upperRange
    2 -> lowerRange
    3 -> lowerRange
    4 -> lowerRange
    5 -> lowerRange
    6 -> numRange
  return $ Char.chr ord
  where
    upperRange = (Char.ord 'A', Char.ord 'Z')
    lowerRange = (Char.ord 'a', Char.ord 'z')
    numRange = (Char.ord '0', Char.ord '9')

generateVariate :: (MWC.Variate n) => Gen -> IO n
generateVariate gen = MWC.uniform gen

generateVariateR :: (MWC.Variate n) => Gen -> (n, n) -> IO n
generateVariateR gen r = MWC.uniformR r gen
