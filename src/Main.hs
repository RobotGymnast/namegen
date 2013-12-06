{-# LANGUAGE FlexibleContexts #-}
module Main( main
           ) where

import Prelude ()
import Prelewd hiding (length, sum, concat)

import Data.Char (toUpper)
import Data.List
import Data.Ratio
import System.IO

-- extensible-effects
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict

-- system-random-effect
import System.Random.Effect

randomElem :: Member (State Random) r => [a] -> Eff r a
randomElem l = let len = fromIntegral $ length l
               in (l !) <$> uniformIntDist 0 (len - 1)

main :: IO ()
main = runLift
     $ mkRandomIO `forRandEff` (genName >>= lift . putStrLn)

genName :: Member (State Random) r => Eff r String
genName = do
      startVowel <- bernoulliDist (2 % 5)
      len <- sum <$> traverse randomElem [[1..4], [1..3]]
      parts <- for (take len $ cycle [startVowel, not startVowel])
                   (\isVowel -> randomElem $ iff isVowel vowels consonants)
      return $ capitalize $ concat parts
  where
    vowels = ["a", "e", "i", "o", "u"]
    consonants = filter (not . (`elem` vowels)) $ (:"") <$> ['a'..'z']

    capitalize (c:cs) = toUpper c : cs
    capitalize "" = ""
