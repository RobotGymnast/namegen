{-# LANGUAGE FlexibleContexts #-}
module Main( main
           ) where

import Prelude ()
import BasicPrelude as Base

import Data.Char (toUpper)
import Data.Ratio
import Data.Traversable

-- extensible-effects
import Control.Eff
import Control.Eff.Lift as Eff
import Control.Eff.State.Strict

-- system-random-effect
import System.Random.Effect

randomElem :: Member (State Random) r => [a] -> Eff r a
randomElem l = let len = fromIntegral $ length l
               in (l !!) <$> uniformIntegralDist 0 (len - 1)

main :: IO ()
main = runLift
     $ mkRandomIO `forRandEff` (genName >>= Eff.lift . putStrLn . fromString)

genName :: Member (State Random) r => Eff r String
genName = do
      startVowel <- bernoulliDist (2 % 5)
      len <- sum <$> traverse randomElem [[1..4], [1..3]]
      parts <- for (take len $ cycle [startVowel, not startVowel])
                   (\isVowel -> randomElem $ if isVowel then vowels else consonants)
      return $ capitalize $ concat parts
  where
    vowels = ["a", "e", "i", "o", "u"]
    consonants = filter (not . (`elem` vowels)) $ (:"") <$> ['a'..'z']

    capitalize (c:cs) = toUpper c : cs
    capitalize "" = ""
