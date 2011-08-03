module EnigmaRotor where

import Data.List (sort)

data Rotor = Rotor Position Tick Configuration deriving Show

type Position = Int
type Tick = Int
type Configuration = [Int]

validConf :: Rotor -> Rotor -> Bool
validConf (Rotor _ _ xs) (Rotor _ _ ys) = sameSymbols (sort xs) (sort ys)
  where sameSymbols :: Configuration -> Configuration -> Bool
        sameSymbols (x:xs) (y:ys)
          | x == y = sameSymbols xs ys
          | otherwise = False
        sameSymbols [] [] = True
        sameSymbols _ _ = False

once :: Rotor -> Bool
once (Rotor pos tick config) = pos < conLength && tick < conLength && checkConfig config
  where conLength = length config
        checkConfig :: Configuration -> Bool
        checkConfig [] = True
        checkConfig (x:xs)
          | x `elem` xs = False
          | otherwise = checkConfig xs

encrypt :: Int -> Rotor -> Int
encrypt sym (Rotor pos tick config)
  | config == [] = error "Invalid Rotor"
  | otherwise = ((config!! ((pos  + sym) `mod` (length config))) - pos + 26) `mod` 26

incrPos :: Rotor -> Rotor
incrPos (Rotor pos tick config) = Rotor (pos + 1) tick config

decrypt :: Int -> Rotor -> Int
decrypt sym (Rotor pos tick (config))
  | config == [] = error "Invalid Rotor"
  | otherwise = (pos + ind sym config) `mod` 26
      where ind a (x:xs) = 0 + (if a == x then 0 else 1 + ind a xs)
            ind a [] = 0
