module Main where

import Lib
import Test.QuickCheck
import Data.List 


---------------------------------------------
-------RECURSIE: FIBONACCI-------------------
---------------------------------------------


fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
  | n < 2     = n
  | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)


fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)


{-| @fibonacciLiniar@ calculeaza @F(n)@, al @n@-lea element din secvența
Fibonacci în timp liniar, folosind funcția auxiliară @fibonacciPereche@ care,
dat fiind @n >= 1@ calculează perechea @(F(n-1), F(n))@, evitănd astfel dubla
recursie. Completați definiția funcției fibonacciPereche.

Indicație:  folosiți matching pe perechea calculată de apelul recursiv.
-}
fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)
  where
    fibonacciPereche :: Integer -> (Integer, Integer)
    fibonacciPereche 1 = (0, 1)
    fibonacciPereche n = let (a, b) = fibonacciPereche(n-1) in (b, a+b)


prop_fib :: Integer -> Property
prop_fib x = x >= 0 ==> fibonacciEcuational x == fibonacciLiniar x


---------------------------------------------
----------RECURSIE PE LISTE -----------------
---------------------------------------------


semiPareRecDestr :: [Int] -> [Int]
semiPareRecDestr l
  | null l    = l
  | even h    = h `div` 2 : t'
  | otherwise = t'
  where
    h = head l
    t = tail l
    t' = semiPareRecDestr t


semiPareRecEq :: [Int] -> [Int]
semiPareRecEq [] = []
semiPareRecEq (h:t)
  | even h    = h `div` 2 : t'
  | otherwise = t'
  where t' = semiPareRecEq t


inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (h:t) 
  | h >= a && h <= b = h:t'
  | otherwise = t'
  where t' = inIntervalRec a b t


pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
  | h>0 = 1 + t' 
  | otherwise = t'
  where t' = pozitiveRec t


contorPozitiiImpareRec :: [Int] -> Int -> [Int]
contorPozitiiImpareRec [] _ = []
contorPozitiiImpareRec (h:t) idx
  | odd h = idx:t'
  | otherwise = t'
  where t' = contorPozitiiImpareRec t (idx+1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec l = contorPozitiiImpareRec l 0


---------------------------------------------
----------DESCRIERI DE LISTE ----------------
---------------------------------------------


semiPareComp :: [Int] -> [Int]
semiPareComp l = [x `div` 2 | x <- l, even x]


inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b l = [x | x <- l, x >= a, x <= b]


pozitiveComp :: [Int] -> Int
pozitiveComp l = length [x | x <- l, x > 0]


pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp l = [b | (a, b) <- indexedList, odd a]
 where 
 indexedList = zip l [0..len]
 len = length l


---------------------------------------------
------------------TESTARI--------------------
---------------------------------------------


prop_pozitive :: [Int] -> Bool
prop_pozitive l = pozitiveRec l == pozitiveComp l


prop_inInterval :: Int -> Int -> [Int] -> Bool
prop_inInterval a b l = inIntervalRec a b l == inIntervalComp a b l


prop_semiPare :: [Int] -> Bool
prop_semiPare l = semiPareRecEq l == semiPareComp l


prop_pozitiiImpare :: [Int] -> Bool
prop_pozitiiImpare l = pozitiiImpareRec l == pozitiiImpareComp l


main :: IO ()
main = someFunc
