module Main where

import Lib
import   Data.List
import   Test.QuickCheck


factori :: Int -> [Int]
factori n = [x | x <- [1..n], rem n x == 0]
    

prim :: Int -> Bool
prim n
 | factori n == [1, n] = True
 | otherwise = False


numerePrime :: Int -> [Int]
numerePrime n = [x | x <- [2..n], prim x]


numerePrimeCiur :: Int -> [Int]
numerePrimeCiur n = ciur [2..n]
                    where 
                    ciur (h:t) = h : ciur (t \\ [h,h+h..n])
                    ciur [] = []


prop_prime :: Int -> Bool
prop_prime n = numerePrime n == numerePrimeCiur n


myzip3 :: [a]->[b]->[c]->[(a, b, c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (x:xt)(y:yt)(z:zt) = (x, y, z): myzip3 xt yt zt


ordonataNatComp :: [Int] -> Bool
ordonataNatComp [] = True
ordonataNatComp [x] = True
ordonataNatComp (h:t) = and [x <= y | (x, y) <- zip (h:t) t]


ordonataNatRec :: [Int] -> Bool
ordonataNatRec [] = True
ordonataNatRec [x] = True
ordonataNatRec (x:y:t)
 | x <= y = ordonataNatRec (y:t)
 |otherwise = False


prop_ordonataNat :: [Int] -> Bool
prop_ordonataNat l = ordonataNatComp l == ordonataNatRec l


double :: Int -> Int
double a = a + a
aplica2 :: (a -> a) -> a -> a
aplica2 f = f . f


maiMare :: Int -> Int -> Bool
maiMare x y
 | x > y = True
 | otherwise = False
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata [] _ = True
ordonata [x] _ = True
ordonata (h:t) f = and [f x y | (x, y) <- zip (h:t) t] 


(*<*) :: (Integer , Integer) -> (Integer , Integer) -> Bool
(*<*) (a, b) (c, d)
 | a*b < c*d = True
 | otherwise = False


firstEl :: [(a, b)] -> [a]
firstEl l = map fst l


sumList :: [[Integer]] -> [Integer]
sumList l = map (\h -> sum h) l


prel2 :: [Integer] -> [Integer]
prel2 l = map (\x -> if even x then x `div` 2 else x*2) l 


compuneList :: (b -> c) -> [(a -> b)] -> [(a -> c)]
compuneList g fl = map (\f -> g . f) fl


aplicaList :: a -> [( a -> b)] -> [b]
aplicaList x fl = map (\f -> f x) fl


ex1 :: Char -> [String] -> [String]
ex1 c l = filter (c `elem`) l
 

ex2 :: [Int] -> [Int]
ex2 l = map (^2) (filter odd l)


ex3 :: [Int] -> [Int]
ex3 l = map (\x -> (fst x) ^2) (filter (odd . snd) (zip l [0..]))


ex4 :: [String] -> [String]
ex4 l = map (\s -> filter (`elem` "aeiouAEIOU") s) l


mymap :: (a -> b) -> [a] -> [b]
mymap f l = [f x | x <- l]


myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f l = [x | x <- l, f x == True]


myEx4 :: [String] -> [String]
myEx4 l = mymap (\string -> myfilter (`elem` "aeiouAEIOU") string) l

prop_ex4 :: [String] -> Bool
prop_ex4 l = ex4 l == myEx4 l


main :: IO ()
main = someFunc
