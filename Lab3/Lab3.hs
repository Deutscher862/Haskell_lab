--1 Funkcje anonimowe i currying

f1 :: Num a => a -> a
f1 = \x -> x-2

f2 :: (Double, Double) -> Double
f2 = \(x,y) -> sqrt(x^2 + y^2)

f3 :: (Double, Double, Double) -> Double
f3 = \(x,y, z) -> sqrt(x^2 + y^2 + z^2)

f4 :: Num a => a -> a
f4 = \x -> 2*x

f5 :: Num a => a -> a
f5 = \x -> x*2

f6 :: Integer -> Integer
f6 = \x -> 2^x

f7' :: Integer -> Integer
f7' = \x -> x^2

f8' :: Fractional a => a -> a
f8' = \x -> 2/x

f9' :: Fractional a => a -> a
f9' = \x -> x/3

f10 :: Num a => a -> a
f10= \x -> 4-x

f7 :: Int -> Bool
f7 = \x -> if x `mod` 2 == 0 then True else False

f8 :: Double -> Double
f8 = \x -> let y = sqrt x in 2 * y^3 * (y + 1)

f9 :: Int -> Int
f9 = \x -> if x == 1 then 3 else 0

--2 Funkcje wyższego rzędu: funkcje jako parametry/argumenty

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' []     = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum'' :: Num a => [a] -> a
sum'' xs = sumWith (1*) xs

sumSqr'' :: Num a => [a] -> a
sumSqr'' xs  = sumWith (^2) xs

sumCube :: Num a => [a] -> a
sumCube xs  = sumWith (^3) xs

sumAbs :: Num a => [a] -> a
sumAbs xs  = sumWith (abs) xs

listLength :: Integral a => [a] -> a
listLength xs = sumWith (\x -> x `div` x) xs

--3 Funkcje wyższego rzędu: funkcje jako wyniki

sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

fact :: Double -> Double
fact 0 = 1
fact n = n * fact (n - 1)

expApproxUpTo2 :: Int -> Double -> Double
expApproxUpTo2 0 = \x -> 1 + x
expApproxUpTo2 n = \x -> (x^(n+1))/(silnia (nToDouble + 1)) + (expApproxUpTo2 (n-1)) x
 where nToDouble = fromIntegral n

--4 Funkcje jako elementy struktur danych

funcList :: [ Double -> Double ]
funcList = [ \x -> (sin x)/x, \x -> log x + sqrt x + 1, \x -> (exp 1) ** x ]

evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x [] = []
evalFuncListAt x (f:fs) = f x : evalFuncListAt x fs

displEqs :: (Double -> Double, Double -> Double)
displEqs = (\t -> 4 * t^2 + 2 * t, \t -> 3 * t^2)

--5 Operator złożenia funkcji (.) (i notacja point-free)

import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort) xs

sortDesc' :: Ord a => [a] -> [a]
sortDesc' = reverse . sort

--6 Operator “aplikacji” funkcji ($)

((,) $ (1 $ 2))

--7 Funkcje wyższego rzędu: filter

onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 == 1 = x : onlyOdd xs
 | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x :xs)
 | fromEnum(x) >= 65 && fromEnum(x) <= 90 = x : onlyUpper xs
 | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]  
filter' _ [] = []  
filter' p (x:xs)   
 | p x == True = x : filter p xs  
 | otherwise = filter p xs  

onlyEven' = filter' even

onlyOdd' = filter' odd

onlyUpper' = filter' (`elem` ['A'..'Z'])

--length (onlyEven [1..10^6]) - 1.07 secs
--length $ onlyEven [1..10^6]

--length (filter even [1..10^6]) - 0.22 secs

--length [a | a <- [1..10^6], a `mod` 2 == 0] - 0.70 secs

--8  Funkcje wyższego rzędu: map

import Data.Char

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems [] = []
sqrElems (x:xs) = sqrt(x) : sqrElems xs

lowerCase "" = ""
lowerCase (x:xs) = toLower(x) : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems' :: Num a => [a] -> [a]
doubleElems' xs = map' (*2) xs

sqrElems' :: Floating a => [a] -> [a]
sqrElems' xs = map' (sqrt) xs

lowerCase' :: [Char] -> [Char]
lowerCase' xs = map' toLower xs

doubleElems'' :: Num a => [a] -> [a]
doubleElems'' xs = [a*2| a <- xs]

sqrElems'' :: Floating a => [a] -> [a]
sqrElems'' xs = [sqrt a| a <- xs]

lowerCase'' :: [Char] -> [Char]
lowerCase'' xs = [toLower a| a <- xs]

-- length . filter even $ doubleElems [1..10^7] - 7.92secs
-- length . filter even . map (*2) $ [1..10^7] - 2.45secs

--9 Funkcje wyższego rzędu: foldr i foldl 

sumWith g []     = 0
sumWith g (x:xs) = g x + sumWith g xs -- (+) (g x) (sumWith g xs)

prodWith g []     = 1
prodWith g (x:xs) = g x * prodWith g xs -- (*) (g x) (prodWith g xs)

sumWith' :: Num a => (a -> a) -> [a] -> a
sumWith' = go 0
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x + acc) g xs

prodWith' :: Num a => (a -> a) -> [a] -> a
prodWith' = go 1
 where
   go acc g [] = acc
   go acc g (x:xs) = go (g x * acc) g xs

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f z []     = z
foldr' f z (x:xs) = f x (foldr f z xs)

sumWith'' g  = foldr' (\x acc -> g x + acc) 0

prodWith'' g  = foldr' (\x acc -> g x * acc) 1


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z []     = z
foldl' f z (x:xs) = foldl f (f z x) xs

sumWith''' g  = foldl' (\acc x -> g x + acc) 0

prodWith''' g  = foldl' (\acc x -> g x * acc) 1

--10 Funkcje: zip, unzip i zipWith

import Data.List

isSortedAsc :: Ord a => [a] -> Bool
isSortedAsc xs = and $ zipWith (==) xs ys
 where ys = sort xs

everySecond :: [t] -> [t]
everySecond xs = map fst $ filter (even . snd) $ zip xs [1..]

--12 Wzorzec Collection pipeline

import Data.Char
import Data.List

capitalize :: [Char] -> [Char]
capitalize [] = []
capitalize (x:xs) = toUpper x : (map toLower xs)

formatStr s = foldr1 (\w s -> w ++ " " ++ s) .
          map capitalize .
          filter (\x -> length x > 1) $
          words s

prodPrices p = case p of
 "A" -> 100
 "B" -> 500
 "C" -> 1000
 _   -> error "Unknown product"

products = ["A","B","C"]

-- basic discount strategy
discStr1 p
 | price > 999 = 0.3 * price
 | otherwise   = 0.1 * price
 where price = prodPrices p

-- flat discount strategy
discStr2 p = 0.2 * prodPrices p

totalDiscout discStr =
 foldl1 (+) .
 map discStr .
 filter (\p -> prodPrices p > 499)