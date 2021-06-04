--1 Definicja funkcji: currying, partially applied functions
myFun x = 2 * x

add2T :: Num a => (a, a) -> a
add2T (x,y) = x + y

add2C :: (Num a) => (a -> a -> a)
add2C x y = x + y

add3T :: Num a => (a, a, a) -> a
add3T (x, y, z) = x + y + z

add3C :: (Num a) => (a-> a-> a-> a)
add3C x y z = x + y + z 

curry2 :: ((a, b) -> c) -> a -> b -> c
curry2 f = g
    where g x y = f (x, y)

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 f = g
    where g (x, y) = f x y

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f = g
    where g x y z = f (x, y, z)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f =g
    where g (x, y, z) = f x y z

--2 Definicja funkcji: sections
fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5 ^)

_ToPower5 :: Num a => a -> a
_ToPower5 = (^ 5)

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5 -)

subtr5From_ :: Num a => a -> a
subtr5From_ = (subtract 5)

flip2 :: (a->b->c) -> (b -> a -> c)
flip2 f = g
    where g x y = f y x

flip3 :: (a->b->c->d) -> (c->b->a->d)
flip3 f = g
    where g x y z = f z y x

--4 Elementarne operacje na listach
isPalindrome :: [Char] -> Bool
isPalindrome s = if reverse s == s then True
                else False

getElemAtIdx :: [Char] -> Int -> Char
getElemAtIdx string id = last (take (id + 1) string)

capitalize :: [Char] -> [Char]
capitalize w = (f (head w)) : (tail w) -- capitalize "ala" = "Ala"
    where f x = toEnum(fromEnum(x) - 32)

--5 List comprehensions
makesTriangle :: Int
makesTriangle = length [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2+b^2 >= c^2]

isPrime :: Integral t => t -> Bool
isPrime n = if n `mod` 2 == 0 then False
    else [i | i <- [3, 5..n-1], n `mod` i == 0] == []

countPrimes :: Int
countPrimes = length [a | a <- [1, 3..10000], isPrime a == True] + 1 -- +1 bo jeszcze liczba 2

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime' :: Int -> Bool
isPrime' x = if x == 1 || any ( == x) xs then True
    else False
    where xs = take x primes

countPrimes' :: Int -> Int
countPrimes' x = length [a | a <- [1, 3..x], isPrime' a==True] + 1

allEqual :: Eq a => [a] -> Bool
allEqual xs = if all ( == head xs) xs then True-- allEqual [1,1] = True, allEqual [1,2] = False
    else False

--6 Rekursja 1
fib :: (Num a, Eq a) => a -> a
fib n =
 if n == 0 || n == 1 then n
 else fib (n - 2) + fib (n - 1)

 --fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

sum' :: Num a => [a] -> a
sum' []   = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a -- prod' [1,2,3] = 6
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int -- length' [1,1,1,1] = 4
length' [] = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool -- or' [True, False, True] = True
or' [] = False
or' (x:xs)
    | x == True = True
    | or' xs == True = True
    | otherwise = False

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
    | x == True = and' xs
    |otherwise = False

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs)
    | n == x = True
    | otherwise = elem' n xs

doubleAll :: Num t => [t] -> [t] -- doubleAll [1,2] = [2,4]
doubleAll [] = []
doubleAll (x:xs) = (2*x) : (doubleAll xs)

squareAll :: Num t => [t] -> [t] -- squareAll [2,3] = [4,9]
squareAll [] = []
squareAll (x:xs) = (x^2) : (squareAll xs)

selectEven :: Integral t => [t] -> [t] -- selectEven [1,2,3] = [2]
selectEven [] = []
selectEven (x:xs)
    | x `mod` 2 == 0 = x : (selectEven xs)
    | otherwise = selectEven xs

--mean' :: Floating a => [a] -> a
mean' xs = (sum' xs) / (length' xs)

--7 Rekursja 2: użycie akumulatora, rekursja końcowa
{-# LANGUAGE BangPatterns #-}

sum'2 :: Num a => [a] -> a -- wywołanie sum'2 [1..10000000] - 6.23 secs
sum'2 xs = loop 0 xs
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'3 :: Num a => [a] -> a -- wywołanie sum'3 [1..10000000] - 7.16 secs
sum'3 = loop 0
 where loop acc []     = acc
       loop acc (x:xs) = loop (x + acc) xs

sum'4 :: Num a => [a] -> a -- wywołanie sum'4 [1..10000000] - 4.33 secs
sum'4 = loop 0
   where loop !acc []     = acc
         loop !acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a=> [a] -> a
prod'2 xs = loop 1 xs
 where loop acc [] = acc
       loop acc (x:xs) = loop (x * acc) xs 

prod'3 :: Num a=> [a] -> a
prod'3 = loop 1
 where loop acc [] = acc
       loop acc (x:xs) = loop (x * acc) xs 

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
    where loop acc [] = acc
          loop acc (x:xs) = loop (1 + acc) xs

length'3 :: [a] -> Int
length'3 = loop 0
    where loop acc [] = acc
          loop acc (x:xs) = loop (1 + acc) xs

--9 Rekursja 4
qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = [ y | y <- xs, y <= x ]
   rightPart xs = [ y | y <- xs, y > x  ]

qSort' :: Ord a => [a] -> [a]
qSort' []     = []
qSort' (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart xs = filter (<=x) xs
   rightPart xs = filter (>x) xs

mSort :: Ord a => [a] -> [a] -> [a]
mSort xs [] = xs
mSort [] ys = ys
mSort (x:xs) (y:ys)
    | x <= y = x : (mSort xs (y:ys))
    | otherwise = y : (mSort (x:xs) ys)

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs)
    | n < x = n:x:xs
    | otherwise = x : (insert n xs)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)

--10 Dopasowanie wzorców: guards
fst2Eq :: Eq a => [a] -> Bool
fst2Eq (x : y : _) | x == y = True
fst2Eq _                    = False

fst2Mod :: Integral a => [a] -> Bool
fst2Mod (x : y : _) | y `mod` x == 0 = True
                    | otherwise = False

fst3Mod :: Integral a => [a] -> Bool
fst3Mod (x : y : z: _) | z `mod` x == 0 = True
                       | otherwise = False