-- Adam Niemiec
-- Jest to plik w ktorym zebralem rozwiazania wszystkich
-- pojedynczych zadań ze skryptu w celu wygodniejszego sprawdzania.

-- ex4 proste funkcje jednej zmiennej

sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt(x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (num, char) = (char, num)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = (x == y) && (x == z)

triangleArea :: (Double, Double, Double) -> Double
triangleArea (a, b, c) = sqrt((a+b+c)*(a+b-c)*(a-b+c)*(-a+b+c)) / 4

--ex 5 wyrażenie if...then...else

sgn :: Int -> Int
sgn n = if n <0
	then -1
	else if n == 0
		then 0
		else 1

absInt :: Int -> Int
absInt n = if n >= 0
	then n
	else n * (-1)

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x <= y
	then x
	else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z)= min2Int(min2Int(x, y), z)

toUpper :: Char -> Char
toUpper a = toEnum(fromEnum(a) - 32)

toLower :: Char -> Char
toLower a = toEnum(fromEnum(a) + 32)

isDigit :: Char -> Bool
isDigit x = if (fromEnum(x) >= 48 && fromEnum(x) <= 57)
	then True
	else False

charToNum :: Char -> Int
charToNum x = if isDigit(x) == True
	then fromEnum(x) - 48
	else -1
	
romanDigit :: Char -> String
romanDigit x = if x == '1'
	then "I"
	else if x == '2'
			then "II"
	else if x == '3'
			then "III"
	else if x == '4'
			then "IV"
	else if x == '5'
			then "V"
	else if x == '6'
			then "VI"
	else if x == '7'
			then "VII"
	else if x == '8'
			then "VIII"
	else if x == '9'
			then "IX"
	else "Null"

--ex6 guards

absInt :: Int -> Int
absInt n | n >= 0 = n
	| otherwise = -n

sgn :: Int -> Int
sgn n
	| n < 0 = -1
	| n == 0 = 0
	| otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z)
	|x <= y && x <= z = x
	|y <= z && y <=z = y
	|otherwise = z

toUpper :: Char -> Char
toUpper a
	|a <= 'z' && a >= 'a' = toEnum(fromEnum(a) - 32)
	|otherwise = '0'

toLower :: Char -> Char
toLower a
	|a <= 'Z' && a >= 'A' = toEnum(fromEnum(a) + 32)
	|otherwise = '0'

isDigit :: Char -> Bool
isDigit x
	|fromEnum(x) >= 48 && fromEnum(x) <= 57 = True
	|otherwise = False

charToNum :: Char -> Int
charToNum x
	|isDigit(x) == True = fromEnum(x) - 48
	|otherwise = -1
	
romanDigit :: Char -> String
romanDigit x
	|x == '1' = "I"
	|x == '2' = "II"
	|x == '3' = "III"
	|x == '4' = "IV"
	|x == '5' = "V"
	|x == '6' = "VI"
	|x == '7' = "VII"
	|x == '8' = "VIII"
	|x == '9' = "IX"
	|otherwise = "Null"

-- ex7 dopasowanie wzorców

not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _      = False


or' :: (Bool, Bool) -> Bool
or' (x, y) = if x == False && y == False
    then False
    else True

and' :: (Bool, Bool) -> Bool
and' (x, y) = if x == True && y == True
    then True
    else False

nand' :: (Bool, Bool) -> Bool
nand' (x, y) = if x == True && y == True
    then False
    else True

xor' :: (Bool, Bool) -> Bool
xor' (x, y) = if x == y
    then False
    else True

-- ex8 wyrażenie case...of

not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True

absInt n = 
    case (n >= 0) of
        True -> n
        _ -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer word = case word of
    "Love" -> True
    _ -> False

or' :: (Bool, Bool) -> Bool
or' (x, y) = case (x, y) of
    (False, False) -> False
    _ -> True

and' :: (Bool, Bool) -> Bool
and' (x, y) = case (x, y) of
    (True, True) -> True
    _ -> False

nand' :: (Bool, Bool) -> Bool
nand' (x, y) = case (x, y) of
    (True, True) -> False
    _ -> True

xor' :: (Bool, Bool) -> Bool
xor' x, y = case (x, y) of
    (True, True) -> False
    (False, False) -> False
    _ -> True

-- ex9 klauzula where

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where d = sqrt (b * b - 4 * a * c)
         e = 2 * a

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = (x/len, y/len)
   where len = sqrt(x^2 + y^2)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = (x/len, y/len, z/len)
   where len = sqrt(x^2 + y^2+ z^2)

triangleArea :: (Double, Double, Double) -> (Double)
triangleArea (a, b, c) = sqrt(p*(p-a)*(p-b)*(p-c))
    where p = (a+b+c)/2

-- ex10 wyrażenie let...in

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) =
 let d = sqrt (b * b - 4 * a * c)
     e = 2 * a
 in ( (-b - d) / e, (-b + d) / e )

unitVec2D :: (Double, Double) -> (Double, Double)
unitVec2D (x, y) = 
 let len = sqrt(x^2 + y^2)
 in (x/len, y/len)

unitVec3D :: (Double, Double, Double) -> (Double, Double, Double)
unitVec3D (x, y, z) = 
 let len = sqrt(x^2 + y^2+ z^2)
 in (x/len, y/len, z/len)

triangleArea :: (Double, Double, Double) -> (Double)
triangleArea (a, b, c) =
 let p = (a+b+c)/2
 in sqrt(p*(p-a)*(p-b)*(p-c))

-- ex11 off-side rule

roots :: (Double, Double, Double) -> (Double, Double)
roots (a, b, c) = ( (-b - d) / e, (-b + d) / e )
   where{ 
         d = sqrt (b * b - 4 * a * c);
         e = 2 * a 
        }-- uwaga na przesunięcie!
      
roots2 :: (Double, Double, Double) -> (Double, Double)
roots2 (a, b, c) =
 let{
     d = sqrt (b * b - 4 * a * c);
     e = 2 * a
    }
 in ( (-b - d) / e, (-b + d) / e )