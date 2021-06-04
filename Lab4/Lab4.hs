--1 Typy w Haskellu: type vs. newtype
polarToCartesian :: Floating a => (a,a) -> (a,a)
polarToCartesian (r,phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a,a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r,phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

--2  Algebraiczne typy danych 1: product & sum types, record syntax
data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

type X = Int
type Y = Int

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

xCoord'' :: Cart2DVec'' a -> a
xCoord'' (MkCart2DVec'' {x = xVal, y = _}) = xVal

yCoord'' :: Cart2DVec'' a -> a
yCoord'' (MkCart2DVec'' {y = yVal, x = _}) = yVal

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

data ThreeColors = Blue |
                   White |
                   Red'

type ActorName = String

leadingActor :: ThreeColors -> ActorName
leadingActor Blue  = "Juliette Binoche"
leadingActor White = "Zbigniew Zamachowski"
leadingActor Red'   = "Irene Jacob"


data Cart3DVec a = MkCart3DVec a a a

x3DCoord :: Cart3DVec a -> a
x3DCoord (MkCart3DVec x _ _) = x

y3DCoord :: Cart3DVec a -> a
y3DCoord (MkCart3DVec _ y _) = y

z3DCoord :: Cart3DVec a -> a
z3DCoord (MkCart3DVec _ _ z) = z

data Cart3DVec' = Cart3DVec' {x3DCoord' :: Float,
                              y3DCoord' :: Float,
                              z3DCoord' :: Float
                             } deriving(Show)

area :: Shape -> Float

area (Circle r) = pi*r^2
area $ Rectangle a b = a*b

data Shape = Circle Float |
             Rectangle Float Float

actionFor :: TrafficLights -> String
actionFor Red = "Stoj"
actionFor RedAndYellow = "Przygotuj sie"
actionFor Yellow = "Hamuj"
actionFor Green = "Jedz"

data TrafficLights = Red| RedAndYellow | Yellow | Green

--3 Algebraiczne typy danych 2: rekursja strukturalna

data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)
                 deriving(Show)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | Add (Expr a) (Expr a) | 
                Diff (Expr a) (Expr a) | Prod (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Diff e1 e2) = eval e1 - eval e2
eval (Prod e1 e2) = eval e1 * eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"
show' (Diff e1 e2) = "(" ++ show' e1 ++ "-" ++ show' e2 ++ ")"
show' (Prod e1 e2) = "(" ++ show' e1 ++ "*" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

preorderFlattenBT :: BinTree a -> [a]
preorderFlattenBT EmptyBT = []
preorderFlattenBT (NodeBT n lt rt) = n : (preorderFlattenBT lt) ++ (preorderFlattenBT rt)

inorderFlattenBT :: BinTree a -> [a]
inorderFlattenBT EmptyBT = []
inorderFlattenBT (NodeBT n lt rt) = inorderFlattenBT lt ++ n : inorderFlattenBT rt

postorderFlattenBT :: BinTree a -> [a]
postorderFlattenBT EmptyBT = []
postorderFlattenBT (NodeBT n lt rt) = (preorderFlattenBT lt ++ preorderFlattenBT rt) ++ [n]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT= EmptyBT
mapBT f (NodeBT n lt rt) =NodeBT (f n) (mapBT f lt) ( mapBT f rt)

insert :: Ord a => a -> BinTree a -> BinTree a
insert a EmptyBT = NodeBT a EmptyBT EmptyBT
insert a (NodeBT n lt rt) = if a < n then NodeBT n (insert a lt) rt else NodeBT n lt (insert a rt)

list2BST :: Ord a => [a] -> BinTree a
list2BST [] = EmptyBT
list2BST (x:xs) = createTree (NodeBT x EmptyBT EmptyBT) xs
                where {createTree tree [] = tree; createTree tree (x:xs) = createTree (insert x tree) xs}

--5  Klasy typów i ich instancje 1: dołączanie typu do istniejącej klasy
newtype MyInt = MkMyInt Int

instance Eq MyInt where
  (==) (MkMyInt i1) (MkMyInt i2) = i1 == i2

instance Ord MyInt where
  (<=) (MkMyInt i1) (MkMyInt i2) = i1 <= i2

instance Num MyInt where
  (+) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 + i2)
  (-) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 - i2)
  (*) (MkMyInt i1) (MkMyInt i2) = MkMyInt (i1 * i2)
  negate (MkMyInt i)            = MkMyInt (negate i)
  abs (MkMyInt i)               = MkMyInt (abs i)
  signum (MkMyInt i)            = MkMyInt (signum i)
  fromInteger int               = MkMyInt (fromIntegral int)

instance Show MyInt where
  show (MkMyInt i) = "MkMyInt " ++ show i

instance Eq a => Eq (BinTree a) where
  EmptyBT == EmptyBT = True
  EmptyBT == NodeBT _ _ _ = False
  NodeBT n1 lt1 rt1 == NodeBT n2 lt2 EmptyBT = False
  NodeBT n1 lt1 rt1 ==  NodeBT n2 lt2 rt2 = n1 == n2 && lt1 == lt2 && rt1 == rt2