{-# LANGUAGE DeriveFunctor #-}

--2 Łączenie (sekwencje) ‘akcji’ I/O — operatory >> (then) i >>= (bind), notacja do
actSeq = putChar 'A' >> putChar 'G' >> putChar 'H' >>  putChar '\n'

doActSeq = do
  putChar 'A'
  putChar 'G'
  putChar 'H'
  putChar '\n'

echo1 = getLine >>= putStrLn

doEcho1 = do
  line <- getLine
  putStrLn line

echo2 = getLine >>= \line -> putStrLn $ line ++ "!"

doEcho2 = do
  line <- getLine
  putStrLn $ line ++ "!"

echo3 :: IO ()
echo3 =  getLine >>= \l1 -> getLine >>= \l2 -> putStrLn $ l1 ++ l2

dialog :: IO ()
dialog = putStr "What is your happy number? "
         >> getLine
         >>= \n -> let num = read n :: Int in
                   if num == 7
                   then putStrLn "Ah, lucky 7!"
                   else if odd num
                        then putStrLn "Odd number! That's most people's choice..."
                        else putStrLn "Hm, even number? Unusual!"

doEcho3 = do
    line1 <- getLine
    line2 <- getLine
    putStrLn $ line1 ++ line2

doDialog = do
    putStr "What is your happy number? "
    number <- getLine
    putStr (answer number) where
        answer number
            | num == 7 = "Ah, lucky 7!"
            | odd num = "Odd number! That's most people's choice..."
            | otherwise = "Hm, even number? Unusual!"
            where num = read number :: Int

--6 Funktory 2: dołączanie typów użytkownika do klasy Functor
newtype Box a = MkBox a deriving (Show, Functor)

--instance Functor Box where
--  fmap f (MkBox x) = MkBox (f x)

data MyList a = EmptyList
              | Cons a (MyList a) deriving (Show)

instance Functor MyList where
  fmap _ EmptyList    = EmptyList
  fmap f (Cons x mxs) = Cons (f x) (fmap f mxs)

data BinTree a = EmptyBT | NodeBT a (BinTree a) (BinTree a) deriving (Show)

instance Functor BinTree where
  fmap _ (EmptyBT) = EmptyBT
  fmap f (NodeBT x lt rt) = NodeBT (f x) (fmap f lt) (fmap f rt)

--8 Funktory aplikatywne 2: dołączanie typów użytkownika do klasy Applicative
newtype Box' a = MkBox' a deriving Show

instance Applicative Box' where
  pure = MkBox'
  (MkBox' f) <*> w = fmap f w

instance Functor Box' where
  fmap f (MkBox' x) = MkBox' (f x)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Applicative MyTriple where
  pure x = MyTriple (x, x, x)
  MyTriple(f1, f2, f3) <*> MyTriple(x1, x2, x3) = MyTriple (f1 x1, f2 x2, f3 x3)

instance Functor MyTriple where
  fmap f (MyTriple(x1, x2, x3)) = MyTriple(f x1, f x2, f x3)