import Data.Foldable qualified as F
import Data.Map qualified as Map

-- data Person = Person String String Int Float String String deriving (Show)

-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

data Car = Car String String Int deriving (Show)

data Car' = Car'
  { company :: String,
    model :: String,
    year :: Int
  }
  deriving (Show)

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

data Person' = Person'
  { firstName' :: String,
    lastName' :: String,
    age' :: Int
  }
  deriving (Eq)

mikeD = Person' {firstName' = "Michael", lastName' = "Diamond", age' = 43}

adRock = Person' {firstName' = "Adam", lastName' = "Horovitz", age' = 41}

mca = Person' {firstName' = "Adam", lastName' = "Yauch", age' = 44}

data Person'' = Person''
  { firstName'' :: String,
    lastName'' :: String,
    age'' :: Int
  }
  deriving (Eq, Show, Read)

mikeD' = Person'' {firstName'' = "Michael", lastName'' = "Diamond", age'' = 43}

mysteryDude = "Person'' { firstName'' = \"Michael\", lastName'' = \"Diamond\", age'' = 43}"

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

phoneBook :: [(String, String)]
phoneBook =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492")
  ]

-- type PhoneBook = [(String, String)]

type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

type AssocList k v = [(k, v)]

type IntMap v = Map.Map Int v

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
  Just (state, code) ->
    if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

infixr 5 :-:

data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++

(^++) :: List' a -> List' a -> List' a
Empty' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r

singleTone :: a -> Tree a
singleTone x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleTone x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

testTree =
  Node
    5 -- root
    ( Node
        3 -- left
        (Node 1 EmptyTree EmptyTree)
        (Node 6 EmptyTree EmptyTree)
    )
    ( Node
        9 -- right
        (Node 8 EmptyTree EmptyTree)
        (Node 10 EmptyTree EmptyTree)
    )

-- testTreeの構造を図で示します
--         5
--        / \
--       3   9
--      / \ / \
--     1  6 8  10

-- main :: IO ()
-- main = do
--   print $ F.foldl (+) 0 testTree
--   print $ F.foldl (*) 1 testTree