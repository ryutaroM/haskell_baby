doubleMe x = x + x

doubleUs x y = x * 2 + y * 2

doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

boomBang xs = [if x > 10 then "BOOM" else "BAND" | x <- xs, odd x]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

-- factorial :: Integer -> Integer
-- factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, youre out of luck, pal!"

sayMe :: Int -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVec :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVec a b = (fst a + fst b, snd a + snd b)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "Cant call head on an empty list, dummy"
head' (x : _) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x : []) = "The list has one element" ++ show x
tell (x : y : []) = "The list has two elements" ++ show x
tell (x : y : _) = "The list is long. The first two elements are : " ++ show x ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops."
firstLetter all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> Double -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pfft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose your weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    skinny = 18.5
    normal = 25.0
    fat = 30.0

max' :: (Ord a) => a -> a -> a
max' a b
  | a <= b = b
  | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a == b = EQ
  | a <= b = LT
  | otherwise = GT

niceGreeting :: String
niceGreeting = "Hello! nice to meet you!"

badGreeting :: String
badGreeting = "Oh! Pfft. It's you!"

greet :: String -> String
greet "Juan" = niceGreeting ++ "Juan!"
greet "Fernando" = niceGreeting ++ "Fernando"
greet name = badGreeting ++ " " ++ name

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r ^ 2
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

head'' :: [a] -> a
head'' [] = error "No head for empty lists!"
head'' (x : _) = x

head''' :: [a] -> a
head''' xs = case xs of
  [] -> error "No head for empty lists!"
  (x : _) -> x

describeList :: [a] -> String
describeList ls =
  "The list is " ++ case ls of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list"

describeList' :: [a] -> String
describeList' ls =
  "The list is " ++ what ls
  where
    what [] = "empty."
    what [x] = "a singleton list."
    what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x : xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
   in quicksort smallerOrEqual ++ [x] ++ quicksort larger

multiThree :: Int -> Int -> Int -> Int
-- multiThree :: Int -> (Int -> (Int -> Int)) you can also write like this
-- In Golang https://go.dev/play/p/IVC9cHlC0Eb
multiThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

-- compareWithHundred = compare 100 you can also write like this

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where
    g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f y x = f x y

remindLet :: Int -> Int
remindLet x =
  let y = 2
      z = 3
   in x + y + z

quicksort''' :: (Ord a) => [a] -> [a]
quicksort''' [] = []
quicksort''' (x : xs) =
  let smallerOrEqual = filter (<= x) xs
      larger = filter (> x) xs
   in quicksort''' smallerOrEqual ++ [x] ++ quicksort''' larger