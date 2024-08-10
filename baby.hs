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