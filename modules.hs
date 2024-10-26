import Data.Char qualified as C
import Data.List
import Data.Map qualified as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

wordsNums :: String -> [(String, Int)]
wordsNums = map (\ws -> (head ws, length ws)) . group . sort . words

isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = any (needle `isPrefixOf`) (tails haystack)

encode :: Int -> String -> String
encode offset msg = map (\c -> C.chr $ C.ord c + offset) msg

decode :: Int -> String -> String
decode shifter msg = encode (negate shifter) msg

digitSum :: Int -> Int
digitSum = sum . map C.digitToInt . show

firstTo40 :: Maybe Int
firstTo40 = find (\x -> digitSum x == 40) [1 ..]

firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1 ..]

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key [] = Nothing
findKey' key ((k, v) : xs)
  | key == k = Just v
  | otherwise = findKey' key xs

findKey'' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey'' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

phoneBook :: M.Map String String
phoneBook =
  M.fromList $
    [ ("betty", "555-2938"),
      ("bonnie", "452-2928"),
      ("patsy", "493-2928"),
      ("lucille", "205-2928"),
      ("wendy", "939-8282"),
      ("penny", "853-2492")
    ]

phoneBook' =
  [ ("betty", "555-2938"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492"),
    ("betty", "112-321")
  ]

string2digits :: String -> [Int]
string2digits = map C.digitToInt . filter C.isDigit

phoneBookToMap :: (Ord k) => [(k, String)] -> M.Map k String
phoneBookToMap xs = M.fromListWith add xs
  where
    add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k, a)] -> M.Map k [a]
phoneBookToMap' xs = M.fromListWith (++) $ map (\(k, v) -> (k, [v])) xs