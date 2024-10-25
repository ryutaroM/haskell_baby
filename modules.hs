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