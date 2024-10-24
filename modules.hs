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