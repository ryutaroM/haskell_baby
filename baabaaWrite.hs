import Data.Char
import System.IO

main = do
  contents <- readFile "baabaa.txt"
  writeFile "baabaacaps.txt" (map toUpper contents)