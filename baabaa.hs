import GHC.Data.ShortText (ShortText (contents))
import System.IO

-- main = do
--   withFile "baabaa.txt" ReadMode $ \handle -> do
--     contents <- hGetContents handle
--     putStr contents

-- readFile ver
main = do
  contents <- readFile "baabaa.txt"
  putStr contents