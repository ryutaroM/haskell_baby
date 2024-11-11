import Control.Monad (forM, forever, when)
import Data.Char (toUpper)

-- main = do
--   putStr "I'm"
--   putStr " "
--   putStr "a"
--   putStr " "
--   putStrLn "rock star!"

-- main = do
--   putChar 't'
--   putChar 'e'
--   putChar 'h'

-- main = do
--   print True
--   print 1
--   print 3.2
--   print "foobar"
--   print [3, 4, 3]

-- main = do
--   input <- getLine
--   when (input == "SWORDFISH") $ do
--     putStrLn input

-- main = do
--   rs <- sequence [getLine, getLine, getLine]
--   print rs

-- main = forever $ do
--   putStr "Give me some input:"
--   l <- getLine

--   putStrLn $ map toUpper l
main = do
  colors <- forM [1, 2, 3, 4] $ \a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
    color <- getLine
    return color
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM putStrLn colors