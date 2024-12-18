import Control.Exception (bracketOnError)
import Data.List
import System.Directory
import System.Environment
import System.IO

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch "bump" = bump
dispatch command = doesntExist command

main = do
  args <- getArgs
  if null args
    then putStrLn "Usage: todo add|view|remove|bump"
    else do
      let (command : argList) = args
      dispatch command argList

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments"

view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStr $ unlines numberedTasks
view _ = putStrLn "The view command takes exactly one argument"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks

  putStrLn "These are your TO-DO items:"
  mapM_ putStrLn numberedTasks
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName
    )
remove _ = putStrLn "The remove command takes exactly two arguments"

bump :: [String] -> IO ()
bump [f, n] = do
  contents <- readFile f
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks

  mapM_ putStrLn numberedTasks
  let number = read n
      bumpedTask = todoTasks !! number
      newTodoItems = bumpedTask : delete bumpedTask todoTasks
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName
    )
    ( \(tempName, tempHandle) -> do
        hPutStr tempHandle $ unlines newTodoItems
        hClose tempHandle
        removeFile f
        renameFile tempName f
    )
bump _ = putStrLn "The bump command takes exactly two arguments"

doesntExist :: String -> [String] -> IO ()
doesntExist command _ = putStrLn $ "The " ++ command ++ " command doesn't exist"
