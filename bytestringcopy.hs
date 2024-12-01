import Control.Exception
import Data.ByteString.Lazy qualified as B
import System.Directory
import System.Environment
import System.IO

main :: IO ()
main = do
  (f1 : f2:_) <- getArgs
  copy f1 f2

copy source dest = do
  contents <- B.readFile source
  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandler) -> do
        hClose tempHandler
        removeFile tempName
    )
    ( \(tempName, tempHandler) -> do
        B.hPutStr tempHandler contents
        hClose tempHandler
        renameFile tempName dest
    )