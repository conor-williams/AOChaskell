import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Monad (when)
--import System.Exit (exitWith, ExitCode(..))
import Control.Monad.Cont
import Data.List
--import Data.IORef
--import System.IO.Unsafe (unsafePerformIO)
--import Control.Monad.ST
--import Data.STRef


loopThrough :: [String] -> [String] -> Int -> Int -> Int
--loopThrough [] list1 cur1 = return -9999
loopThrough [] list1 cnt pos = cnt
loopThrough (str:vv) list1 cnt pos = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt
    let sec = extractInt (list1 !! pos)
    let sec2 = convertMaybeToInt sec

    if pos == (length list1) then do
        cnt
    else do 
	if sec2 > amt2 then do
	    let ans = loopThrough vv list1 (cnt+1) (pos+1)
	    ans
	else do
	    let ans = loopThrough vv list1 (cnt) (pos+1)
	    ans

extractInt :: String -> Maybe Int
extractInt str =
    let digits = filter isDigit str
    in if null digits then Nothing else Just (read digits :: Int)

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt maybeInt = fromMaybe 0 maybeInt

    
main :: IO ()
main = do
    putStrLn "\t\t2021 day1.1"

    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let v1 = concat (replicate 1 (lines content))
            --print v20
            putStr "h_ans: "
            --countUp3 v
            --print $ loopThrough v1 (reverse v1) -1
            print $ loopThrough v1 v1 0 1
