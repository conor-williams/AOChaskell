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


loopThrough :: [String] -> Int -> Int
loopThrough (str:vv) tot1 = do
    let tot2 = totup2 str tot1
    if vv == [] then do
        tot2
    else do
        let ans = loopThrough vv tot2
        ans

totup2 :: String -> Int -> Int
totup2 str tot = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt
    let xx = amt2 `div` 3
    let zz = xx - 2
    if zz <= 0 then do
        tot
    else do 
        let yy = (tot + zz)
        let tot3 = totup2 (show zz) yy
        tot3

extractInt :: String -> Maybe Int
extractInt str =
    let digits = filter isDigit str
    in if null digits then Nothing else Just (read digits :: Int)

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt maybeInt = fromMaybe 0 maybeInt

main :: IO ()
main = do
    putStrLn "\t\t2019 day1.2"

    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let v1 = concat (replicate 1 (lines content))
            putStr "h_ans: "
            print $ loopThrough v1 0
