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


loopThrough :: [String] -> [Int] -> Int -> [Int]
loopThrough [] tots cur = do
    let ans = tots ++ [cur]
    ans
loopThrough (str:vv) tots cur = do
    if str == "" then do
        let ans = tots ++ [cur]
        let ans2 = loopThrough vv ans 0
        ans2
    else do
        let amt = extractInt str
        let amt2 = convertMaybeToInt amt
        let cur2 = cur + amt2
        let ans = loopThrough vv tots cur2
        ans

extractInt :: String -> Maybe Int
extractInt str =
    let digits = filter isDigit str
    in if null digits then Nothing else Just (read digits :: Int)

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt maybeInt = fromMaybe 0 maybeInt

    
main :: IO ()
main = do
    putStrLn "\t\t2022 day1.2"
    putStrLn "\t/ghc -package mtl xx.hs"

    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let v1 = concat (replicate 1 (lines content))
            --print v20
            putStr "h_ans: "
            --countUp3 v
            --print $ loopThrough v1 (reverse v1) -1
            let zz = loopThrough v1 [] 0
            --print zz
            let ii = reverse (sort zz)
            --print (maximum zz)
            let sum = ((ii !! 0) + (ii !! 1) + (ii !! 2))
            print sum
