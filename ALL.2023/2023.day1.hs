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
loopThrough [] cur = cur
loopThrough (str:vv) cur = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt
    let xx = ((firstDigit amt2) * 10) + (amt2 `mod` 10)
    let cur2 = cur + xx
    let ans = loopThrough vv cur2
    ans

firstDigit :: Int -> Int
firstDigit n = read [head (show (abs n))] :: Int

extractInt :: String -> Maybe Int
extractInt str =
    let digits = filter isDigit str
    in if null digits then Nothing else Just (read digits :: Int)

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt maybeInt = fromMaybe 0 maybeInt

    
main :: IO ()
main = do
    putStrLn "\t\t2023 day1.1"
    putStrLn "\t/ghc -package mtl xx.hs"

    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let v1 = concat (replicate 1 (lines content))
            putStr "h_ans: "
            let zz = loopThrough v1 0
            print (zz)
