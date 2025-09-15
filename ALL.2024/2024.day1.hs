import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Control.Monad (forM_)
import Control.Monad (when)
--import System.Exit (exitWith, ExitCode(..))
import Control.Monad.Cont
import Data.List
import Data.List (isPrefixOf)
--import Data.IORef
--import System.IO.Unsafe (unsafePerformIO)
--import Control.Monad.ST
--import Data.STRef


loopThrough :: [String] -> Int -> [Int] -> [Int] -> ([Int], [Int])
loopThrough [] pos123 left right = (left, right)
loopThrough (str:vv) pos123 left right = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt
    if (pos123 `mod` 2) == 0 then do
        let left2 = left ++ [amt2]
        let ans = loopThrough vv (pos123 + 1) left2 right
        --(left2, right)
        ans
    else do
        let right2 = right ++ [amt2]
        let ans = loopThrough vv (pos123 + 1) left right2
        --(left, right2)
        ans

loop2 :: [Int] -> [Int] -> Int -> Int -> Int
loop2 [] vv2 pos1 tot = tot
loop2 (xx:vv) vv2 pos1 tot = do
    let tot2 = tot + (abs (xx - (vv2 !! pos1)))
    let ans = loop2 vv vv2 (pos1+1) tot2
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
    putStrLn "\t\t2024 day1.1"
    putStrLn "\t/ghc -package mtl xx.hs"

    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let yy = replaceSubstring "   " "\n" content
            let v1 = concat (replicate 1 (lines yy))
            --print (lines yy)
            putStr "h_ans: "
            let (le, ri) = loopThrough v1 0 [] []
            let le1 = sort le
            let ri1 = sort ri
            let xx = loop2 le1 ri1 0 0
            print xx

replaceSubstring :: String -> String -> String -> String
replaceSubstring _ _ [] = []
replaceSubstring old new str
  | old `isPrefixOf` str = new ++ replaceSubstring old new (drop (length old) str)
  | otherwise            = head str : replaceSubstring old new (tail str)

