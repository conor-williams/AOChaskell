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


loopThrough :: [String] -> [String] -> Int
--loopThrough [] list1 cur1 = return -9999
loopThrough [] list1 = -9999
loopThrough (str:vv) list1 = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt

    let (nn, tot2) = loop2 list1 list1 amt2
    if nn /= -99 then do
        nn
    else do
        let ans = loopThrough vv list1
        ans

loop2 :: [String] -> [String] -> Int -> (Int, Int)
loop2 [] list1 amt = (-99, -99)
loop2 (xx:vv1) list1 amt = do
    let sec = extractInt xx
    let sec2 = convertMaybeToInt sec
    let (nn, tot2) = loop3 list1 amt sec2
    if nn /= -99 then do
        (nn, tot2)
    else do
        let rr = loop2 vv1 list1 amt
        rr
      


loop3 :: [String] -> Int -> Int -> (Int, Int)
loop3 [] amt1 amt2 = (-99, -99)
loop3 (yyy:vv2) amt1 amt2 = do
    let thi = extractInt yyy
    let thi3 = convertMaybeToInt thi
    if (amt1 + amt2 + thi3) == 2020 then do
        ((amt1 * amt2 * thi3), (amt1 * amt2 * thi3)) 
    else do
         let qq = loop3 vv2 amt1 amt2
         qq

totup2 :: String -> [String] -> Int -> ([String], Int, Int)
totup2 str list freq = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt
    if str !! 0 == '+' then do
        let tot2 = freq + amt2
        if elem (show tot2) list then do
            (list, tot2, tot2)
        else do
            let newList = list ++ [show tot2]
            (newList, -99, tot2)
    else do
        let tot2 = freq - amt2
        if elem (show tot2) list then do
            (list, tot2, tot2)
        else do
            let newList = list ++ [show tot2]
            (newList, -99, tot2)


totUp :: String -> Int -> Int
totUp str tot = do
    let amt = extractInt str
    let amt2 = convertMaybeToInt amt
    if str !! 0 == '+' then do
        let tot2 = tot + amt2
        tot2
    else do
        let tot2 = tot - amt2
        tot2

extractInt :: String -> Maybe Int
extractInt str =
    let digits = filter isDigit str
    in if null digits then Nothing else Just (read digits :: Int)

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt maybeInt = fromMaybe 0 maybeInt

    
main :: IO ()
main = do
    putStrLn "\t\t2020 day1.2"
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
            print $ loopThrough v1 v1
