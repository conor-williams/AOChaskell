import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (ord)

addFirst :: String -> String
addFirst str = do
    let str2 = ((str!!0): str)
    str2

charToString :: Char -> String -> String
charToString c s = c : s

counttoEnd :: String -> Int
counttoEnd str = do
    let zz = recur str (length str) 0 0
    zz

recur :: String -> Int -> Int -> Int -> Int
recur str len tot cur = do
    if cur == (len - 1) then do
        tot
    else do
        if (str !! cur) == (str !! (cur + 1)) then do
            let tot2 = tot + (ord (str !! cur)) - 48
            let qq = recur str len tot2 (cur+1)
            qq
        else do
            let qq = recur str len tot (cur+1)
            qq

processLines :: V.Vector String -> V.Vector Int
--processLines vec = V.map (\str -> (counttoEnd (addFirst str))) vec
processLines vec = V.map (\str -> (counttoEnd (str++[str!!0]))) vec

main :: IO ()
main = do
    putStrLn "\t\t2017 day1.1"
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let vectorOfLines = V.fromList (lines content)
            putStr "h_ans: "
            print $ processLines vectorOfLines
