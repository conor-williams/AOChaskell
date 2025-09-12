import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (ord)

belowChar3 :: String -> Int
belowChar3 str = do
    let zz = recur str 0 0
    zz

recur :: String -> Int -> Int -> Int
recur str tot cur = do
    if (str !! cur) == '(' then do
        let tot2 = tot + 1
        let qq = recur str tot2 (cur+1)
        qq
    else do
        let tot2 = tot -1
        if tot2 == -1 then do
            cur + 1
        else do
            let qq = recur str tot2 (cur+1)
            qq
    
processLines :: V.Vector String -> V.Vector Int
--processLines vec = V.map (\str -> (counttoEnd (addFirst str))) vec
processLines vec = V.map (\str -> belowChar3 str) vec

main :: IO ()
main = do
    putStrLn "\t\t2015 day1.2"
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let vectorOfLines = V.fromList (lines content)
            putStr "h_ans: "
            print $ processLines vectorOfLines
