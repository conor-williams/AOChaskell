import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)


countUp :: V.Vector String -> Int
countUp vv = do
    sum $ V.map (\str -> totUp str 0) vv

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
    putStrLn "\t\t2018 day1.1"
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let vectorOfLines = V.fromList (lines content)
            putStr "h_ans: "
            print $ countUp vectorOfLines
