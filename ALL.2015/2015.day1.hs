import qualified Data.Vector as V
import System.Environment (getArgs)

countChar2 :: Char -> String -> Int
countChar2 c = foldl (\acc x -> if x == c then acc + 1 else acc) 0

processLines :: V.Vector String -> V.Vector Int
processLines vec = V.map (\str -> (countChar2 '(' str) - (countChar2 ')' str)) vec

main :: IO ()
main = do
    putStrLn "\t\t2015 day1.1"
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let vectorOfLines = V.fromList (lines content)
            putStr "h_ans: "
            print $ processLines vectorOfLines
