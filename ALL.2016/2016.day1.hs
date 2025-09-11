import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Debug.Trace

finc :: Int -> Int
finc x = (x + 1) `mod` 4
--finc x = x + 1
fdec :: Int -> Int
fdec x = (x - 1 + 4) `mod` 4
--fdec x = x - 1

extractInt :: String -> Maybe Int
extractInt str =
    let digits = filter isDigit str
    in if null digits then Nothing else Just (read digits :: Int)

main :: IO ()
main = do
    putStrLn "\t\t2016 day1.1"
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let vectorOfLines = V.fromList (lines content)
            putStr "h_ans: "
            print $ processLines vectorOfLines
            --strtok "R5, R5, R5, R3, R5, R10, R20" "," 0 0 0 
            --strtok "R5, R5, R5, R6, R7," "," 0 0 0 
            --print qq
            --print 44
            --print xx

adjust :: Int -> Int -> Int -> Int -> (Int, Int)
adjust gdir amt ns ew
  | gdir == 0 = (ns - amt, ew)
  | gdir == 1 = (ns, ew + amt)
  | gdir == 2 = (ns + amt, ew)
  | gdir == 3 = (ns, ew - amt)

convertMaybeToInt :: Maybe Int -> Int
convertMaybeToInt maybeInt = fromMaybe 0 maybeInt

modifyDir :: String -> Int -> Int -> Int -> (Int, Int, Int)
modifyDir token gdir ns ew = do 
    if token !! 0 == 'R' then do 
        let gdir3 = finc gdir;
        let amt = extractInt token;
        let amt2 = convertMaybeToInt amt;
        if amt2 == 0 then do
            --let x = x + 1
            (gdir3, ns, ew)
        else do
            let (ns3, ew3) = adjust gdir3 amt2 ns ew
            (gdir3, ns3, ew3)
    else do 
        let gdir3 = fdec gdir
        let amt = extractInt token
        let amt2 = convertMaybeToInt amt
        if amt2 == 0 then do
            --let x = x + 1
            (gdir3, ns, ew)
        else do
            let (ns3, ew3) = adjust gdir3 amt2 ns ew
            (gdir3, ns3, ew3)
       
removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

processLines :: V.Vector String -> V.Vector Int
processLines vec = V.map (\str -> strtok (str ++ ",") "," 0 0 0) vec

addComma :: String -> String
addComma str = str ++ ","

pr :: String -> IO ()
pr x = do 
    putStrLn x

--prInt :: Int -> IO ()
--prInt x = do
 --   putStrLn show x

strtok :: String -> String -> Int -> Int -> Int -> Int
strtok str delims gdir ns ew = do

    let str2 = removeSpaces str
    let (token, _:rest) = break (`elem` delims) str2
    if rest == "" then do
        let (gdir2, ns2, ew2) = modifyDir token gdir ns ew
        let zz = (abs ns2 + abs ew2)
        --print gdir2
        zz
    else do
        let (gdir2, ns2, ew2) = modifyDir token gdir ns ew
        --let zz = (abs ns2 + abs ew2)
        let zz = strtok rest delims gdir2 ns2 ew2
        --print gdir2
        zz
