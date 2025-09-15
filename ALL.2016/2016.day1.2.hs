import qualified Data.Vector as V
import System.Environment (getArgs)
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Data.List (concat)
import Data.List (intercalate)

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
    putStrLn "\t\t2016 day1.2"
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
processLines vec = V.map (\str -> strtok (str ++ ",") "," [] 0 0 0) vec

addComma :: String -> String
addComma str = str ++ ","

pr :: String -> IO ()
pr x = do 
    putStrLn x

--prInt :: Int -> IO ()
--prInt x = do
 --   putStrLn show x

check2 :: [String] -> Int -> Int -> Int -> Int -> Int -> ([String], Int)
check2 list gdir ns ew ns2 ew2 = do
    if ns == ns2 then do
        if gdir == 1 then do
            let zz = recur2 list ns (ew+1) ew2
            zz
        else do
            let zz = recur4 list ns (ew-1) ew2
            zz
    else do
        if gdir == 2 then do
            let zz = recur3 list ew (ns+1) ns2
            zz
        else do
            let zz = recur5 list ew (ns-1) ns2
            zz
 

recur2 :: [String] -> Int -> Int -> Int -> ([String], Int)
recur2 list ns ew1 ew2 = do
    if ew1 == (ew2+1) then do
        (list, -9999)
    else do
        let newElem = (show ew1 ++ "," ++ show ns)
        if elem newElem list then do
            (list, (abs ns + abs ew1))
            --(list, newElem)
        else do
            let newList = list ++ [newElem]
            let zz = recur2 newList ns (ew1+1) ew2
            zz
      
recur4 :: [String] -> Int -> Int -> Int -> ([String], Int)
recur4 list ns ew1 ew2 = do
    if ew1 == (ew2-1) then do
        (list, -9999)
    else do
        let newElem = (show ew1 ++ "," ++ show ns)
        if elem newElem list then do
            (list, (abs ns + abs ew1))
            --(list, newElem)
        else do
            let newList = list ++ [newElem]
            let zz = recur4 newList ns (ew1-1) ew2
            zz
      
              
recur3 :: [String] -> Int -> Int -> Int -> ([String], Int)
recur3 list ew ns1 ns2 = do
    if ns1 == (ns2+1) then do
        (list, -9999)
    else do
        let newElem = (show ew ++ "," ++ show ns1)
        if elem newElem list then do
            (list, (abs ns1 + abs ew))
            --(list, newElem)
        else do
            let newList = list ++ [newElem]
            let zz = recur3 newList ew (ns1+1) ns2
            zz
 
     
recur5 :: [String] -> Int -> Int -> Int -> ([String], Int)
recur5 list ew ns1 ns2 = do
    if ns1 == (ns2-1) then do
        (list, -9999)
    else do
        let newElem = (show ew ++ "," ++ show ns1)
        if elem newElem list then do
            (list, (abs ns1 + abs ew))
            --(list, newElem)
        else do
            let newList = list ++ [newElem]
            let zz = recur5 newList ew (ns1-1) ns2
            zz

strtok :: String -> String -> [String] -> Int -> Int -> Int -> Int
strtok str delims list gdir ns ew = do

    let str2 = removeSpaces str
    let (token, _:rest) = break (`elem` delims) str2
    if rest == "" then do
        let (gdir2, ns2, ew2) = modifyDir token gdir ns ew
        let (newList, qq) = check2 list gdir2 ns ew ns2 ew2
        if qq == -9999 then do
            -9999
        else do
            qq
        --let zz = (abs ns2 + abs ew2)
        --intercalate " " list
        --"----"
        --print gdir2
    else do
        let (gdir2, ns2, ew2) = modifyDir token gdir ns ew
        let (newList, qq) = check2 list gdir2 ns ew ns2 ew2
        if qq == -9999 then do
            let zz = strtok rest delims newList gdir2 ns2 ew2
            zz
        else do
            --intercalate " " newList
	    qq
