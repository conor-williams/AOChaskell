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


loopThrough :: [String] -> [String] -> Int -> Int
--loopThrough [] list1 cur1 = return -9999
loopThrough [] list1 cur1 = -9999
loopThrough (str:vv) list1 cur1 = do
    --let list1 = []
    --let cur1 = 0
    let (newList, nn, tot2) = totup2 str list1 cur1
    if nn /= -99 then do
        nn
    else do
        --let list1 = newList
        --let cur1 = freq
        let ans = loopThrough vv newList tot2
        ans

--countUp3 :: [String] -> Int
--countUp3 vv = runCont (callCC $ \exit -> do
--    let list1 = []
--    let cur1 = 0
--    let result = 0
--   
--    result <- forM_ vv $ \str -> do
--        let (newList, freq) = totup2 str list1 cur1
--        let list1 = newList
--        let cur1 = freq
--        if freq /= -99 then do
--            return cur1
--        else do
--            return 27
--
--    return 99
--    ) id
--    
        
--countUp4 :: [String] -> Int
--countUp4 vv = runCont (callCC $ \exit -> do
 --   let list1 = []
--    let cur1 = 0
    --let result = 0
   
 --   result <- forM_ vv $ \str -> do
 --       let (newList, freq) = totup2 str list1 cur1
 --       let list1 = newList
 --       let cur1 = freq
 --       when (freq /= -99) (exit (29))
 --   return result
 --   ) id
    
        

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
    putStrLn "\t\t2018 day1.2"
    putStrLn "\tSLOW ~30mins"

    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            --let vectorOfLines = V.fromList (lines content)
            --let v2 = vectorOfLines V.++ V.fromList (lines content);
            --let v3 = v2 V.++ V.fromList (lines content);
            --let v4 = v3 V.++ V.fromList (lines content);
            --let v5 = v4 V.++ V.fromList (lines content);
            --let v6 = v5 V.++ V.fromList (lines content);
            --let v7 = v6 V.++ V.fromList (lines content);
            --let v8 = v7 V.++ V.fromList (lines content);
            --let v9 = v8 V.++ V.fromList (lines content);
            let v1 = concat (replicate 200 (lines content))
            --print v20
            putStr "h_ans: "
            --countUp3 v4
            print $ loopThrough v1 [] 0
