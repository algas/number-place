module NumberPlace where

import Data.Char
import Data.List
import Data.Array
import Control.Applicative
import System.Environment (getArgs)

type Index = (Int,Int)
type Board = Array Index Int

-- example :: [[Int]]
-- example = 
    -- [[5,3,0 ,0,7,0 ,0,0,0]
    -- ,[6,0,0 ,1,9,5 ,0,0,0]
    -- ,[0,9,8 ,0,0,0 ,0,6,0]
    -- ,[8,0,0 ,0,6,0 ,0,0,3]
    -- ,[4,0,0 ,8,0,3 ,0,0,1]
    -- ,[7,0,0 ,0,2,0 ,0,0,6]
    -- ,[0,6,0 ,0,0,0 ,2,8,0]
    -- ,[0,0,0 ,4,1,9 ,0,0,5]
    -- ,[0,0,0 ,0,8,0 ,0,7,9]
    -- ]

boardBounds :: (Index,Index)
boardBounds = ((1,1),(9,9))

createBoard :: [[Int]] -> Board
createBoard = listArray boardBounds . concat

-- board :: Board
-- board = createBoard example

showBoard :: Board -> String
showBoard b = intercalate "\n" [intersperse ',' (concat [show y | y <- ys]) | ys <- horizontalElems b]

update :: Board -> Index -> Int -> Board
update b i x
    | (b ! i) == 0 = b // [(i,x)]
    | otherwise = b

updates :: Board -> [(Index,Int)] -> Board
updates b [] = b
updates b ((x,y):zs) = updates (update b x y) zs

splits :: Int -> [a] -> [[a]]
splits n [] = []
splits n xs = [take n xs] ++ splits n (drop n xs)

splitAt3 :: (Int,Int) -> [a] -> [[a]]
splitAt3 (m,n) xs = [(take m xs), (drop m (take n xs)), (drop n xs)]

horizontalElems :: Board -> [[Int]]
horizontalElems = splits 9 . elems

verticalElems :: Board -> [[Int]]
verticalElems = transpose . horizontalElems

blockElems :: Board -> [[Int]]
blockElems b = concat [ sp xss | xss <- (splits 27 . elems) b]
    where
        sp :: [Int] -> [[Int]]
        sp xss = map concat $ transpose [splitAt3 (3,6) xs | xs <- splits 9 xss]

isFinished :: Board -> Bool
isFinished b = all check $ [horizontalElems, verticalElems, blockElems] <*> [b]
    where
        check :: [[Int]] -> Bool
        check xs = all (==[1..9]) $ map sort xs

count :: Board -> Int -> Int
count b n = sum [1 | x <- elems b, x == n]

vacancies :: Board -> [Index]
vacancies b = [i | (i,n) <- assocs b, n == 0]

anti :: [Int] -> [Int]
anti xs = [n | n <- [1..9], notElem n xs]

horizontalIndices :: Index -> [Index]
horizontalIndices (i,_) = [(i,j) | j <- [1..9]]

verticalIndices :: Index -> [Index]
verticalIndices (_,i) = [(j,i) | j <- [1..9]]

blockIndices :: Index -> [Index]
blockIndices (i,j) = [(f i + x, f j + y) | x <- [1..3], y <- [1..3]]
    where f n = 3 * ((n - 1) `div` 3)

getIndices :: Board -> Int -> [Index]
getIndices b x = [i | (i,n) <- assocs b, n == x]

candidateIndices :: Board -> Int -> [Index]
candidateIndices b x = [v | v <- vacancies b, v `notElem` (filled b x)]
    where
        filled :: Board -> Int -> [Index]
        filled z y = nub [j | i <- getIndices z y, j <- concat ([horizontalIndices, verticalIndices, blockIndices] <*> [i])]

removeDuplicates :: [Index] -> [Index]
removeDuplicates xs = [x | x <- xs, noDuplicates x xs]
    where
        noDuplicates :: Index -> [Index] -> Bool
        noDuplicates i js = any (==0) [checkCount (i,js) f | f <- [horizontalIndices, verticalIndices, blockIndices]]
        checkCount :: (Index,[Index]) -> (Index -> [Index]) -> Int
        checkCount (i,js) f = length [1 | j <- js, f i == f j, i /= j]

candidates :: Board -> Int -> [(Index, Int)]
candidates b n = zp n (removeDuplicates (candidateIndices b n))
    where
        zp x ys = zip ys (replicate (length ys) x)

solveStep :: Board -> Board
solveStep b = updates b $ concat [candidates b i | i <- [1..9]]

solve :: Board -> Board
solve b
    | isFinished b = b
    | s == b = b
    | otherwise = solve s
    where
        s = solveStep b

gameMain :: Board -> IO ()
gameMain b = do
    putStrLn $ (showBoard b) ++ "\n"
    if isFinished b
        then return ()
        else do
            gameMain $ updates b $ concat [candidates b i | i <- [1..9]]

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split c xs = f : split c (dropWhile (==c) s)
    where
        (f,s) = break (==c) xs

readBoard :: String -> Board
readBoard str = createBoard [[readInt s | s <- split ',' line] | line <- lines str]
    where
        readInt :: String -> Int
        readInt st = read st

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    fin <- readFile fileName
    let b = readBoard fin
    putStrLn $ head args
    putStrLn $ showBoard b
    putStrLn "result"
    putStrLn $ showBoard $ solve b
