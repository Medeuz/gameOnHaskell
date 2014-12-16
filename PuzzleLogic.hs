module PuzzleLogic where

import System.Random
import Data.Array.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Array
import Control.Monad.ST
import Data.Array.ST

data Moves = UpMove | DownMove | RightMove | LeftMove
	deriving (Show, Eq)

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

valueK arr = (findElem 16 arr 0) `div` 4 + 1
	where
		findElem _ [] _ = (-1)
		findElem el (x:xs) i = if el == x then i else findElem el xs (i + 1)
		
	
valueN (x:xs) = fst $ foldl (\(val, pred) next -> (if pred > next then val + 1 else val, next)) (0, x) xs

getO k n = (k + n) `mod` 2 == 0

checkList arr = ((valueK arr) + (valueN arr)) `mod` 2 == 0

checkListIO arr = liftM checkList arr

generateGame = do
	let arr = shuffle [1..16]
	val <- checkListIO arr
	if val then arr else generateGame
	
getArrayGame = (listArray (0,15)) `liftM` (generateGame)

checkMove :: Moves -> IO (Array Integer Integer) -> IO Bool
checkMove m arr = do
	let i = (elemIndex 16) `liftM` (elems `liftM` arr)
	let pos = (fromMaybe (-1)) `liftM` i
	case m of
		UpMove -> (>0) `liftM` ((`div` 4) `liftM` pos)
		DownMove -> (<3) `liftM` (`div` 4) `liftM` pos
		RightMove -> (<3) `liftM` (`mod` 4) `liftM` pos
		LeftMove -> (>0) `liftM` (`mod` 4) `liftM` pos
		
moving m arr = join $ do
	let check = checkMove m arr
	check >>= (\val -> if val then return $ getArrayGame else return arr)


swap i j arr = elems $ runSTArray $ do
	let len = length arr
	newarr <- newListArray (0, len - 1) arr
	swap' i j newarr
	return newarr
	
swap' i j arr = do
	xi <- readArray arr i
	xj <- readArray arr j
	writeArray arr i xj
	writeArray arr j xi