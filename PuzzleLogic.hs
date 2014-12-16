module PuzzleLogic where

import System.Random
import Data.Array.IO
import Control.Monad
import Data.Maybe
import Data.List
import Data.Array
import Control.Monad.ST
import Data.Array.ST
import Data.List

type Board = Array Integer Integer
data Moves = UpMove | DownMove | RightMove | LeftMove
	deriving (Show, Eq)

-- генерация случайной перестановки
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

-- проверки на существование решение у случайной перестановки
checkListIO :: (Ord a, Num a, Monad m) => m [a] -> m Bool
checkListIO arr = liftM checkList arr
	where
		checkList arr = ((valueK arr) + (valueN arr)) `mod` 2 == 0
		valueK arr = (findElem 16 arr 0) `div` 4 + 1
		findElem _ [] _ = (-1)
		findElem el (x:xs) i = if el == x then i else findElem el xs (i + 1)
		valueN (x:xs) = fst $ foldl (\(val, pred) next -> (if pred > next then val + 1 else val, next)) (0, x) xs
	
-- генерация случайной игры
generateGame :: IO (Array Integer Integer)
generateGame = (listArray (0,15)) `liftM` (generateRandonList)
	where
		generateRandonList = do
			let arr = shuffle [1..16]
			val <- checkListIO arr
			if val then arr else generateRandonList

-- проверка на допустимость хода
checkMove :: (Ix i, Num a, Eq a) => Moves -> Array i a -> Bool
checkMove m arr = if (isJust i) then
		case m of
			UpMove ->(j `div` 4) > 0
			DownMove -> (j `div` 4) < 3
			RightMove -> (j `mod` 4) < 3
			LeftMove -> (j `mod` 4) > 0
	else False
	where 
		i = elemIndex 16 $ elems arr
		j = fromJust i
	
-- переходы
moving :: (Num a, Eq a) => Moves -> Array Integer a -> Array Integer a
moving m arr = if (checkMove m arr) then
		swap i (moveTo m i) arr
	else arr
	where
		i = fromIntegral (fromJust $ elemIndex 16 $ elems arr) :: Integer
		moveTo :: Moves -> Integer -> Integer
		moveTo m i 
			| m == UpMove = i - 4
			| m == DownMove = i + 4
			| m == RightMove = i + 1
			| otherwise = i - 1
	
-- перемена местами элементов в массиве
swap :: Ix i => Integer -> Integer -> Array i a -> Array Integer a
swap i j arr = runSTArray $ do
	let len = fromIntegral (length $ elems arr) :: Integer
	newarr <- newListArray (0, len-1) (elems arr)
	swap' i j newarr
	return newarr
	where
		swap' i j arr = do
			xi <- readArray arr i
			xj <- readArray arr j
			writeArray arr i xj
			writeArray arr j xi

checkWin :: Array Integer Integer -> Bool
checkWin arr = elems arr == [1..16]

writeGameToFile :: (Ix i, Show a) => Array i a -> FilePath -> IO ()
writeGameToFile arr filename = writeFile filename $ unwords $ map show $ elems arr

readGameFromFile :: FilePath -> IO (Array Integer Integer)
readGameFromFile filename = do
	elems <- readFromFile filename
	return $ listArray (0,15) elems
	
readFromFile :: FilePath -> IO [Integer]
readFromFile filename = do
	content <- readFile filename
	let elems = concatMap words $ lines content
	return $ map read elems :: IO [Integer]

-- тесты на корректность
test :: IO ()
test = do
	t <- generateGame
	print t
	let r = moving UpMove t
	print r
	let r = moving DownMove t
	print r
	let r = moving RightMove t
	print r
	let r = moving LeftMove t
	print r