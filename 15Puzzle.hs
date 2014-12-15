import System.Random
import Data.Array.IO
import Control.Monad
import Data.Maybe
import Data.List

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

checkGame arr = ((valueK arr) + (valueN arr)) `mod` 2 == 0