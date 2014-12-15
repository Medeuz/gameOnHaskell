import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split

btnLabel :: Int -> String
btnLabel x = if (x == 16) then "" else (show x)

btnList :: Window a -> Array Int Int -> IO ([Button ()])
btnList wnd arr = sequence $ map (\x -> button wnd [text := (btnLabel x)]) (elems arr)

btnSet :: (Form f, Widget w) => f -> [w] -> IO ()
btnSet wnd btns = set wnd [layout := column 3 $ map (\x -> margin 3 $ row 3 (map widget x)) (chunksOf 4 btns)]

redrawGui :: Array Int Int -> IO ()
redrawGui a = undefined

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  -- создаем окно
  wnd <- frame [ text := "Пятнашки", size := sz 300 300, bgcolor := blue ]
  
  -- создаем список кнопок по массиву
  let btnsArray = array (1,16) [(i,i) | i <- [1..16]]
  btns <- btnList wnd btnsArray
  
  -- прикрепляем список кнопок к окну
  btnSet wnd btns
  
  --добавляем действие по нажатию на кнопку на клаве
  set wnd [on (charKey 's') := putStrLn "S button pushed",
		   on (charKey 'w') := putStrLn "W button pushed",
		   on (charKey 'a') := putStrLn "A button pushed",
		   on (charKey 'd') := putStrLn "D button pushed"]

  return ()