import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import PuzzleLogic

btnLabel :: Int -> String
btnLabel x = if (x == 16) then "" else (show x)

btnList :: Window a -> Array Int Int -> IO ([Button ()])
btnList wnd arr = sequence $ map (\x -> button wnd [text := (btnLabel x)]) (elems arr)

btnSet :: (Form f, Widget w) => f -> [w] -> IO ()
btnSet wnd btns = set wnd [layout := column 3 $ map (\x -> margin 3 $ row 3 (map widget x)) (chunksOf 4 btns)]

redrawGui :: Array Int Int -> IO ()
redrawGui a = undefined


--диалоговое окно, открытия файла
loadGame :: Window a -> Var(Maybe FilePath) -> IO ()
loadGame win filePath = do
	maybePath <- fileOpenDialog win True True "Загрузка игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
		varSet filePath $ Just path

--диалоговое окно, сохранения файла		
saveGame :: Window a -> Var(Maybe FilePath) -> IO ()
saveGame win filePath = do
	maybePath <- fileSaveDialog win True True "Сохранение игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
		varSet filePath $ Just path
		
main :: IO ()
main = start gui

gui :: IO ()
gui = do

  -- создаем окно
  wnd <- frame [ text := "Пятнашки", virtualSize := sz 300 300, bgcolor := blue ]
  
  --добавляем сверху меню, с новой игрой, загрузкой и сохранением игры
  topLevelMenu <- menuPane [text := "Игра"]
  menuItem topLevelMenu [on command := putStrLn "Не реализовано", text := "Новая игра"]
  menuAppend topLevelMenu wxID_OPEN "Загрузить игру" " " False
  menuAppend topLevelMenu wxID_SAVEAS "Сохранить игру" " " False
  menuQuit topLevelMenu [on command := wxcAppExit, text := "Выход"]
  --добавляем действия по кнопке загрузить игру
  filePath <- varCreate Nothing
  evtHandlerOnMenuCommand wnd wxID_OPEN $ loadGame wnd filePath --по нажатию сохранить в FilePath путь к выбранному файлу
 --добавляем действия по кнопке сохранить игру
  evtHandlerOnMenuCommand wnd wxID_SAVEAS $ saveGame wnd filePath
 
  --вспомогательная функция, для вывода небольшого окна с текстом
  let say title desc = infoDialog wnd title desc
  
  --отдельная кнопочка с помощью "Об авторах", самая важная часть
  topLevelMenuHelp <- menuHelp []
  menuAbout topLevelMenuHelp [on command := say "Игра пятнашки       "
								"Проект в рамках курса ФП. Команда: Пархоменко, Любаненко, Янушка"]
  
  --добавляем менюшки к фрейму
  set wnd [menuBar := [topLevelMenu, topLevelMenuHelp]]
  
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
