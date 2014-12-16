import Control.Monad
import Control.Monad.Trans
import Control.Monad.State as State
import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import PuzzleLogic

data GameState = GameState {
	gameList :: Array Integer Integer,
	btnsList :: [Button ()]	
}

btnLabel :: Integer -> String
btnLabel x = if (x == 16) then "" else (show x)

-- получаем список кнопок по массиву "Пятнашек"
getBtns :: Window a -> Array Integer Integer -> IO ([Button ()])
getBtns wnd arr = sequence $ map (\x -> button wnd [text := (btnLabel x)]) (elems arr)

-- прикрепляет список кнопок к окну
setBtns :: (Form f, Widget w) => f -> [w] -> IO ()
setBtns wnd btns = set wnd [layout := column 3 $ map (\x -> margin 3 $ row 3 (map widget x)) (chunksOf 4 btns)]

-- обновлет label кнопок
updateBtns :: StateT GameState IO ()
updateBtns = do
	st <- State.get
	let game = gameList st
	let btns = btnsList st
	let z = zip (elems game) btns
	forM_ z $ \p -> liftIO $ set (snd p) [text := (btnLabel (fst p))]
	

redrawGui :: Array Integer Integer -> IO ()
redrawGui a = undefined


-- диалоговое окно, открытия файла
loadGame :: Window a -> Var(Maybe FilePath) -> IO ()
loadGame win filePath = do
	maybePath <- fileOpenDialog win True True "Загрузка игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
		varSet filePath $ Just path

-- диалоговое окно, сохранения файла
saveGame :: Window a -> Var(Maybe FilePath) -> IO ()
saveGame win filePath = do
	maybePath <- fileSaveDialog win True True "Сохранение игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
		varSet filePath $ Just path

-- новая игра
newGame :: StateT GameState IO ()
newGame = do
	st <- State.get
	let btns = btnsList st
	game <- liftIO $ generateGame
	put $ GameState game btns
	updateBtns

	
main :: IO ()
main = start $ execStateT gui (GameState defaultGame [])

gui :: StateT GameState IO () 
gui = do
  -- создаем окно
  wnd <- liftIO $ frame [ text := "Пятнашки", virtualSize := sz 300 300, bgcolor := blue ]
  
  -- добавляем сверху меню, с новой игрой, загрузкой и сохранением игры
  topLevelMenu <- liftIO $ menuPane [text := "Игра"]
  liftIO $ menuItem topLevelMenu [on command := newGame, text := "Новая игра"]
  liftIO $ menuAppend topLevelMenu wxID_OPEN "Загрузить игру" " " False
  liftIO $ menuAppend topLevelMenu wxID_SAVEAS "Сохранить игру" " " False
  liftIO $ menuQuit topLevelMenu [on command := wxcAppExit, text := "Выход"]
  
  -- обавляем действия по кнопке загрузить игру
  filePath <- liftIO $ varCreate Nothing
  liftIO $ evtHandlerOnMenuCommand wnd wxID_OPEN $ loadGame wnd filePath --по нажатию сохранить в FilePath путь к выбранному файлу
  
  -- добавляем действия по кнопке сохранить игру
  liftIO $ evtHandlerOnMenuCommand wnd wxID_SAVEAS $ saveGame wnd filePath
 
  -- вспомогательная функция, для вывода небольшого окна с текстом
  let say title desc = infoDialog wnd title desc
  
  -- отдельная кнопочка с помощью "Об авторах", самая важная часть
  topLevelMenuHelp <- liftIO $ menuHelp []
  liftIO $ menuAbout topLevelMenuHelp [on command := say "Игра пятнашки       "
								"Проект в рамках курса ФП. Команда: Пархоменко, Любаненко, Янушка"]
  
  -- добавляем менюшки к фрейму
  liftIO $ set wnd [menuBar := [topLevelMenu, topLevelMenuHelp]]
  
  -- создаем список кнопок по массиву  
  game <- liftIO $ generateGame
  btns <- liftIO $ getBtns wnd game
  State.put $ GameState game btns
  
  -- прикрепляем список кнопок к окну
  liftIO $ setBtns wnd btns
  
  -- добавляем действие по нажатию на кнопку на клаве
  liftIO $ set wnd [on (charKey 's') := putStrLn "S button pushed",
		   on (charKey 'w') := putStrLn "W button pushed",
		   on (charKey 'a') := putStrLn "A button pushed",
		   on (charKey 'd') := putStrLn "D button pushed"]

  return ()
