import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import PuzzleLogic
import Data.IORef
import Control.Monad as Monad 


-- структура для хранения состояния
data GameState = GameState {
	gameList :: Array Integer Integer,
	btnsList :: [Button ()]
}

-- получаем корректный label кнопки
btnLabel :: Integer -> String
btnLabel x = if (x == 16) then "" else (show x)

-- получаем набор кнопок по массиву
getBtns :: Window a -> Array Integer Integer -> IO ([Button ()])
getBtns wnd arr = sequence $ map (\x -> button wnd [text := (btnLabel x)]) (elems arr)

-- прикрепляем кнопки к окну
placeBtns :: (Form f, Widget w) => f -> [w] -> IO ()
placeBtns wnd btns = set wnd [layout := column 3 $ map (\x -> margin 3 $ row 3 (map widget x)) (chunksOf 4 btns)]

-- обновляем label на кнопках в соответствии с массивом
updateBtns :: [Button ()] -> Array Integer Integer -> IO ()
updateBtns btns arr = do
	let z = zip (elems arr) btns
	forM_ z $ \p -> set (snd p) [text := (btnLabel (fst p))]

-- callback-функция сделать ход
makeMove :: Moves -> IORef GameState -> IO ()
makeMove m ref = do
	st <- readIORef ref
	let game = gameList st
	let btns = btnsList st
	game' <- moving m game
	putStrLn $ "Сделан ход: " ++ show m
	updateBtns btns game'
	Monad.when (checkWin game') gameWinAction
	writeIORef ref (GameState game' btns)

-- действие при победе
gameWinAction :: IO ()
gameWinAction = putStrLn $ "ПОБЕДА!"

-- новая игра
newGame :: IORef GameState -> IO ()
newGame ref = do
	st <- readIORef ref
	let btns = btnsList st
	game' <- generateGame
	updateBtns btns game'
	writeIORef ref (GameState game' btns)
	
-- диалоговое окно, открытия файла
loadGame :: IORef GameState -> Window a -> Var(Maybe FilePath) -> IO ()
loadGame ref win filePath = do
	maybePath <- fileOpenDialog win True True "Загрузка игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
		varSet filePath $ Just path
		st <- readIORef ref
		let btns = btnsList st
		game' <- readGameFromFile path
		updateBtns btns game'
		writeIORef ref (GameState game' btns)

-- диалоговое окно, сохранения файла		
saveGame :: IORef GameState -> Window a -> Var(Maybe FilePath) -> IO ()
saveGame ref win filePath = do
	maybePath <- fileSaveDialog win True True "Сохранение игры..." [("Any file",["*.*"]),("Text",["*.txt"])] "" ""
	print maybePath
	case maybePath of
		Nothing -> return ()
		Just path -> do
			varSet filePath $ Just path
			st <- readIORef ref
			let game = gameList st
			writeGameToFile game path
		
main :: IO ()
main = start gui

gui :: IO ()
gui = do
  -- создаем окно
  wnd <- frame [ text := "Пятнашки", virtualSize := sz 300 300, bgcolor := blue ]
    
  -- создаем список кнопок по массиву
  game <- generateGame
  btns <- getBtns wnd game
  
  -- прикрепляем список кнопок к окну
  placeBtns wnd btns

  -- IORef
  let st = GameState game btns
  ref <- newIORef st
  
  -- добавляем сверху меню, с новой игрой, загрузкой и сохранением игры
  topLevelMenu <- menuPane [text := "Игра"]
  menuItem topLevelMenu [on command := newGame ref, text := "Новая игра"]
  menuAppend topLevelMenu wxID_OPEN "Загрузить игру" " " False
  menuAppend topLevelMenu wxID_SAVEAS "Сохранить игру" " " False
  menuQuit topLevelMenu [on command := wxcAppExit, text := "Выход"]
  
  -- обавляем действия по кнопке загрузить игру
  filePath <- varCreate Nothing
  evtHandlerOnMenuCommand wnd wxID_OPEN $ loadGame ref wnd filePath --по нажатию сохранить в FilePath путь к выбранному файлу
  
  -- добавляем действия по кнопке сохранить игру
  evtHandlerOnMenuCommand wnd wxID_SAVEAS $ saveGame ref wnd filePath
 
  -- вспомогательная функция, для вывода небольшого окна с текстом
  let say title desc = infoDialog wnd title desc
  
  -- отдельная кнопочка с помощью "Об авторах", самая важная часть
  topLevelMenuHelp <- menuHelp []
  menuAbout topLevelMenuHelp [on command := say "Игра пятнашки       "
								"Проект в рамках курса ФП. Команда: Пархоменко, Любаненко, Янушка"]
  
  -- добавляем менюшки к фрейму
  set wnd [menuBar := [topLevelMenu, topLevelMenuHelp]]
  
  -- добавляем действие по нажатию на кнопку на клаве
  set wnd [on (charKey 's') := makeMove UpMove ref,
		   on (charKey 'w') := makeMove DownMove ref,
		   on (charKey 'a') := makeMove RightMove ref,
		   on (charKey 'd') := makeMove LeftMove ref]

  return ()
