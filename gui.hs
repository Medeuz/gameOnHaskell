import Graphics.UI.WXCore
import Graphics.UI.WX
import Data.Array.IArray
import Data.List.Split
import Data.Maybe
import PuzzleLogic
import Data.IORef
import Control.Monad as Monad 


-- структуры данных
data GameState = GameState {
	board :: Board,
	buttons :: [Button ()]
}

-- возвращает корректный label кнопки
btnLabel :: Integer -> String
btnLabel x = if (x == 16) then "" else (show x)

-- возвращает набор кнопок по Board
getBtns :: Window a -> Board -> IO ([Button ()])
getBtns wnd brd = sequence $ map (\x -> button wnd [text := (btnLabel x), clientSize := sz 50 50]) (elems brd)

-- прикрепляет кнопки к окну
placeBtns :: (Form f, Widget w) => f -> [w] -> IO ()
placeBtns wnd btns = set wnd [layout := minsize (sz 300 155) $column 3 $ map (\x -> margin 3 $ row 3 (map widget x)) (chunksOf 4 btns)]

-- обновляет label на кнопках в соответствии с Board
updateBtns :: [Button ()] -> Board -> IO ()
updateBtns btns brd = do
	let z = zip (elems brd) btns
	forM_ z $ \p -> set (snd p) [text := (btnLabel (fst p))]

-- callback-функция сделать ход
makeMove :: Moves -> IORef GameState -> IO () -> IO ()
makeMove m ref winAction= do
	st <- readIORef ref
	let (brd, btns) = (board st, buttons st)
	let brd' = moving m brd
	putStrLn $ "Сделан ход: " ++ show m
	updateBtns btns brd'
	Monad.when (checkWin brd') winAction
	writeIORef ref (GameState brd' btns)

-- новая игра
newGame :: IORef GameState -> IO ()
newGame ref = do
	st <- readIORef ref
	let btns = buttons st
	brd' <- generateGame
	updateBtns btns brd'
	writeIORef ref (GameState brd' btns)
	
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
		let btns = buttons st
		brd' <- readGameFromFile path
		updateBtns btns brd'
		writeIORef ref (GameState brd' btns)

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
			let brd = board st
			writeGameToFile brd path
		
main :: IO ()
main = start gui

gui :: IO ()
gui = do
  -- создаем окно
  let wndTitle = "Игра \"Пятнашки\""
  wnd <- frame [ text := wndTitle, bgcolor := blue ]
  
  -- вспомогательная функция, для вывода небольшого окна с текстом  
  let say desc = infoDialog wnd wndTitle desc
  let aboutPopup = say "Проект в рамках курса ФП. Команда: Пархоменко, Любаненко, Янушка"
  let winPopup = say "Победа!"
  
  -- создаем список кнопок по массиву
  brd <- generateGame
  btns <- getBtns wnd brd
  
  -- прикрепляем список кнопок к окну
  placeBtns wnd btns

  -- IORef
  let st = GameState brd btns
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
 
  -- отдельная кнопочка с помощью "Об авторах", самая важная часть
  topLevelMenuHelp <- menuHelp []
  menuAbout topLevelMenuHelp [on command := aboutPopup]
  
  -- добавляем менюшки к фрейму
  set wnd [menuBar := [topLevelMenu, topLevelMenuHelp]]
  
  -- добавляем действие по нажатию на кнопку на клаве
  set wnd [ 
		   on (charKey 's') := makeMove UpMove ref winPopup,
		   on (charKey 'w') := makeMove DownMove ref winPopup,
		   on (charKey 'a') := makeMove RightMove ref winPopup,
		   on (charKey 'd') := makeMove LeftMove ref winPopup]

  return ()
