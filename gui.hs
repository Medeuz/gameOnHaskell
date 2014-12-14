import Graphics.UI.WX

main :: IO ()
main = start gui

gui :: IO ()
gui = do
  -- форма, на которой будут лежать все наши контролы
  wnd <- frame [ text := "Пятнашки", size := sz 300 300, bgcolor := blue ]
  
  -- несколько кнопок фиксированного размера
  --которые отображают наше поле
  button1 <- button wnd [ text := "1"] 
  button2 <- button wnd [ text := "2", clientSize := sz 25 25 ]
  button3 <- button wnd [ text := "3", clientSize := sz 25 25 ]
  button4 <- button wnd [ text := "4", clientSize := sz 25 25 ]
  button5 <- button wnd [ text := "5", clientSize := sz 25 25 ]
  button6 <- button wnd [ text := "6", clientSize := sz 25 25 ]
  button7 <- button wnd [ text := "7", clientSize := sz 25 25 ]
  button8 <- button wnd [ text := "8", clientSize := sz 25 25 ]
  button9 <- button wnd [ text := "9", clientSize := sz 25 25 ]
  button10 <- button wnd [ text := "10", clientSize := sz 25 25 ]
  button11 <- button wnd [ text := "11", clientSize := sz 25 25 ]
  button12 <- button wnd [ text := "12", clientSize := sz 25 25 ]
  button13 <- button wnd [ text := "13", clientSize := sz 25 25 ]
  button14 <- button wnd [ text := "14", clientSize := sz 25 25 ]
  button15 <- button wnd [ text := "15", clientSize := sz 25 25 ]
  button16 <- button wnd [ text := "", clientSize := sz 25 25 ]
 --задаем действия по клику на кнопку
  --set button1 [ on (charKey 'S') := 
		 --set button1 [ text := if (get button5 text) /= "" then (get button5 text) else ""] 
		 --set button5 [ text := if btnText /= "" then "" else btnText]
		 
  -- прикрепл¤ем контролы к окну
  set wnd [
    layout := 
      column 3 [
        {- 1  -}
        margin 3 $ row 3 [
		  widget button1,
		  widget button2,
		  widget button3,
		  widget button4
        ],      
        {- 2  -}
        margin 3 $ row 3 [
          widget button5,
		  widget button6,
		  widget button7,
		  widget button8
        ],
		{- 3 -}
        margin 3 $ row 3 [
          widget button9,
		  widget button10,
		  widget button11,
		  widget button12
        ],
		{- 4 -}
        margin 3 $ row 3 [
          widget button13,
		  widget button14,
		  widget button15,
		  widget button16
        ]
      ]
    ]
  return ()