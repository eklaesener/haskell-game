module Draw where

import Movement

type DrawList = [((Int, Int),String)]



dot :: String
dot = " . "


player :: Direction -> String
player North = " ⭡ "
player East = " ⭢ "
player South = " ⭣ "
player West = " ⭠ "


ladder :: String
ladder = " H "


win :: String
win = " X "


draw :: DrawList -> IO ()
draw list = mapM_ putStrLn $ drawing list
   where drawing (x:xs) = 
