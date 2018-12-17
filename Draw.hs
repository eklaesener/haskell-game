module Draw where

type DrawList = [((Int, Int),String)]



dot :: String
dot = " . "


player :: String
player = " 0 "


ladder :: String
ladder = " H "


win :: String
win = " X "


draw :: DrawList -> IO ()
draw (((x,y), str):rest) = 
