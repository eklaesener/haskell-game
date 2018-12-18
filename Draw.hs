module Draw where

import Movement

type InputList = [((Int, Int),String)]
type DrawList = [String]



dot :: String
dot = " . "

door :: String
door = " ▯ "

doorList :: [(InnerLocation, String)]
doorList = [((x,y), door) 
           | x <- [0 .. roomSize]
           , y <- [0 .. roomSize]
           , isDoorFull (x,y)]
filterDoorList :: InnerLocation -> [(InnerLocation, String)]
filterDoorList (a,b) = [((x,y), door)
                       | x <- [0 .. roomSize]
                       , y <- [0 .. roomSize]
                       , (x,y) /= (a,b)]


player :: Direction -> String
player North = " ⭡ "
player East = " > "
player South = " ⭣ "
player West = " < "


ladder :: String
ladder = " H "


win :: String
win = " X "


draw :: InputList -> IO ()
draw list = helper (drawing 0 (cleanList list))
   where
      helper [] = putStrLn ""
      helper (x:xs) = (putStrLn . filter (\ch -> ch /= '\n') . unlines $ x) >> helper xs



cleanList :: InputList -> DrawList
cleanList [] = []
cleanList ((_, str):rest) = str : cleanList rest

drawing :: Int -> DrawList -> [DrawList]
drawing count list
            | count == roomSize = [list]
            | otherwise = let (comp1, comp2) = splitAt (roomSize + 1) list
                          in comp1 : drawing (count + 1) comp2
