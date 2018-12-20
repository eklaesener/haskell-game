module Draw where

import Movement

type InputList = [((Int, Int),String)]
type DrawList = [String]



-- generates the full list of doors
doorList :: [(InnerLocation, String)]
doorList = [((x,y), door) 
           | x <- [0 .. roomSize]
           , y <- [0 .. roomSize]
           , isDoorFull (x,y)]

-- filters out those doors that overlap with the player, ladder or win positions
filterDoorList :: [InnerLocation] -> [(InnerLocation, String)]
filterDoorList [(a,b)] = 
   [((x,y), door)
   | x <- [0 .. roomSize]
   , y <- [0 .. roomSize]
   , (x,y) /= (a,b)
   , isDoorFull (x,y)]
filterDoorList [(a,b), (c,d)] = 
   [((x,y), door)
   | x <- [0 .. roomSize]
   , y <- [0 .. roomSize]
   , (x,y) /= (a,b)
   , (x,y) /= (c,d)
   , isDoorFull (x,y)]
filterDoorList [(a,b), (c,d), (e,f)] = 
   [((x,y), door)
   | x <- [0 .. roomSize]
   , y <- [0 .. roomSize]
   , (x,y) /= (a,b)
   , (x,y) /= (c,d)
   , (x,y) /= (e,f)
   , isDoorFull (x,y)]



dot :: String
dot = " . "


door :: String
door = " ▯ "


player :: Direction -> String
player North = " ⯅ "
player East = " ⯈ "
player South = " ⯆ "
player West = " ⯇ "


ladder :: String
ladder = " ☷ "


win :: String
win = " ⭙ "



-- draws the room
draw :: InputList -> IO ()
draw list = helper . drawing 0 $ cleanList list
   where
      -- takes one element out of the [DrawList] (one row), compresses the strings into one with unlines, filters out the newlines the unlines call has generated, prints that string to the command line, and recursively calls itself with the rest of the [DrawList]
      helper [] = putStrLn ""
      helper (x:xs) = (putStrLn . filter (\ch -> ch /= '\n') . unlines $ x) >> helper xs


-- discards the coordinates, since we only needed them for sorting
cleanList :: InputList -> DrawList
cleanList [] = []
cleanList ((_, str):rest) = str : cleanList rest

-- takes a cleaned up list of strings and splits it in lists of roomSize
drawing :: Int -> DrawList -> [DrawList]
drawing count list
            | count == roomSize = [list]
            | otherwise = let (comp1, comp2) = splitAt (roomSize + 1) list
                          in comp1 : drawing (count + 1) comp2
