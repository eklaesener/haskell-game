module Draw where

import Movement

type InputList = [(InnerLocation,String)]
type DrawList = [String]



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


weapon :: String
weapon = " W "

shield :: String
shield = " S "

key :: String
key = " K "


enemy :: String -> String
enemy "Berserker" = " B "
enemy "Cave Troll" = " C "
enemy "Ghoul" = " G "
enemy "Hobgoblin" = " H "
enemy "Orc" = " O "
enemy "Wraith" = " R "



-- used for sorting the InputList
insert :: (InnerLocation, String) -> InputList -> InputList
insert a [] = [a]
insert a@((x1, y1), _) (b@((x2, y2), _) : rest)
   | x2 < x1 = b : insert a rest
   | x1 < x2 = a : b : rest
   | y2 < y1 = b : insert a rest
   | otherwise = a : b : rest


-- generates the full list of doors
doorList :: InputList
doorList = [((x,y), door)
           | x <- [0 .. roomSize]
           , y <- [0 .. roomSize]
           , isDoorFull (x,y)
           ]

-- filters out those doors that overlap with any other positions
filterDoorList :: [InnerLocation] -> InputList
filterDoorList = filterHelper doorList


-- generates a room full of dots, but filters out the doors (since they stay the same each time)
dotList :: InputList
dotList = [((x,y), dot)
          | x <- [0 .. roomSize]
          , y <- [0 .. roomSize]
          , not $ isDoorFull (x,y)
          ]

--filters out those dots that overlap with any other positions
filterDotList :: [InnerLocation] -> InputList
filterDotList = filterHelper dotList

-- filters out all occurrences of elements in the second list from the first list
filterHelper :: InputList -> [InnerLocation] -> InputList
filterHelper = foldl (\acc x -> filter (\(a, _) -> x /= a) acc)




-- draws the room
draw :: InputList -> IO ()
draw list = helper . drawing 0 . cleanList . foldr insert (filterDotList tempList) $ filterDoorList tempList ++ list
  where
   tempList = map fst list
   -- takes one element out of the [DrawList] (one row), compresses the strings into one with unlines, filters out the newlines the unlines call has generated, prints that string to the command line, and recursively calls itself with the rest of the [DrawList]
   helper = foldr (\x -> (>>) (putStrLn . filter (/= '\n') . unlines $ x)) (putStrLn "")


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
