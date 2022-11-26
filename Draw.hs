module Draw where

import Movement
import qualified Item 
import qualified Character as Cha

import qualified Debug.Trace as Trace


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


itemList :: [String]
itemList =
    [" 🗡️ "
    ," 🛡 "
    ," 🔑 "
    ]

weapon :: String
weapon = " 🗡️ "

shield :: String
shield = " 🛡 "

key :: String
key = " 🔑 "


enemyList :: [String]
enemyList =
    [" ᕕ "
    ," ᕗ "
    ," ᕓ "
    ," ᕙ "
    ," ᘯ "
    ," ᘰ "
    ," ᘮ "
    ," ᘳ "
    ," ᘺ "
    ," ᘿ "
    ," ᘻ "
    ," ᘼ "
    ," ᗑ "
    ," ᗒ "
    ," ᗐ "
    ," ᗕ "
    ," ᕱ "
    ," ᕲ "
    ," ᕰ "
    ," ᕳ "
    ," ᗅ "
    ," ᗆ "
    ," ᗄ "
    ," ᗉ "
    ]

enemy :: String -> Direction -> String
enemy "Berserker" North = " ᕕ "
enemy "Berserker" East = " ᕗ "
enemy "Berserker" South = " ᕓ "
enemy "Berserker" West = " ᕙ "

enemy "Cave Troll" North = " ᘯ "
enemy "Cave Troll" East = " ᘰ "
enemy "Cave Troll" South = " ᘮ "
enemy "Cave Troll" West = " ᘳ "

enemy "Ghoul" North = " ᘺ "
enemy "Ghoul" East = " ᘿ "
enemy "Ghoul" South = " ᘻ "
enemy "Ghoul" West = " ᘼ "

enemy "Hobgoblin" North = " ᗑ "
enemy "Hobgoblin" East = " ᗒ "
enemy "Hobgoblin" South = " ᗐ "
enemy "Hobgoblin" West = " ᗕ "

enemy "Orc" North = " ᕱ "
enemy "Orc" East = " ᕲ "
enemy "Orc" South = " ᕰ "
enemy "Orc" West = " ᕳ "

enemy "Wraith" North = " ᗅ "
enemy "Wraith" East = " ᗆ "
enemy "Wraith" South = " ᗄ "
enemy "Wraith" West = " ᗉ "



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
           | x <- [lowInnerBoundNS .. highInnerBoundNS]
           , y <- [lowInnerBoundWE .. highInnerBoundWE]
           , isDoorFull (x,y)
           ]

-- filters out those doors that overlap with any other positions
filterDoorList :: [InnerLocation] -> InputList
filterDoorList = filterHelper doorList


-- generates a room full of dots, but filters out the doors (since they stay the same each time)
dotList :: InputList
dotList = [((x,y), dot)
          | x <- [lowInnerBoundNS .. highInnerBoundNS]
          , y <- [lowInnerBoundWE .. highInnerBoundWE]
          , not $ isDoorFull (x,y)
          ]

--filters out those dots that overlap with any other positions
filterDotList :: [InnerLocation] -> InputList
filterDotList = filterHelper dotList

-- filters out all occurrences of elements in the second list from the first list
filterHelper :: InputList -> [InnerLocation] -> InputList
filterHelper = foldl (\acc x -> filter (\(a, _) -> x /= a) acc)


-- if any two positions clash, take the one with the higher priority
filterClashes :: InputList -> InputList
filterClashes [] = []
filterClashes ((pos, str) : rest)
    | str `elem` [" ⯅ ", " ⯈ ", " ⯆ ", " ⯇ "] = expr
    | str == " ☷ " = expr
    | str `elem` enemyList = expr
    | str `elem` itemList = expr
    | otherwise = expr
  where
    expr = (pos, str) : filterClashes (filter (\(x, _) -> x /= pos) rest)



-- displays the current status like hp, weapons and so on
drawStats :: Cha.Character -> DrawList
drawStats char@(Cha.Character charName hp _ currInv) = ["Your name: " ++ charName, "Your health: " ++ show hp ++ " HP", weaponStats, "Other weapons: " ++ weapons, shieldStats, "Other shields: " ++ shields, "Your keys: " ++ show keys, ""]
  where
    (weaponName, weaponDmg, weaponRange) = case Cha.equippedWeapon char of
        Nothing -> ("Fists", Item.fistDmg, 1)
        Just (str, _, Item.Weapon dmg rng) -> (str, dmg, rng)
    (shieldName, shieldDur) = case Cha.equippedShield char of
        Nothing -> ("No shield", 0)
        Just (str, _, Item.Shield dur) -> (str, dur)
    keys = map (\((_, _, Item.Key room), _) -> room) (Cha.keyList char)
    weaponStats = "Your weapon: " ++ weaponName ++ " Damage: " ++ show weaponDmg ++ " Range: " ++ show weaponRange
    weapons = unwords . map Item.name . filter Item.isWeapon . map fst . filter (not . snd) $ currInv
    shieldStats = "Your shield: " ++ shieldName ++ "  Remaining strength: " ++ show shieldDur
    shields = unwords . map Item.name . filter Item.isShield . map fst . filter (not . snd) $ currInv





-- draws the room
draw :: String -> Cha.Character -> InputList -> IO ()
draw narrStr char list = helper . drawing 0 narrStr (drawStats char) . cleanList . filterClashes . foldr insert (filterDotList tempList) $ filterDoorList tempList ++ list
  where
    tempList = map fst list
    -- takes one element out of the [DrawList] (one row), compresses the strings into one with unlines, filters out the newlines the unlines call has generated, prints that string to the command line, and recursively calls itself with the rest of the [DrawList]
    helper = foldr (\x -> (>>) (putStrLn . filter (/= '\n') . unlines $ x)) (putStrLn "")


-- discards the coordinates, since we only needed them for sorting
cleanList :: InputList -> DrawList
cleanList [] = []
cleanList ((_, str):rest) = str : cleanList rest

-- takes a cleaned up list of strings, the narrator string and the stats and splits them in lists of highInnerBoundWE
drawing :: Int -> String -> DrawList -> DrawList -> [DrawList]
drawing count narrStr stats list
    | count == highInnerBoundNS - 1 = [list ++ ["   ", narrStr]]
    | otherwise = let (comp1, comp2) = splitAt (highInnerBoundWE - lowInnerBoundWE + 1) list
                  in (comp1 ++ ["   ", stats !! min count (length stats - 1)]) : drawing (count + 1) narrStr stats comp2
