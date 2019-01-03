module Character where

import qualified Item

type Inventory = [Item.Item]

data Character = Null
               | Character { name :: String
                           , hp :: Float
                           , pc :: Bool
                           , items :: Inventory
                           } deriving Show


setHealth :: Float -> Character -> Character
setHealth x char = char { hp = x }

changeHealth :: Float -> Character -> Character
changeHealth x char@(Character { hp = oldhp }) = char { hp = (oldhp + x) }

setName :: String -> Character -> Character
setName newName char = char { name = newName }

setPlayerCharacter :: Bool -> Character -> Character
setPlayerCharacter x char = char { pc = x }

pickupItem :: Item.Item -> Character -> Either String Character
pickupItem item@(_, isInventory, _) char@(Character { items = oldItems })
   | not isInventory = Left "Item can't be picked up"
   | item `elem` oldItems = Left "Item already in Inventory"
   | otherwise = Right (char { items = (item:oldItems) })

dropItem :: Item.Item -> Character -> Either String Character
dropItem item char@(Character { items = oldItems })
   | not (item `elem` oldItems) = Left "Item not in Inventory"
   | otherwise = Right (char { items = (filter (/= item) oldItems) })


