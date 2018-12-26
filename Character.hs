module Character where

import qualified Item

type Inventory = [Item.Item]

data Character = Character { name :: String
                           , hp :: Float
                           , pc :: Bool
                           , items :: Inventory
                           } deriving Show


setHealth :: Float -> Character -> Character
setHealth x (Character name _ pc items) = Character name x pc items

changeHealth :: Float -> Character -> Character
changeHealth x (Character name oldhp pc items) = Character name (oldhp + x) pc items

setName :: String -> Character -> Character
setName name (Character _ hp pc items) = Character name hp pc items

setPlayerCharacter :: Bool -> Character -> Character
setPlayerCharacter pc (Character name hp _ items) = Character name hp pc items

pickupItem :: Item.Item -> Character -> Either String Character
pickupItem item@(_, isInventory, _) (Character name hp pc oldItems)
   | not isInventory = Left "Item can't be picked up"
   | item `elem` oldItems = Left "Item already in Inventory"
   | otherwise = Right $ Character name hp pc (item:oldItems)

dropItem :: Item.Item -> Character -> Either String Character
dropItem item (Character name hp pc oldItems)
   | not (item `elem` oldItems) = Left "Item not in Inventory"
   | otherwise = Right $ Character name hp pc (filter (/= item) oldItems)


