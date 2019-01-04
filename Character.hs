module Character where

import qualified Item

-- reads as [(Item, equipped)]
type Inventory = [(Item.Item, Bool)]

data Character = Null
               | Character { name :: String
                           , hp :: Float
                           , pc :: Bool
                           , inv :: Inventory
                           } deriving Show


setHealth :: Float -> Character -> Character
setHealth x char = char { hp = x }

changeHealth :: Float -> Character -> Character
changeHealth x char@Character { hp = oldhp } = char { hp = oldhp + x }

setName :: String -> Character -> Character
setName newName char = char { name = newName }

setPlayerCharacter :: Bool -> Character -> Character
setPlayerCharacter x char = char { pc = x }

pickupItem :: Item.Item -> Character -> Either String Character
pickupItem item@(_, isInventory, _) char@Character { inv = oldInv }
   | not isInventory = Left "Item can't be picked up"
   | item `elem` oldItems = Left "Item already in Inventory"
   | otherwise = Right (char { inv = (item, False) : oldInv })
  where (oldItems, _) = unzip oldInv

dropItem :: Item.Item -> Character -> Either String Character
dropItem item char@Character { inv = oldInv }
   | item `notElem` oldItems = Left "Item not in Inventory"
   | otherwise = Right (char { inv = filter (\(x, _) -> x /= item) oldInv })
  where (oldItems, _) = unzip oldInv


