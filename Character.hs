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


reduceHealth :: Float -> Character -> Character
reduceHealth x = changeHealth (-x)


setName :: String -> Character -> Character
setName newName char = char { name = newName }


setPlayerCharacter :: Bool -> Character -> Character
setPlayerCharacter x char = char { pc = x }




isDead :: Character -> Bool
isDead Character { hp = currHP }
   | currHP <= 0 = True
   | otherwise = False


-- which weapon, if any, is currently equipped
equippedWeapon :: Character -> Maybe Item.Item
equippedWeapon Character { inv = currInv } = if null list
                                             then Nothing
                                             else Just $ head list
  where list = filter Item.isWeapon . map fst . filter snd $ currInv


-- which shield, if any, is currently equipped
equippedShield :: Character -> Maybe Item.Item
equippedShield Character { inv = currInv } = if null list
                                             then Nothing
                                             else Just $ head list
  where list = filter Item.isShield . map fst . filter snd $ currInv


-- changes an item in the character's inventory
modifyInv :: Bool -> Item.Item -> Item.Item -> Character -> Character
modifyInv isEquipped oldItem newItem char@Character { inv = oldInv }
   | null list = error $ "Old Item " ++ show oldItem ++ " not in " ++ name char ++ "'s inventory"
   | otherwise = pickupItem isEquipped newItem . dropItem oldItem $ char
  where list = filter (\(x, _) -> x == oldItem) oldInv


pickupItem :: Bool -> Item.Item -> Character -> Character
pickupItem isEquipped item@(_, isInventory, _) char@Character { inv = oldInv }
   | not isInventory = error "Item can't be picked up"
   | item `elem` oldItems = error "Item already in Inventory"
   | otherwise = char { inv = (item, isEquipped) : oldInv }
  where (oldItems, _) = unzip oldInv


dropItem :: Item.Item -> Character -> Character
dropItem item char@Character { inv = oldInv }
   | item `notElem` oldItems = error "Item not in Inventory"
   | otherwise = char { inv = filter (\(x, _) -> x /= item) oldInv }
  where (oldItems, _) = unzip oldInv


