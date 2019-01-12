module Character where

import qualified Item
import qualified Movement as Mov

-- reads as [(Item, equipped)]
type Inventory = [(Item.Item, Bool)]

data Character = Character { name :: String
                           , hp :: Float
                           , pc :: Bool
                           , inv :: Inventory
                           } deriving (Show, Eq)


nullCharacter :: Character
nullCharacter = Character "" 0 False []



setHealth :: Float -> Character -> Character
setHealth x char = char { hp = x }


changeHealth :: Float -> Character -> Character
changeHealth x char@Character { hp = oldHP } = char { hp = oldHP + x }


reduceHealth :: Float -> Character -> Character
reduceHealth x = changeHealth (-x)


setName :: String -> Character -> Character
setName newName char = char { name = newName }


setPlayerCharacter :: Bool -> Character -> Character
setPlayerCharacter x char = char { pc = x }


setEquipped :: Bool -> Item.Item -> Character -> Character
setEquipped val item char@Character { inv = oldInv } = char { inv = newInv }
  where list = filter (\(x,y) -> x == item && y /= val) oldInv
        newInv = if null list
                 then error $ "Character " ++ show char ++ " doesn't have an unequipped " ++ show item ++ "!"
                 else (item, val) : filter (/= head list) oldInv


isDead :: Character -> Bool
isDead Character { hp = currHP }
   | currHP <= 0 = True
   | otherwise = False


-- checks if the given character has a key unlocking the given room
hasKey :: Mov.Location -> Character -> Bool
hasKey room char@Character { inv = currInv } = if numKeys char < 1
                                               then False
                                               else (room `elem`) . map ((\(_, _, Item.Key x) -> x) . fst) $ currInv

weaponList :: Character -> Inventory
weaponList Character { inv = currInv } = filter (\(x, _) -> Item.isWeapon x) currInv


-- how many weapons the character has
numWeapons :: Character -> Int
numWeapons = length . weaponList


-- which weapon, if any, is currently equipped
equippedWeapon :: Character -> Maybe Item.Item
equippedWeapon Character { inv = currInv } = if null list
                                             then Nothing
                                             else Just $ head list
  where list = filter Item.isWeapon . map fst . filter snd $ currInv


shieldList :: Character -> Inventory
shieldList Character { inv = currInv } = filter (\(x, _) -> Item.isShield x) currInv


-- how many shields the character has
numShields :: Character -> Int
numShields = length . shieldList


-- which shield, if any, is currently equipped
equippedShield :: Character -> Maybe Item.Item
equippedShield Character { inv = currInv } = if null list
                                             then Nothing
                                             else Just $ head list
  where list = filter Item.isShield . map fst . filter snd $ currInv


keyList :: Character -> Inventory
keyList Character { inv = currInv } = filter (\(x, _) -> Item.key x) currInv

-- how many keys the character has
numKeys :: Character -> Int
numKeys = length . keyList

-- which key, if any, is currently equipped
equippedKey :: Character -> Maybe Item.Item
equippedKey Character { inv = currInv } = if null list
                                          then Nothing
                                          else Just $ head list
  where list = filter Item.isKey . map fst . filter snd $ currInv


-- changes an item in the character's inventory
modifyInv :: Bool -> Item.Item -> Item.Item -> Character -> Character
modifyInv isEquipped oldItem newItem char@Character { inv = oldInv }
   | null list = error $ "Old Item " ++ show oldItem ++ " not in " ++ name char ++ "'s inventory"
   | otherwise = pickupItem isEquipped newItem . dropItem oldItem $ char
  where list = filter (\(x, _) -> x == oldItem) oldInv


pickupItem :: Bool -> Item.Item -> Character -> Character
pickupItem isEquipped item@(_, isInventory, _) char@Character { inv = oldInv }
   | not isInventory = error "Item can't be picked up"
--   | item `elem` oldItems = error "Item already in Inventory"
   | otherwise = char { inv = (item, isEquipped) : oldInv }
--  where (oldItems, _) = unzip oldInv


dropItem :: Item.Item -> Character -> Character
dropItem item char@Character { inv = oldInv }
   | item `notElem` oldItems = error "Item not in Inventory"
   | otherwise = char { inv = filter (\(x, _) -> x /= item) oldInv }
  where (oldItems, _) = unzip oldInv


