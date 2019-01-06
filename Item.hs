module Item where

import System.Random
import qualified Movement as Mov

data Attributes = Nil | Weapon { power :: Float
                               , range :: Int
                               }
                      | Shield { durability :: Float }
                      | Key { room :: (Int, Int) }
                      | Ladder deriving (Show, Eq)

instance Random Attributes where
   randomR (lowAttr, highAttr) gen
      | low < 1 || high > 3 = error "Out of range!"
      | otherwise = let (tempRand, gen') = randomR (low :: Int, high) gen
                        (randPower, tempGenWeapon) = randomR (3 :: Float, 50) gen'
                        (randRange, genWeapon) = randomR (1, Mov.roomSize) tempGenWeapon
                        (randDur, genShield) = randomR (15 :: Float, 100) gen'
                        (randNS, tempGenKey) = randomR (Mov.lowBoundNS, Mov.highBoundNS) gen'
                        (randWE, genKey) = randomR (Mov.lowBoundWE, Mov.highBoundWE) tempGenKey
                    in case tempRand of
                       1 -> (Weapon randPower randRange, genWeapon)
                       2 -> (Shield randDur, genShield)
                       3 -> (Key (randNS, randWE), genKey)
     where low = case lowAttr of
              (Weapon _ _) -> 1
              (Shield _) -> 2
              (Key _) -> 3
           high = case highAttr of
              (Weapon _ _) -> 1
              (Shield _) -> 2
              (Key _) -> 3


   random = randomR (Weapon 0 0, Key (0,0))




-- reads as (name, isInventory, [possibly other attributes like power])
type Item = (String, Bool, Attributes)


randomItem :: RandomGen g => String -> g -> Item
randomItem name gen = let (randAttr, _) = random gen
                      in (name, True, randAttr)


-- the damage a character produces if he doesn't have a weapon equipped
fistDmg :: Float
fistDmg = 2.5


-- modifies shields' durability, errors if the item isn't a shield
changeDur :: Float -> Item -> Item
changeDur x (name, isInv, Shield oldDur) = (name, isInv, Shield (oldDur + x))
changeDur _ item = error $ "Not a shield: " ++ show item


-- reduces shield's durability
reduceDur :: Float -> Item -> Item
reduceDur x = changeDur (-x)


-- checks if the shield's durability is 0 or less, errors if the item isn't a shield
isDestroyed :: Item -> Bool
isDestroyed (_, _, Shield dur)
   | dur <= 0 = True
   | otherwise = False
isDestroyed item = error $ "Not a shield: " ++ show item


isWeapon :: Item -> Bool
isWeapon (_, _, Weapon _ _) = True
isWeapon _ = False


isShield :: Item -> Bool
isShield (_, _, Shield _) = True
isShield _ = False


isKey :: Item -> Bool
isKey (_, _, Key _) = True
isKey _ = False


ladder :: Item
ladder = ("Ladder", False, Ladder)


keyList :: [Item]
keyList = [("Key", True, Key (0,0))
          ,("Key", True, Key (0,1))
          ,("Key", True, Key (0,2))
          ,("Key", True, Key (0,3))
          ,("Key", True, Key (1,0))
          ,("Key", True, Key (1,1))
          ,("Key", True, Key (1,2))
          ,("Key", True, Key (1,3))
          ,("Key", True, Key (2,0))
          ,("Key", True, Key (2,1))
          ,("Key", True, Key (2,2))
          ,("Key", True, Key (2,3))
          ]

weaponList :: [Item]
weaponList = [("Side sword", True, Weapon 10 1)
             ,("Longsword", True, Weapon 15 2)
             ,("Bow", True, Weapon 10 Mov.roomSize)
             ,("Club", True, Weapon 5.5 1)
             ]

shieldList :: [Item]
shieldList = [("Oakenshield", True, Shield 20)
             ,("Iron Shield", True, Shield 60)
             ]

invItemList :: [Item]
invItemList = shieldList ++ weaponList ++ keyList
