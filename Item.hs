module Item where

import qualified Movement as Mov

data Attributes = Nil | Weapon { power :: Float
                               , range :: Int
                               }
                      | Shield { durability :: Float }
                      | Ladder
                      | Key { room :: (Int, Int) } deriving (Show, Eq)


-- reads as (name, isInventory, [possibly other attributes like power])
type Item = (String, Bool, Attributes)


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
