module Item where

--import System.Random (Random(..), RandomGen)
import qualified Movement as Mov


-- | Set up type and attributes an item could have.
data Attributes = Nil | Weapon { power :: Float
                               , range :: Int
                               }
                      | Shield { durability :: Float }
                      | Key { room :: Mov.Location }
                      | Ladder deriving (Show, Read, Eq)

-- TODO: Decide if we're going to produce random items
{-
instance Random Attributes where
    randomR (lowAttr, highAttr) gen
        | low < 1 || high > 3 = error "Out of range!"
        | otherwise = let (tempRand, gen') = randomR (low :: Int, high) gen
                          (randPower, tempGenWeapon) = randomR (3 :: Float, 50) gen'
                          (randRange, genWeapon) = randomR (1, max Mov.highInnerBoundNS Mov.highInnerBoundWE) tempGenWeapon
                          (randDur, genShield) = randomR (15 :: Float, 100) gen'
                          (randNS, tempGenKey) = randomR (Mov.lowBoundNS, Mov.highBoundNS) gen'
                          (randWE, genKey) = randomR (Mov.lowBoundWE, Mov.highBoundWE) tempGenKey
                      in case tempRand of
                          1 -> (Weapon randPower randRange, genWeapon)
                          2 -> (Shield randDur, genShield)
                          3 -> (Key (randNS, randWE), genKey)
      where
        low = case lowAttr of
            (Weapon _ _) -> 1
            (Shield _) -> 2
            (Key _) -> 3
        high = case highAttr of
            (Weapon _ _) -> 1
            (Shield _) -> 2
            (Key _) -> 3


    random = randomR (Weapon 0 0, Key (0,0))

-}




-- reads as (name, isInventory, type and further attributes)
type Item = (String, Bool, Attributes)


-- | Returns the name of the item.
name :: Item -> String
name (str, _, _) = str


{-
randomItem :: RandomGen g => String -> g -> (Item, g)
randomItem itemName gen = let (randAttr, newGen) = random gen
                          in ((itemName, True, randAttr), newGen)
-}


{-|
 - Returns the damage a character produces
 - if he doesn't have a weapon equipped.
 -}
fistDmg :: Float
fistDmg = 2.5 -- TODO: Maybe set this in the configuration file


{-|
 - Adds the given amount to a shield's durability,
 - errors if the given item isn't a shield.
 -}
addDur :: Float -> Item -> Item
addDur x (itemName, isInv, Shield oldDur) = (itemName, isInv, Shield (oldDur + x))
addDur _ item = error $ "Not a shield: " ++ show item


-- | Reduces a shield's durability by the given amount. (see changeDur)
reduceDur :: Float -> Item -> Item
reduceDur x = addDur (-x)


{-
 - Checks if the shield's durability is 0 or less,
 - gives a default of False for all other items.
 -}
isDestroyed :: Item -> Bool
isDestroyed (_, _, Shield dur) = dur <= 0
isDestroyed item = False


-- | Checks if the given item is a weapon.
isWeapon :: Item -> Bool
isWeapon (_, _, Weapon _ _) = True
isWeapon _ = False


-- | Checks if the given item is a shield.
isShield :: Item -> Bool
isShield (_, _, Shield _) = True
isShield _ = False


-- | Checks if the given item is a key.
isKey :: Item -> Bool
isKey (_, _, Key _) = True
isKey _ = False


-- | Creates a new key for the given location.
genKey :: Mov.Location -> Item
genKey loc = ("Key", True, Key loc)


-- | This should be used instead of creating your own ladder.
ladder :: Item
ladder = ("Ladder", False, Ladder)



-- | Lists a few default weapons.
weaponList :: [Item]
weaponList = [("Side sword", True, Weapon 10 1)
             ,("Longsword", True, Weapon 15 2)
             ,("Bow", True, Weapon 10 maxRange)
             ,("Club", True, Weapon 5.5 1)
             ]
  where
    maxRange = max Mov.highInnerBoundNS Mov.highInnerBoundWE


-- | Lists a few default shields.
shieldList :: [Item]
shieldList = [("Oakenshield", True, Shield 20)
             ,("Iron Shield", True, Shield 40)
             ]


-- | Lists all weapons and shields.
invItemList :: [Item]
invItemList = shieldList ++ weaponList
