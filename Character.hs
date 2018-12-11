module Character where

data Attributes = Nil | Weapon { power :: Float
                                   , range :: Int
                                   , needsAmmo :: Bool
                                   }
                          | Shield { durability :: Float }
                          | Key { room :: (Int, Int) } deriving Show

type Item = (Int, String, Attributes) -- reads as (internal reference, name, [possibly other attributes like power])

itemList :: [Item]
itemList = [(0, "Test", Nil)
           ,(1, "Key for Room (0,0)", Key (0,0))
           ,(2, "Key for Room (0,1)", Key (0,1))
           ,(3, "Key for Room (0,2)", Key (0,2))
           ,(4, "Key for Room (0,3)", Key (0,3))
           ,(5, "Key for Room (1,0)", Key (1,0))
           ,(6, "Key for Room (1,1)", Key (1,1))
           ,(7, "Key for Room (1,2)", Key (1,2))
           ,(8, "Key for Room (1,3)", Key (1,3))
           ,(9, "Key for Room (2,0)", Key (2,0))
           ,(10, "Key for Room (2,1)", Key (2,1))
           ,(11, "Key for Room (2,2)", Key (2,2))
           ,(12, "Key for Room (2,3)", Key (2,3))
           ]

itemCount :: Int
itemCount = length itemList

data Character = Character { name :: String
                           , hp :: Float
                           , pc :: Bool
                           , items :: [Item]
                           } deriving Show


setHealth :: Float -> Character -> Character
setHealth x (Character name _ pc items) = (Character name x pc items)

changeHealth :: Float -> Character -> Character
changeHealth x (Character name oldhp pc items) = (Character name (oldhp + x) pc items)

changeName :: String -> Character -> Character
changeName name (Character _ hp pc items) = (Character name hp pc items)

setPlayerCharacter :: Bool -> Character -> Character
setPlayerCharacter pc (Character name hp _ items) = (Character name hp pc items)
