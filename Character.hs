module Character where

data Attributes = Nothing | Weapon { power :: Float
                                   , range :: Int
                                   , needsAmmo :: Bool
                                   }
                          | Shield { durability :: Float }
                          | Key { room :: (Int, Int) } deriving Show
type Item = (Int, String, Attributes) -- reads as (internal reference, name, [possibly other attributes like power])

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
