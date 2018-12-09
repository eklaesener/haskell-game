module Character where

type Item = (Int, String, Int, [Int]) -- reads as (internal reference, name, price, [possibly other attributes like power])

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
