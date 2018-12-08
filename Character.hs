module Character where

type Item = (Int, Integer, String, Int, [Int]) -- reads as (internal reference, unique id, name, price, [possibly other attributes like power])

data Character = Character { id :: Integer
                           , name :: String
                           , hp :: Float
                           , pc :: Bool
                           , items :: [Item]
                           } deriving Show

-- We don't need every value to be the same, only the unique id, so we can't derive Eq
instance Eq Character where
   (Character idA _ _ _ _) == (Character idB _ _ _ _) = idA == idB

setHealth :: Float -> Character -> Character
setHealth x (Character id name _ pc items) = (Character id name x pc items)

changeHealth :: Float -> Character -> Character
changeHealth x (Character id name oldhp pc items) = (Character id name (oldhp + x) pc items)

changeName :: String -> Character -> Character
changeName name (Character id _ hp pc items) = (Character id name hp pc items)

setPlayerCharacter :: Bool ->Character -> Character
setPlayerCharacter pc (Character id name hp _ items) = (Character id name hp pc items)
