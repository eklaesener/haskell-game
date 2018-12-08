module Character where

type Item = (Int, String, Int, [Int]) -- reads as (unique id, name, price, [possibly other attributes like power])

data Character = Character { id :: Integer
                           , name :: String
                           , hp :: Float
                           , pc :: Bool
                           , items :: [Item]
                           } deriving Show

-- We don't need every value to be the same, only the unique id, so we can't derive Eq
instance Eq Character where
   (Character idA _ _ _ _) == (Character idB _ _ _ _) = idA == idB

setHealth :: Character -> Float -> Character
setHealth (Character id name _ pc items) x = (Character id name x pc items)

changeHealth :: Character -> Float -> Character
changeHealth (Character id name oldhp pc items) x = (Character id name (oldhp + x) pc items)


