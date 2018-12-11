import Data.Array
import System.IO
import System.Random
import Control.Monad.Random
import qualified Data.Map.Strict as Map
import qualified Movement as Mov
import qualified Character as Cha

{- making Pairs an instance of Random
   taken from https://stackoverflow.com/questions/26674929/haskell-no-instance-for-random-point-arising-from-a-use-of-genrandom -}
instance (Random x, Random y) => Random (x, y) where
   randomR ((lo_x, lo_y), (hi_x, hi_y)) g = ((rand_x, rand_y), g'')
      where (rand_x, g')  = randomR (lo_x, hi_x) g
            (rand_y, g'') = randomR (lo_y, hi_y) g'
   random g = ((rand_x, rand_y), g'')
      where (rand_x, g') = random g
            (rand_y, g'') = random g'

-- making Directions an instance of Random
instance Random Mov.Direction where
   randomR (low, high) gen = (toEnum . fst $ randomR (fromEnum low, fromEnum high) gen, snd $ randomR (fromEnum low, fromEnum high) gen)
   random = randomR (Mov.West, Mov.East)



{- creates weighted random lists
   taken from https://stackoverflow.com/questions/8785577/generating-random-integers-with-given-probabilities -}
weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weightList = evalRand m gen
   where m = sequence . repeat . fromList $ weightList


createMap :: IO Mov.Map -- creates a new map with randomly locked rooms
createMap = do 
   gen <- newStdGen
   let weightList = [(True, 2), (False, 6)]
   let randomBools = take Mov.numRooms (weightedList gen weightList)
   return (listArray ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) randomBools)


createCharacter :: CharMap -> IO (Int, CharMap) -- creates an empty character with a unique id - that's why we have to pass that map around!
createCharacter map = do
   gen <- newStdGen
   let id = head $ filter (\x -> not (Map.member x map)) (randoms gen :: [Int])
   let player = Cha.Character "" 100 False []
   let pos = ((0,0), (0,0), Mov.West)
   let newMap = Map.insert id (player,pos) map
   return (id, newMap)


createPlayer :: Mov.Map -> CharMap -> IO (Int, CharMap) -- creates a new player character
createPlayer roomMap charMap = do
   (id, tempMap) <- createCharacter charMap
   print "How would you like to be called?"
   name <- getLine
   print ("Very well, " ++ name ++ " it is then.")
   let (Just (char, pos)) = Map.lookup id tempMap
   let player = Cha.setPlayerCharacter True . Cha.changeName name $ char
   gen <- newStdGen
   let allRooms = take Mov.numRooms $ randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen :: [(Int,Int)] -- get a randomized list of rooms
   let newLoc = head $ filter (\x -> not (roomMap ! x)) allRooms -- get the first unlocked room in that list
   gen2 <- newStdGen
   let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen2 -- get a random Mov.InnerLocation
   gen3 <- newStdGen
   let dir = head $ randomRs (Mov.West, Mov.East) gen3 -- get a random Direction
   let newPos = (newLoc, innerLoc, dir)
   let newMap = Map.insert id (player,newPos) tempMap
   return (id, newMap)


createItem :: Mov.Map -> ItemMap -> IO (Int, ItemMap)



getCharacter :: Int -> CharMap -> Maybe Cha.Character -- just an alias for Map.lookup, but discards the position
getCharacter id map = case expr of Nothing -> Nothing
                                   Just (pl, _) -> Just pl
   where expr = Map.lookup id map

getPosition :: Int -> CharMap -> Maybe Mov.Position -- just an alias for Map.lookup, but discards the character
getPosition id map = case expr of Nothing -> Nothing
                                  Just (_, pos) -> Just pos
   where expr = Map.lookup id map

type Characters = [Int]
type CharMap = Map.Map Int (Cha.Character, Mov.Position)

type Items = [Int]
type ItemMap = Map.Map Int (Cha.Item, Mov.Position)

type Game = (Mov.Map, Characters, CharMap, Items, ItemMap)


initialize :: Game
initialize = do
   roomMap <- createMap
   (playerID, charMap1) <- createPlayer roomMap Map.empty
   let gen <- newStdGen
   let numItems = head $ randomRs (0,Mov.numRooms) gen
   items <- createItems roomMap Map.empty
   
   



-- Here, we finally get to play the game!
play :: Game -> Either String Game
play

main = do 
   roomMap <- createMap
   (playerID, charMap1) <- createPlayer roomMap Map.empty
   (charID, charMap2) <- createCharacter charMap1
   print roomMap
--   putStrLn result
