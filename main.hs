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


createCharacter :: Bool -> Mov.Map -> CharMap -> IO (Int, CharMap) -- creates an empty character with a unique id - that's why we have to pass that map around!
createCharacter checkForLocked roomMap charMap = do
   gen <- newStdGen
   let id = head $ filter (\x -> not (Map.member x charMap)) (randoms gen :: [Int])
   let char = Cha.Character "" 100 False []
   pos <- randomPosition checkForLocked roomMap
   let newMap = Map.insert id (char,pos) charMap
   return (id, newMap)


createPlayer :: Mov.Map -> CharMap -> IO (Int, CharMap) -- creates a new player character
createPlayer roomMap charMap = do
   (id, tempMap) <- createCharacter True roomMap charMap
   print "How would you like to be called?"
   name <- getLine
   print ("Very well, " ++ name ++ " it is then.")
   let (Just (char, pos)) = Map.lookup id tempMap
   let player = Cha.setPlayerCharacter True . Cha.changeName name $ char
   let newMap = Map.insert id (player,pos) tempMap
   return (id, newMap)


createItem :: Bool ->  Mov.Map -> ItemMap -> IO (Int, ItemMap)
createItem checkForLocked roomMap itemMap = do
   gen <- newStdGen
   let id = head $ filter (\x -> not (Map.member x itemMap)) (randoms gen :: [Int])
   gen2 <- newStdGen
   let itemID = head (randomRs (0,Cha.itemCount-1) gen :: [Int])
   let item = Cha.itemList !! itemID
   pos <- randomPosition checkForLocked roomMap
   let newMap = Map.insert id (item, pos) itemMap
   return (id, newMap)

createLadder :: Bool -> Mov.Map -> ItemMap -> IO (Int, ItemMap)
createLadder checkForLocked roomMap itemMap = do
  gen <- newStdGen
  let id = head $ filter (\x -> not (Map.member x itemMap)) (randoms gen :: [Int])
  gen2 <- newStdGen
  let itemID = last Cha.itemList
  let item = Cha.itemList !! itemID
  pos <- randomPosition checkForLocked roomMap
  let newMap = Map.insert id (item, pos) itemMap
  return (id, newMap)

randomPosition :: Bool -> Mov.Map -> IO Mov.Position
randomPosition checkForLocked roomMap
   | checkForLocked = do
      gen <- newStdGen
      let loc = head $ filter (\x -> not (roomMap ! x)) (randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen :: [(Int,Int)]) -- get a random unlocked room
      gen2 <- newStdGen
      let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen2 -- get a random Mov.InnerLocation
      gen3 <- newStdGen
      let dir = head $ randomRs (Mov.West, Mov.East) gen3 -- get a random Mov.Direction
      let newPos = (loc, innerLoc, dir)
      return newPos
   | otherwise = do
      gen <- newStdGen
      let loc = head $ randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen :: [(Int,Int)] -- get a random room
      gen2 <- newStdGen
      let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen2
      gen3 <- newStdGen
      let dir = head $ randomRs (Mov.West, Mov.East) gen3
      let newPos = (loc, innerLoc, dir)
      return newPos




getCharacter :: Int -> CharMap -> Maybe Cha.Character -- just an alias for Map.lookup, but discards the position
getCharacter id map = case expr of Nothing -> Nothing
                                   Just (pl, _) -> Just pl
   where expr = Map.lookup id map

getPosition :: Int -> CharMap -> Maybe Mov.Position -- just an alias for Map.lookup, but discards the character
getPosition id map = case expr of Nothing -> Nothing
                                  Just (_, pos) -> Just pos
   where expr = Map.lookup id map







type Player = Int
type CharMap = Map.Map Int (Cha.Character, Mov.Position)

type Ladder = Int
type ItemMap = Map.Map Int (Cha.Item, Mov.Position)

type Game = (Mov.Map, Player, CharMap, Ladder, ItemMap)










initialize :: IO Game
initialize = do
   roomMap <- createMap
   (playerID, charMap1) <- createPlayer roomMap Map.empty
   gen <- newStdGen
   let numItems = head $ randomRs (0,Mov.numRooms) gen
   (itemID1, itemMap1) <- createItem False roomMap Map.empty
   (itemID2, itemMap2) <- createItem False roomMap itemMap1
   (ladderID, itemMap3) <- createLadder True roomMap itemMap2
   let (Just (_, ladderPos)) = Map.lookup ladderID itemMap3
   let itemMap4 = Map.insert ladderID (last Cha.itemList, ladderPos)
   return (roomMap, playerID, charMap1, ladderID, itemMap4)


gameState :: Game -> IO String
gameState game = do
   resultUnformatted <- play game
   case resultUnformatted of
      (Left str) -> return str
      (Right game2) -> gameState game2


-- Here, we finally get to play the game!
play :: Game -> IO (Either String Game)
play game = do
   print "What do you want to do next?"
   input <- getLine
   action input game


action :: String -> Game -> IO (Either String Game)
action str game@(roomMap, playerID, charMap, ladderID, itemMap)
   | str == "go forward" = do
      let (Just player, pos) = Map.lookup playerID charMap
      let (Just _, ladderPos@(ladderRoom, ladderInner, ladderDir)) = Map.lookup ladderID itemMap
      let resultUnformatted = Mov.move pos Advance
      case resultUnformatted of
         (Left str) -> do
            case str of str
               | str == "Wall" -> print getWallMsg
               | str == "Door blocked" -> print getDoorBlockedMsg
            return $ Right game
         (Right newPos@(room, inner@(x,y), dir))
            | roomMap ! room -> do
               print getRoomLockedMsg
               return $ Right game
            | (ladderRoom == room) && (ladderInner == inner) ->
               if Mov.isCorner inner then return $ Left "Idiot!"
               else case (x,y) of (x,y)
                  | x == 0 && Mov.isDoor y -> 
                  let
                  return $ Left "Bla"
            | otherwise -> do
               let newCharMap = Map.insert playerID (player, newPos) charMap
               return $ Right (roomMap, playerID, newCharMap, ladderID, itemMap)





main = do
   game <- initialize
   state <- gameState game
   putStrLn state
