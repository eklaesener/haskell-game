import Data.Array
import Data.Char
import System.IO
import System.Exit
import System.Random
import qualified Control.Monad.Random as Rand
import qualified Data.Map.Strict as Map
import qualified Movement as Mov
import qualified Character as Cha
import qualified Draw
import qualified Item

-- making Pairs an instance of Random
instance (Random x, Random y) => Random (x, y) where
   randomR ((lowX, lowY), (highX, highY)) gen = ((randX, randY), gen'')
      where (randX, gen')  = randomR (lowX, highX) gen
            (randY, gen'') = randomR (lowY, highY) gen'
   random gen = ((randX, randY), gen'')
      where (randX, gen') = random gen
            (randY, gen'') = random gen'


--


-- creates weighted random lists to create a map where comparatively few rooms are locked
weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weightList = Rand.evalRand m gen
   where m = sequence . repeat . Rand.fromList $ weightList


-- creates a new map with randomly locked rooms
createMap :: IO Mov.Map
createMap = do
   gen <- newStdGen
   let weightList = [(True, 2), (False, 8)]
   let randomBools = take Mov.numRooms (weightedList gen weightList)
   return (listArray ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) randomBools)


-- creates an empty character with a unique ID - that's why we have to pass that map around!
createCharacter :: Bool -> Mov.Map -> CharMap -> IO (Int, CharMap)
createCharacter checkForLocked roomMap charMap = do
   gen <- newStdGen
   let newID = head $ filter (\x -> not (Map.member x charMap)) (randoms gen :: [Int])
   let char = Cha.Character "" 100 False []
   pos <- randomPosition checkForLocked roomMap
   let newMap = Map.insert newID (char,pos) charMap
   return (newID, newMap)


 -- creates a new player character
createPlayer :: Mov.Map -> CharMap -> IO (Int, CharMap)
createPlayer roomMap charMap = do
   (newID, tempMap) <- createCharacter True roomMap charMap
   putStrLn "What is your name?"
   name <- getLine
   let (Just (char, pos)) = Map.lookup newID tempMap
   let player = Cha.setPlayerCharacter True . Cha.setName name $ char
   let newMap = Map.insert newID (player,pos) tempMap
   return (newID, newMap)


-- creates a ladder
createLadder :: Bool -> Mov.Map -> ItemMap -> IO (Int, ItemMap)
createLadder checkForLocked roomMap itemMap = do
   gen <- newStdGen
   let newID = head $ filter (\x -> not (Map.member x itemMap)) (randoms gen :: [Int])
   let ladder = Item.ladder
   pos@(_, inner, _) <- randomPosition checkForLocked roomMap
   if Mov.isWall inner
      then createLadder checkForLocked roomMap itemMap
      else do
         let newMap = Map.insert newID (ladder, pos) itemMap
         return (newID, newMap)


-- creates a random position, possibly disallowing locked rooms
randomPosition :: Bool -> Mov.Map -> IO Mov.Position
randomPosition checkForLocked roomMap
   | checkForLocked = do
      gen <- newStdGen
      let loc = head $ filter (\x -> not (roomMap ! x)) (randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen :: [(Int,Int)]) -- get a random unlocked room
      gen2 <- newStdGen
      let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen2 -- get a random Mov.InnerLocation
      gen3 <- newStdGen
      let dir = head $ randoms gen3 :: Mov.Direction -- get a random Mov.Direction
      let newPos = (loc, innerLoc, dir)
      return newPos
   | otherwise = do
      gen <- newStdGen
      let loc = head $ randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen :: (Int, Int) -- get a random room
      gen2 <- newStdGen
      let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen2 :: (Int, Int)
      gen3 <- newStdGen
      let dir = head $ randoms gen3 :: Mov.Direction
      let newPos = (loc, innerLoc, dir)
      return newPos


-- used for sorting the list that gets sent to the Draw.draw function
insert :: (Mov.InnerLocation, String) -> [(Mov.InnerLocation, String)] -> [(Mov.InnerLocation, String)]
insert a [] = [a]
insert a@((x1, y1), _) (b@((x2, y2), _) : rest)
   | x2 < x1 = b : insert a rest
   | x1 < x2 = a : b : rest
   | y2 < y1 = b : insert a rest
   | otherwise = a : b : rest


type Player = Int
type CharMap = Map.Map Int (Cha.Character, Mov.Position)

type Ladder = Int
type ItemMap = Map.Map Int (Item.Item, Mov.Position)

type Game = (Bool, Mov.Map, Mov.Position, Player, CharMap, Ladder, ItemMap)


getWallMsg :: IO String
getWallMsg = do
   gen <- newStdGen
   return . (wallMsgs!!) . head $ randomRs (0, length wallMsgs - 1) gen

wallMsgs :: [String]
wallMsgs = 
   ["You don't possess the ability to phase through walls yet, unfortunately."
   ,"I'm afraid I can't let you do that."
   ,"Why don't you try to walk through a door instead of a wall next time?"
   ]


getDoorBlockedMsg :: IO String
getDoorBlockedMsg = do
   _ <- newStdGen
   gen <- getStdGen
   return . (doorBlockedMsgs!!) . head $ randomRs (0, length doorBlockedMsgs - 1) gen

doorBlockedMsgs :: [String]
doorBlockedMsgs =
   ["Oh no! This tunnel has fallen in!"
   ,"You see multiple cracks in the ceiling. You decide you don't want to enter this doorway after all, fearing it might fall in."
   ,"The floor in front of you - well, \"floor\" - turns out to be a deep hole."
   ]


getRoomLockedMsg :: IO String
getRoomLockedMsg = do
   gen <- newStdGen
   return . (roomLockedMsgs!!) . head $ randomRs (0, length roomLockedMsgs - 1) gen

roomLockedMsgs :: [String]
roomLockedMsgs =
   ["This door is locked! You'll need a key to unlock it. Unfortunately, I haven't\ngotten to implementing those, so pray this isn't the only way."
   ,"You cannot pass. I want the key of this room to unlock myself. You cannot pass."
   ,"You try the door, but it won't budge. Maybe a key would help..."
   ]


getWallPushMsg :: IO String
getWallPushMsg = do
   _ <- newStdGen
   gen <- getStdGen
   return . (wallPushMsgs!!) . head $ randomRs (0, length wallPushMsgs - 1) gen

wallPushMsgs :: [String]
wallPushMsgs =
   ["Try as you might, you still can't push the ladder through the wall."
   ,"Maybe try to push the ladder through a door, not through a wall?"
   ,"This is a wall. It has the property of not allowing big enough things to go\nthrough it. You should know that."
   ]


-- compares the given ladder position and win position, but disregards the direction (since it's not important here)
hasWon :: Mov.Position -> Mov.Position -> Bool
hasWon (room1, inner1, _) (room2, inner2, _) = room1 == room2 && inner1 == inner2


-- creates an empty G ame so action "help" can be run
emptyGame :: Bool -> Mov.Map -> Game
emptyGame control roomMap = (control, roomMap, ((0,0),(0,0),Mov.North), 0, Map.empty, 0, Map.empty)


-- converts the user input to a Bool
setControl :: String -> Bool
setControl str = map toLower str == "short"


-- sets the starting parameters for the game like positions and control settings
initialize :: IO Game
initialize = do
   putStrLn "Do you want to use long or short controls? Enter short (case insensitive) for short controls or anything else for long controls:"
   controlStr <- getLine
   let control = setControl controlStr
   roomMap <- createMap
   let empty = emptyGame control roomMap
   action "help" empty
   (playerID, charMap) <- createPlayer roomMap Map.empty
   let (Just (player, winPosition)) = Map.lookup playerID charMap
   gen <- newStdGen
   let numItems = head $ randomRs (0,Mov.numRooms) gen
   (ladderID, itemMap) <- createLadder True roomMap Map.empty
   putStr $ "\nWell, " ++ Cha.name player
   putStrLn ", you're in quite a pickle right now. Remember? You were exploring a cave, but the floor you were standing on fell down and you with it... Maybe there's a ladder here somewhere?"
   return (control, roomMap, winPosition, playerID, charMap, ladderID, itemMap)


-- uses the module Draw to create a representation of the room's contents on the command line
drawMap :: Mov.Position -> Mov.Position -> Mov.Position -> IO ()
drawMap (playerRoom, playerInner, playerDir) (ladderRoom, ladderInner, _) (winRoom, winInner, _)
   -- both the ladder and the win position are in the current room and thus need to be displayed
   | playerRoom == ladderRoom && playerRoom == winRoom = if playerInner == winInner
      -- the player should always take priority if his position and the win position clash
      then if Mov.isDoorFull playerInner
         -- we don't want the door and the player to clash, so we're filtering that out
         then do
            let list1 = [(playerInner, Draw.player playerDir), (ladderInner, Draw.ladder)] ++ Draw.filterDoorList [playerInner, ladderInner]
            let list2 = [((x,y), Draw.dot) 
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , (x,y) /= ladderInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
         else do
            let list1 = [(playerInner, Draw.player playerDir), (ladderInner, Draw.ladder)] ++ Draw.filterDoorList [ladderInner]
            let list2 = [((x,y), Draw.dot) 
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , (x,y) /= ladderInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
      else if Mov.isDoorFull playerInner
         then do
            let list1 = [(playerInner, Draw.player playerDir), (ladderInner, Draw.ladder), (winInner, Draw.win)] ++ Draw.filterDoorList [playerInner, ladderInner, winInner]
            let list2 = [((x,y), Draw.dot)
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , (x,y) /= ladderInner
                        , (x,y) /= winInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
         else do
            let list1 = [(playerInner, Draw.player playerDir), (ladderInner, Draw.ladder), (winInner, Draw.win)] ++ Draw.filterDoorList [ladderInner, winInner]
            let list2 = [((x,y), Draw.dot)
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , (x,y) /= ladderInner
                        , (x,y) /= winInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
   -- the win position isn't in the current room, so only the door check is necessary
   | playerRoom == ladderRoom = if Mov.isDoorFull playerInner
      then do
         let list1 = [(playerInner, Draw.player playerDir), (ladderInner, Draw.ladder)] ++ Draw.filterDoorList [playerInner, ladderInner]
         let list2 = [((x,y), Draw.dot)
                     | x <- [0 .. Mov.roomSize]
                     , y <- [0 .. Mov.roomSize]
                     , (x,y) /= playerInner
                     , (x,y) /= ladderInner
                     , not $ Mov.isDoorFull (x,y)]
         let sortedList = foldr insert list2 list1
         Draw.draw sortedList
      else do
         let list1 = [(playerInner, Draw.player playerDir), (ladderInner, Draw.ladder)] ++ Draw.filterDoorList [ladderInner]
         let list2 = [((x,y), Draw.dot)
                     | x <- [0 .. Mov.roomSize]
                     , y <- [0 .. Mov.roomSize]
                     , (x,y) /= playerInner
                     , (x,y) /= ladderInner
                     , not $ Mov.isDoorFull (x,y)]
         let sortedList = foldr insert list2 list1
         Draw.draw sortedList
   -- the ladder isn't in the current room, but the win position is
   | playerRoom == winRoom = if playerInner == winInner
      then if Mov.isDoorFull playerInner
         then do
            let list1 = (playerInner, Draw.player playerDir) : Draw.filterDoorList [playerInner]
            let list2 = [((x,y), Draw.dot)
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
         else do
            let list1 = (playerInner, Draw.player playerDir) : Draw.doorList
            let list2 = [((x,y), Draw.dot)
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
      else if Mov.isDoorFull playerInner
         then do
            let list1 = [(playerInner, Draw.player playerDir), (winInner, Draw.win)] ++ Draw.filterDoorList [playerInner, winInner]
            let list2 = [((x,y), Draw.dot)
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , (x,y) /= winInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
         else do
            let list1 = [(playerInner, Draw.player playerDir), (winInner, Draw.win)] ++ Draw.filterDoorList [winInner]
            let list2 = [((x,y), Draw.dot)
                        | x <- [0 .. Mov.roomSize]
                        , y <- [0 .. Mov.roomSize]
                        , (x,y) /= playerInner
                        , (x,y) /= winInner
                        , not $ Mov.isDoorFull (x,y)]
            let sortedList = foldr insert list2 list1
            Draw.draw sortedList
   -- only the player is in the current room
   | otherwise = if Mov.isDoorFull playerInner
      then do
         let list1 = (playerInner, Draw.player playerDir) : Draw.filterDoorList [playerInner]
         let list2 = [((x,y), Draw.dot)
                     | x <- [0 .. Mov.roomSize]
                     , y <- [0 .. Mov.roomSize]
                     , (x,y) /= playerInner
                     , not $ Mov.isDoorFull (x,y)]
         let sortedList = foldr insert list2 list1
         Draw.draw sortedList
      else do
         let list1 = (playerInner, Draw.player playerDir) : Draw.doorList
         let list2 = [((x,y), Draw.dot)
                     | x <- [0 .. Mov.roomSize]
                     , y <- [0 .. Mov.roomSize]
                     , (x,y) /= playerInner
                     , not $ Mov.isDoorFull (x,y)]
         let sortedList = foldr insert list2 list1
         Draw.draw sortedList


-- Here, we finally get to play the game!
gameState :: Game -> IO String
gameState game@(control, _, winPos, playerID, charMap, ladderID, itemMap) = do
   let (Just (_, pos)) = Map.lookup playerID charMap
   let (Just (_, ladderPos)) = Map.lookup ladderID itemMap
   drawMap pos ladderPos winPos
   if control
      then do
         resultUnformatted <- shortInput game
         case resultUnformatted of
            (Left str) -> return str
            (Right game2) -> do
               putStrLn . take 7 $ repeat '\n'
               gameState game2
      else do
         resultUnformatted <- longInput game
         case resultUnformatted of
            (Left str) -> return str
            (Right game2) -> do
               putStrLn . take 7 $ repeat '\n'
               gameState game2


-- asks for user input and passes it on to the action function
longInput :: Game -> IO (Either String Game)
longInput game = do
   putStrLn "What do you want to do next?\n"
   input <- getLine
   let inputCaseIns = map toLower input
   putStrLn "\n"
   action inputCaseIns game


-- silently reads one keypress and returns it
getKey :: IO Char
getKey = do
   hSetEcho stdin False
   hSetBuffering stdin NoBuffering
   x <- getChar
   hSetEcho stdin True
   hSetBuffering stdin LineBuffering
   return x


-- translates the keys into actions
shortInput :: Game -> IO (Either String Game)
shortInput game = do
   input <- getKey
   putStrLn "\n"
   let inputCaseIns = toLower input
   case inputCaseIns of
      'w' -> action "go forward" game
      'd' -> action "go right" game
      's' -> action "go back" game
      'a' -> action "go left" game
      'f' -> action "turn right" game
      'r' -> action "turn around" game
      'e' -> action "turn left" game
      'h' -> action "help" game
      'q' -> action "quit" game
      _ -> action "unknown" game -- there isn't actually a match for unknown, so it will just fall through to the catch-all



-- decide, if the wanted action is allowed (in the case of moves)
action :: String -> Game -> IO (Either String Game)
action str oldGame@(control, roomMap, winPos, playerID, charMap, ladderID, itemMap)
   | str == "go forward" = do
      let (Just (player, oldPos@(_, oldInner, _))) = Map.lookup playerID charMap
      let (Just (ladder, (ladderRoom, ladderInner, ladderDir))) = Map.lookup ladderID itemMap
      let result = Mov.move oldPos Mov.Advance
      case result of
         Left resStr -- the move function has caught something to be handled here
            | resStr == "Wall" -> do
               msg <- getWallMsg
               putStrLn msg
               return $ Right oldGame
            | resStr == "Door blocked" -> do
               msg <- getDoorBlockedMsg
               putStrLn msg
               return $ Right oldGame
         Right newPos@(newRoom, newInner, _)
            | roomMap ! newRoom -> do -- if the new room is locked, don't allow the move
               msg <- getRoomLockedMsg
               putStrLn msg
               return $ Right oldGame
            | newRoom /= ladderRoom || newInner /= ladderInner -> do -- we won't collide with the ladder, so everything is alright
               let newCharMap = Map.insert playerID (player, newPos) charMap
               return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, itemMap)
            | Mov.isWall oldInner || not (Mov.isWall newInner) -> do
               -- we know that the push can't go wrong, so we don't need case
               let (Right newLadderPos@(newLadderRoom, newLadderInner, _)) = Mov.move newPos Mov.Advance
               if hasWon newLadderPos winPos
                  then return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n\n"
                  else if Mov.isCorner newLadderInner
                     then return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all...\n\n"
                     else do
                        -- update ladder and player positions and return them
                        let newItemMap = Map.insert ladderID (ladder, (newLadderRoom, newLadderInner, ladderDir)) itemMap
                        let newCharMap = Map.insert playerID (player, newPos) charMap
                        return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, newItemMap)
            | otherwise -> do -- we're okay to move, but need to check the same things as before
               let result2 = Mov.move newPos Mov.Advance
               case result2 of
                  Left resStr
                     | resStr == "Wall" -> do
                        msg <- getWallPushMsg
                        putStrLn msg
                        return $ Right oldGame
                     | resStr == "Door blocked" -> do
                        msg <- getDoorBlockedMsg
                        putStrLn msg
                        return $ Right oldGame
                  Right newLadderPos@(newLadderRoom, newLadderInner, _)
                     | roomMap ! newLadderRoom -> do
                        msg <- getRoomLockedMsg
                        putStrLn msg
                        return $ Right oldGame
                     | hasWon newLadderPos winPos -> return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n\n"
                     | Mov.isCorner newLadderInner -> return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all...\n\n"
                     | otherwise -> do
                        -- update ladder and player positions and return them
                        let newItemMap = Map.insert ladderID (ladder, (newLadderRoom, newLadderInner, ladderDir)) itemMap
                        let newCharMap = Map.insert playerID (player, newPos) charMap
                        return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, newItemMap)
   --
   -- turns around, goes forward, then turns around again if we didn't win or lose
   | str == "go back" = do
      (Right tempGame1) <- action "turn around" oldGame
      tempGameRes <- action "go forward" tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame2 -> action "turn around" tempGame2
   --
   -- Now for turning:
   | str == "turn left" = do
      let (Just (player, oldPos)) = Map.lookup playerID charMap
      let (Right newPos) = Mov.move oldPos Mov.TurnLeft
      let newCharMap = Map.insert playerID (player, newPos) charMap
      return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, itemMap)
   | str == "turn right" = do
      let (Just (player, oldPos)) = Map.lookup playerID charMap
      let (Right newPos) = Mov.move oldPos Mov.TurnRight
      let newCharMap = Map.insert playerID (player, newPos) charMap
      return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, itemMap)
   | str == "turn around" = do
      (Right tempGame) <- action "turn left" oldGame
      action "turn left" tempGame
   --
   -- And walking sideways:
   | str == "go left" = do
      (Right tempGame1) <- action "turn left" oldGame
      tempResult2 <- action "go forward" tempGame1
      case tempResult2 of
         Left resStr -> return $ Left resStr
         (Right tempGame2) -> action "turn right" tempGame2
   | str == "go right" = do
      (Right tempGame1) <- action "turn right" oldGame
      tempResult2 <- action "go forward" tempGame1
      case tempResult2 of
         Left resStr -> return $ Left resStr
         (Right tempGame2) -> action "turn left" tempGame2
   --
   -- Getting a list of commands:
   | str == "help" = do
      putStrLn "Possible commands:\n"
      if control
         then do
            putStrLn "w for going forward"
            putStrLn "d for going right"
            putStrLn "s for going back"
            putStrLn "a for going left\n"
            putStrLn "f for turning right"
            putStrLn "r for turning around"
            putStrLn "e for turning left\n"
            putStrLn "h for help"
            putStrLn "q for quitting\n"
            putStrLn "Key:\nYour position: ⯅ ⯈ ⯆ ⯇"
            putStrLn "The doors: ▯"
            putStrLn "The ladder: ☷"
            putStrLn "And the cave-in: ⭙\n\n"
            return $ Right oldGame
         else do
            putStrLn "go forward"
            putStrLn "go right"
            putStrLn "go back"
            putStrLn "go left\n"
            putStrLn "turn right"
            putStrLn "turn around"
            putStrLn "turn left\n"
            putStrLn "help"
            putStrLn "quit\n"
            putStrLn "Key:\nYour position: ⯅ ⯈ ⯆ ⯇"
            putStrLn "The doors: ▯"
            putStrLn "The ladder: ☷"
            putStrLn "And the cave-in: ⭙\n\n"
            return $ Right oldGame
   --
   -- Quitting:
   | str == "quit" = exitSuccess
   --
   -- Unknown commands:
   | otherwise = do
      putStrLn "This command was not recognized!"
      action "help" oldGame





main :: IO ()
main = do
   game <- initialize
   state <- gameState game
   putStrLn . take 70 $ repeat '\n'
   putStrLn state
