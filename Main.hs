import Data.Array
import Data.Char (toLower)
import System.IO
import System.Exit (exitSuccess)
import System.Random
import qualified Control.Monad.Random as Rand
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



-- defining some type synonyms for easier tracking
type Character = (Cha.Character, Mov.Position)
type Player = Character
type Enemy = Character
type Enemies = [Enemy]

type Item = (Item.Item, Mov.Position)
type Ladder = Item
type Items = [Item]

type Game = (Mov.Map, Mov.Position, Player, Enemies, Ladder, Items)




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


-- creates a new character
createCharacter :: Bool -> Mov.Map -> IO Character
createCharacter checkForLocked roomMap = do
   name <- getEnemyName
   let char = Cha.Character name 100 False []
   pos <- randomPosition checkForLocked roomMap
   return (char, pos)

-- creates a list of new enemies with length n
createEnemies :: (Num a, Ord a, Show a) => a -> Bool -> Mov.Map -> Enemies -> IO Enemies
createEnemies n checkForLocked roomMap enemies
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return enemies
   | n == 1 = do
      enemyWithPos@(_, pos) <- createCharacter checkForLocked roomMap
      if any (\(_, x) -> hasWon x pos) enemies -- checks if there already is another enemy at that position
         then createEnemies n checkForLocked roomMap enemies
         else return $ enemyWithPos : enemies
   | otherwise = do
      enemyWithPos@(_, pos) <- createCharacter checkForLocked roomMap
      if any (\(_, x) -> hasWon x pos) enemies
         then createEnemies n checkForLocked roomMap enemies
         else createEnemies (n - 1) checkForLocked roomMap (enemyWithPos : enemies)


-- creates a new player character
createPlayer :: Mov.Map -> IO Player
createPlayer roomMap = do
   (char, pos) <- createCharacter True roomMap
   putStrLn "What is your name?"
   name <- getLine
   let player = Cha.setPlayerCharacter True . Cha.setName name $ char
   return (player, pos)


-- creates a new, random item that isn't a ladder
createItem :: Bool -> Mov.Map -> IO Item
createItem checkForLocked roomMap = do
   gen <- newStdGen
   let item@(_, _, attr) = (Item.invItemList!!) . head $ randomRs (0, length Item.invItemList - 1) gen
   pos@(itemRoom, _, _) <- randomPosition checkForLocked roomMap
   case attr of
      Item.Key {Item.room = keyRoom} -- if the item is a key, some further checks are necessary
         | itemRoom == keyRoom -> createItem checkForLocked roomMap
         -- the key was spawned in the room that it unlocks, rendering it inaccessible
         | not (roomMap ! keyRoom) -> createItem checkForLocked roomMap
         -- the key is useless, because the room it unlocks already is unlocked
      attr -> return (item, pos)

-- creates a list of new items with length n
createItems :: (Num a, Ord a, Show a) => a -> Bool -> Mov.Map -> Items -> IO Items
createItems n checkForLocked roomMap items
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return items
   | n == 1 = do
      itemWithPos@(_, pos) <- createItem checkForLocked roomMap
      if any (\(_, x) -> hasWon x pos) items -- checks if there already is another item at that position
         then createItems n checkForLocked roomMap items
         else return $ itemWithPos : items
   | otherwise = do
      itemWithPos@(_, pos) <- createItem checkForLocked roomMap
      if any (\(_, x) -> hasWon x pos) items
         then createItems n checkForLocked roomMap items
         else createItems (n - 1) checkForLocked roomMap (itemWithPos : items)



-- creates a ladder, takes care not to spawn it at the edges of the rooms
createLadder :: Bool -> Mov.Map -> IO Item
createLadder checkForLocked roomMap = do
   let ladder = Item.ladder
   pos@(_, inner, _) <- randomPosition checkForLocked roomMap
   if Mov.isWall inner
      then createLadder checkForLocked roomMap
      else return (ladder, pos)


-- creates a random position, possibly disallowing locked rooms
randomPosition :: Bool -> Mov.Map -> IO Mov.Position
randomPosition checkForLocked roomMap
   | checkForLocked = do
      gen <- newStdGen
      let loc = head . filter (\x -> not (roomMap ! x)) . randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) $ gen -- get a random unlocked room
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





getEnemyName :: IO String
getEnemyName = do
   gen <- newStdGen
   return . (enemyNames!!) . head $ randomRs (0, length enemyNames - 1) gen

enemyNames :: [String]
enemyNames =
   ["Berserker"
   ,"Cave Troll"
   ,"Ghoul"
   ,"Hobgoblin"
   ,"Orc"
   ,"Wraith"
   ]


printWallMsg :: IO ()
printWallMsg = do
   gen <- newStdGen
   putStrLn . (wallMsgs!!) . head $ randomRs (0, length wallMsgs - 1) gen

wallMsgs :: [String]
wallMsgs = 
   ["You don't possess the ability to phase through walls yet, unfortunately."
   ,"I'm afraid I can't let you do that."
   ,"Why don't you try to walk through a door instead of a wall next time?"
   ]


printDoorBlockedMsg :: IO ()
printDoorBlockedMsg = do
   _ <- newStdGen
   gen <- getStdGen
   putStrLn . (doorBlockedMsgs!!) . head $ randomRs (0, length doorBlockedMsgs - 1) gen

doorBlockedMsgs :: [String]
doorBlockedMsgs =
   ["Oh no! This tunnel has fallen in!"
   ,"You see multiple cracks in the ceiling. You decide you don't want to enter this doorway after all, fearing it might fall in."
   ,"The floor in front of you - well, \"floor\" - turns out to be a deep hole."
   ]


printRoomLockedMsg :: IO ()
printRoomLockedMsg = do
   gen <- newStdGen
   putStrLn . (roomLockedMsgs!!) . head $ randomRs (0, length roomLockedMsgs - 1) gen

roomLockedMsgs :: [String]
roomLockedMsgs =
   ["This door is locked! You'll need a key to unlock it. Unfortunately, I haven't\ngotten to implementing those, so pray this isn't the only way."
   ,"You cannot pass. I want the key of this room to unlock myself. You cannot pass."
   ,"You try the door, but it won't budge. Maybe a key would help..."
   ]


printWallPushMsg :: IO ()
printWallPushMsg = do
   _ <- newStdGen
   gen <- getStdGen
   putStrLn . (wallPushMsgs!!) . head $ randomRs (0, length wallPushMsgs - 1) gen

wallPushMsgs :: [String]
wallPushMsgs =
   ["Try as you might, you still can't push the ladder through the wall."
   ,"Maybe try to push the ladder through a door, not through a wall?"
   ,"This is a wall. It has the property of not allowing big enough things to go\nthrough it. You should know that."
   ]


printKey :: IO ()
printKey = do
   putStrLn "Key:\nYour position: ⯅ ⯈ ⯆ ⯇"
   putStrLn "The doors: ▯"
   putStrLn "The ladder: ☷"
   putStrLn "And the cave-in: ⭙\n\n"


-- compares the given ladder position and win position, but disregards the direction (since it's not important here)
hasWon :: Mov.Position -> Mov.Position -> Bool
hasWon (room1, inner1, _) (room2, inner2, _) = room1 == room2 && inner1 == inner2


-- creates an empty Game so playerAction "help" can be run
emptyGame :: Mov.Map -> Game
emptyGame roomMap = (roomMap, Mov.nullPosition, (Cha.Null, Mov.nullPosition), [], (("", False, Item.Nil), Mov.nullPosition), [])





-- sets the starting parameters for the game like positions and control settings
initialize :: IO Game
initialize = do
   roomMap <- createMap
   let empty = emptyGame roomMap
   playerAction "help" empty
   playerWithPos@(player, winPosition) <- createPlayer roomMap
   gen <- newStdGen
   gen2 <- getStdGen
   let numEnemies = head $ randomRs (0, Mov.numRooms * 2) gen
   enemies <- createEnemies numEnemies False roomMap []
   let numItems = head $ randomRs (0, Mov.numRooms) gen2
   items <- createItems numItems False roomMap []
   ladderWithPos <- createLadder True roomMap
   putStr $ "\nWell, " ++ Cha.name player
   putStrLn ", you're in quite a pickle right now. Remember? You were exploring a cave, but the floor you were standing on fell down and you with it... Maybe there's a ladder here somewhere?"
   return (roomMap, winPosition, playerWithPos, enemies, ladderWithPos, items)


-- uses the module Draw to create a representation of the room's contents on the command line
drawMap :: Game -> IO ()
drawMap (roomMap, winPos@(winRoom, winInner, _), (player, (playerRoom, playerInner, playerDir)), enemyList, (_, (ladderRoom, ladderInner, _)), itemList) = do
   let playerHP = Cha.hp player
   let playerInv = Cha.inv player
   let equipPlayer = filter snd playerInv
   case () of
      ()
         -- both the ladder and the win position are in the current room and thus need to be displayed
         | playerRoom == ladderRoom && playerRoom == winRoom -> if playerInner == winInner
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
         | playerRoom == ladderRoom -> if Mov.isDoorFull playerInner
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
         | playerRoom == winRoom -> if playerInner == winInner
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
         | otherwise -> if Mov.isDoorFull playerInner
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

-- tracks the state of the game, returns only if you've won or lost
gameState :: Game -> IO String
gameState game = do
   putStrLn . take 7 $ repeat '\n'
   drawMap game
   isNewInput <- hWaitForInput stdin 2000
   if isNewInput
      then do
         input <- getChar
         let action = shortInput input
         playerResultUnformatted <- playerAction action game
         case playerResultUnformatted of
            (Left str) -> return str
            (Right newGame) -> do
               -- TODO: Add Enemy actions here
               gameState newGame
      else do
         putStrLn "Kein Input"
         -- TODO: Add Enemy actions here
         gameState game




-- translates the keys into playerAction strings
shortInput :: Char -> String
shortInput input = case inputCaseIns of
      'w' -> "go forward"
      'd' -> "go right"
      's' -> "go back"
      'a' -> "go left"
      'f' -> "turn right"
      'r' -> "turn around"
      'e' -> "turn left"
      'h' -> "help"
      'q' -> "quit"
      _ -> "unknown" -- there isn't actually a match for unknown, so it will just fall through to the catch-all
  where inputCaseIns = toLower input



-- decide if the action is allowed and if it is, do it
playerAction :: String -> Game -> IO (Either String Game)
playerAction str oldGame@(roomMap, winPos, (player, oldPlayerPos@(_, oldPlayerInner, _)), enemyList, (ladder, oldLadderPos@(ladderRoom, ladderInner, ladderDir)), itemList)
   | str == "go forward" = do
      let result = Mov.move oldPlayerPos Mov.Advance
      case result of
         Left resStr -- the move function has caught something to be handled here
            | resStr == "Wall" -> do
               printWallMsg
               return $ Right oldGame
            | resStr == "Door blocked" -> do
               printDoorBlockedMsg
               return $ Right oldGame
         Right newPlayerPos@(newPlayerRoom, newPlayerInner, _)
            | roomMap ! newPlayerRoom -> do -- if the new room is locked, don't allow the move
               printRoomLockedMsg
               return $ Right oldGame
            | not (any (newPlayerPos `hasWon`) (ladderPos : map snd enemyList)) -> return $ Right (roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
               -- we won't collide with the ladder, so everything is alright
            | Mov.isWall oldPlayerInner || not (Mov.isWall newPlayerInner) -> do
               -- we know that the push can't go wrong, so we don't need case
               let (Right newLadderPos@(newLadderRoom, newLadderInner, _)) = Mov.move newPlayerPos Mov.Advance
               if hasWon newLadderPos winPos
                  then return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n\n"
                  else if Mov.isCorner newLadderInner
                     then return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all...\n\n"
                     else return $ Right (roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, (newLadderRoom, newLadderInner, ladderDir)), itemList)
                           -- update ladder and player positions and return them
            | otherwise -> do -- we're okay to move, but need to check the same things as before
               let result2 = Mov.move newPlayerPos Mov.Advance
               case result2 of
                  Left resStr
                     | resStr == "Wall" -> do
                        printWallPushMsg
                        return $ Right oldGame
                     | resStr == "Door blocked" -> do
                        printDoorBlockedMsg
                        return $ Right oldGame
                  Right newLadderPos@(newLadderRoom, newLadderInner, _)
                     | roomMap ! newLadderRoom -> do
                        printRoomLockedMsg
                        return $ Right oldGame
                     | hasWon newLadderPos winPos -> return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n"
                     | Mov.isCorner newLadderInner -> return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all...\n"
                     | otherwise -> return $ Right (roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, (newLadderRoom, newLadderInner, ladderDir)), itemList)
                       -- update ladder and player positions and return them
   --
   -- turns around, goes forward, then turns around again if we didn't win or lose
   | str == "go back" = do
      (Right tempGame1) <- playerAction "turn around" oldGame
      tempGameRes <- playerAction "go forward" tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame2 -> playerAction "turn around" tempGame2
   --
   -- Now for turning:
   | str == "turn left" = do
      let (Right newPlayerPos) = Mov.move oldPlayerPos Mov.TurnLeft
      return $ Right (roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   | str == "turn right" = do
      let (Right newPlayerPos) = Mov.move oldPlayerPos Mov.TurnRight
      return $ Right (roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   | str == "turn around" = do
      (Right tempGame) <- playerAction "turn left" oldGame
      playerAction "turn left" tempGame
   --
   -- And walking sideways:
   | str == "go left" = do
      (Right tempGame1) <- playerAction "turn left" oldGame
      tempResult2 <- playerAction "go forward" tempGame1
      case tempResult2 of
         Left resStr -> return $ Left resStr
         (Right tempGame2) -> playerAction "turn right" tempGame2
   | str == "go right" = do
      (Right tempGame1) <- playerAction "turn right" oldGame
      tempResult2 <- playerAction "go forward" tempGame1
      case tempResult2 of
         Left resStr -> return $ Left resStr
         (Right tempGame2) -> playerAction "turn left" tempGame2
   --
   -- Getting a list of commands:
   | str == "help" = do
      putStrLn "Possible commands:\n"
      putStrLn "w for going forward"
      putStrLn "d for going right"
      putStrLn "s for going back"
      putStrLn "a for going left\n"
      putStrLn "f for turning right"
      putStrLn "r for turning around"
      putStrLn "e for turning left\n"
      putStrLn "h for help"
      putStrLn "q for quitting\n"
      printKey
      return $ Right oldGame
   --
   -- Quitting:
   | str == "quit" = exitSuccess
   --
   -- Unknown commands:
   | otherwise = do
      putStrLn "This command was not recognized!"
      playerAction "help" oldGame


-- and now the possible actions for enemies
enemyAction :: String -> Enemy -> Game -> IO (Either String Game)
enemyAction str (enemy, oldEnemyPos@(oldEnemyRoom, oldEnemyInner, oldEnemyDir)) oldGame@(roomMap, winPos, (player, playerPos@(playerRoom, playerInner, _)), enemyList, (ladder, ladderPos@(ladderRoom, ladderInner, _)), itemList)
   | str == "go forward" = do -- much the same as above
      let result = Mov.move oldEnemyPos Mov.Advance
      case result of
         Left resStr
            | resStr == "Wall" -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
            | resStr == "Door blocked" -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
         Right newEnemyPos@(newEnemyRoom, newEnemyInner, _)
            -- TODO: decide if enemies should be able to enter locked rooms
            | roomMap ! newEnemyRoom -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
            -- makes sure there isn't anything on the new position
            | not (any (newEnemyPos `hasWon`) (ladderPos : playerPos : map snd enemyList)) -> return $
               Right (roomMap, winPos, (player, playerPos), (enemy, newEnemyPos) : enemyList, (ladder, ladderPos), itemList)
            -- checks if the obstacle is the player
            | hasWon newEnemyPos playerPos -> enemyAction "attack" (enemy, oldEnemyPos) oldGame
            | otherwise -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
   | str == "go back" = do
      (Right tempGame1@(_, _, _, (_, tempEnemyPos1) : _, _, _)) <- enemyAction "turn around" (enemy, oldEnemyPos) oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame2@(_, _, _, (tempEnemy, tempEnemyPos2) : _, _, _) -> enemyAction "turn around" (tempEnemy, tempEnemyPos2) tempGame2
      










main :: IO ()
main = do 
   game <- initialize
   hSetEcho stdin False
   hSetBuffering stdin NoBuffering
   state <- gameState game
   hSetEcho stdin True
   hSetBuffering stdin LineBuffering
   putStrLn . take 70 $ repeat '\n'
   putStrLn $ state ++ "\n"
