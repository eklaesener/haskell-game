import Data.Array (listArray, (!))
import Data.Char (toLower)
import Data.List (minimumBy)
import Data.Maybe (isJust, isNothing)
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

type Game = (String, Mov.Map, Mov.Position, Player, Enemies, Ladder, Items)




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
createEnemies :: Int -> Bool -> Mov.Map -> Enemies -> IO Enemies
createEnemies n checkForLocked roomMap enemies
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return enemies
{-   | n == 1 = do
      enemyWithPos@(_, pos) <- createCharacter checkForLocked roomMap
      if any (\(_, x) -> hasWon x pos) enemies -- checks if there already is another enemy at that position
         then createEnemies n checkForLocked roomMap enemies
         else return $ enemyWithPos : enemies  -}
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
createItems :: Int -> Bool -> Mov.Map -> Items -> IO Items
createItems n checkForLocked roomMap items
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return items
{-   | n == 1 = do
      itemWithPos@(_, pos) <- createItem checkForLocked roomMap
      if any (\(_, x) -> hasWon x pos) items -- checks if there already is another item at that position
         then createItems n checkForLocked roomMap items
         else return $ itemWithPos : items  -}
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



-- filters the given enemy out of the enemyList and, if wanted, prepends them to it
filterEnemy :: Bool -> Enemy -> Game -> Game
filterEnemy bool enemyWithPos (narrStr, roomMap, winPos, playerWithPos, enemyList, ladderWithPos, itemList)
   | bool = (narrStr, roomMap, winPos, playerWithPos, enemyWithPos : filteredList, ladderWithPos, itemList)
   | otherwise = (narrStr, roomMap, winPos, playerWithPos, filteredList, ladderWithPos, itemList)
  where filteredList = filter (/= enemyWithPos) enemyList



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


getMapKey :: String
getMapKey =  "Key:\nYour position: ⯅ ⯈ ⯆ ⯇\n"
          ++ "The doors: ▯\n"
          ++ "The ladder: ☷\n"
          ++ "And the cave-in: ⭙\n"


-- compares the two given positions, but disregards the direction (since it's not important here)
hasWon :: Mov.Position -> Mov.Position -> Bool
hasWon (room1, inner1, _) (room2, inner2, _) = room1 == room2 && inner1 == inner2


-- creates an empty Game so playerAction "help" can be run
emptyGame :: Mov.Map -> Game
emptyGame roomMap = ("", roomMap, Mov.nullPosition, (Cha.nullCharacter, Mov.nullPosition), [], (("", False, Item.Nil), Mov.nullPosition), [])





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
   return ("", roomMap, winPosition, playerWithPos, enemies, ladderWithPos, items)


-- uses the module Draw to create a representation of the room's contents on the command line
drawMap :: Game -> IO ()
drawMap (narratorStr, roomMap, winPos@(winRoom, winInner, _), (player, (playerRoom, playerInner, playerDir)), enemyList, (_, (ladderRoom, ladderInner, _)), itemList) = do
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
   isNewInput <- hWaitForInput stdin 500
   if isNewInput
      then do
         input <- getChar
         let action = shortInput input
         playerResultUnformatted <- playerAction action game
         case playerResultUnformatted of
            Left str -> return str
            Right tempGame -> do
               enemyResultUnformatted <- cycleEnemies tempGame
               case enemyResultUnformatted of
                  Left str -> return str
                  Right newGame -> gameState newGame
      else do
         enemyResultUnformatted <- cycleEnemies game
         case enemyResultUnformatted of
            Left str -> return str
            Right newGame -> gameState newGame



-- cycles through the list of enemies, performing one action each
cycleEnemies :: Game -> IO (Either String Game)
cycleEnemies oldGame@(_, _, _, _, enemyList, _, _)
   | null enemyList = return $ Right oldGame
   | otherwise = helper (head enemyList) (tail enemyList) oldGame
  where
   helper enemyWithPos rest oldGame@(narrStr, roomMap, winPos, playerWithPos, oldEnemyList, ladderWithPos, itemList)
      | null rest = do
         result <- enemyAction "random action" enemyWithPos oldGame
         case result of
            Left str -> return $ Left str
            Right (newNarrStr, _, _, newPlayerWithPos, newEnemyWithPos : tempEnemyList, _, newItemList) -> do
               let newEnemyList = newEnemyWithPos : filter (/= enemyWithPos) tempEnemyList
               return $ Right (newNarrStr, roomMap, winPos, newPlayerWithPos, newEnemyList, ladderWithPos, newItemList)
      | otherwise = do
         result <- enemyAction "random action" enemyWithPos oldGame
         case result of
            Left str -> return $ Left str
            Right (newNarrStr, _, _, newPlayerWithPos, newEnemyWithPos : tempEnemyList, _, newItemList) -> do
               let newEnemyList = newEnemyWithPos : filter (/= enemyWithPos) tempEnemyList
               helper (head rest) (tail rest) (newNarrStr, roomMap, winPos, newPlayerWithPos, newEnemyList, ladderWithPos, newItemList)


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
playerAction str oldGame@(narratorstr, roomMap, winPos, (player, oldPlayerPos@(_, oldPlayerInner, _)), enemyList, (ladder, oldLadderPos@(ladderRoom, ladderInner, ladderDir)), itemList)
   | str == "go forward" = do
      let result = Mov.move oldPlayerPos Mov.Advance
      case result of
         Left resStr -- the move function has caught something to be handled here
            | resStr == "Wall" -> do
               newNaStr <- getWallMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            | resStr == "Door blocked" -> do
               newNaStr <- getDoorBlockedMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         Right newPlayerPos@(newPlayerRoom, newPlayerInner, _)
            | roomMap ! newPlayerRoom && not (Cha.hasKey newPlayerRoom player) -> do -- if the new room is locked and the player doesn't have the right key, don't allow the move
               newNaStr <- getRoomLockedMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            -- we won't collide with anything, so everything is alright
            | not (any (newPlayerPos `hasWon`) (oldLadderPos : map snd enemyList)) -> return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            -- we did collide with something, if it's an enemy, make him attack the player or turn
            |  any (newPlayerPos `hasWon`) $ map snd enemyList -> do
               let enemyWithPos = head . filter (\(_, x) -> newPlayerPos `hasWon` x) $ enemyList
               enemyAction "attack" enemyWithPos oldGame
            -- we collided with the ladder
            | Mov.isWall oldPlayerInner || not (Mov.isWall newPlayerInner) -> do
               -- we know that the push can't go wrong, so we don't need case
               let (Right newLadderPos@(newLadderRoom, newLadderInner, _)) = Mov.move newPlayerPos Mov.Advance
               if hasWon newLadderPos winPos
                  then return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n\n"
                  else if Mov.isCorner newLadderInner
                     then return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all...\n\n"
                     else return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, (newLadderRoom, newLadderInner, ladderDir)), itemList)
                           -- update ladder and player positions and return them
            | otherwise -> do -- we're okay to move, but need to check the same things as before
               let result2 = Mov.move newPlayerPos Mov.Advance
               case result2 of
                  Left resStr
                     | resStr == "Wall" -> do
                        newNaStr <- getWallPushMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                     | resStr == "Door blocked" -> do
                        newNaStr <- getDoorBlockedMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                  Right newLadderPos@(newLadderRoom, newLadderInner, _)
                     | roomMap ! newLadderRoom && not (Cha.hasKey newLadderRoom player) -> do
                        newNaStr <- getRoomLockedMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                     | hasWon newLadderPos winPos -> return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n"
                     | Mov.isCorner newLadderInner -> return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all...\n"
                     | otherwise -> return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, (newLadderRoom, newLadderInner, ladderDir)), itemList)
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
      return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   | str == "turn right" = do
      let (Right newPlayerPos) = Mov.move oldPlayerPos Mov.TurnRight
      return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
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
   | str == "attack" = do
      let losList = filter (\(_, pos) -> pos `Mov.inLOS` oldPlayerPos) enemyList
      let frontList = filter (\(_, pos) -> pos `Mov.inFrontOf` oldPlayerPos) losList
      if null losList
         then return $ Right ("I don't see anyone standing there, do you?", roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         else do
            let (nextEnemy, nextEnemyPos) = minimumBy (\(_, x) (_, y) -> compare (x `Mov.distanceTo` oldPlayerPos) (y `Mov.distanceTo` oldPlayerPos)) losList
            let playerWeapon = Cha.equippedWeapon player
            let enemyShield = Cha.equippedShield nextEnemy
            failMsg <- getOutOfReachMsg $ Cha.name nextEnemy
            winMsg <- getVictorMsg $ Cha.name nextEnemy
            hitMsg <- getHitMsg $ Cha.name nextEnemy
            shieldHitMsg <- getShieldHitMsg $ Cha.name nextEnemy
            shieldDestroyedMsg <- getShieldDestroyedMsg $ Cha.name nextEnemy
            if null frontList
               then case playerWeapon of
                  Nothing -> return $ Right (failMsg, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                  Just (_, _, Item.Weapon dmg range) -> if (nextEnemyPos `Mov.distanceTo` oldPlayerPos) <= range
                     then case enemyShield of
                        Nothing -> do
                           let newEnemy = Cha.reduceHealth dmg nextEnemy
                           if Cha.isDead newEnemy
                              then return $ Right (winMsg, roomMap, winPos, (player, oldPlayerPos), filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                              else return $ Right (hitMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                        Just oldShield -> do
                           let newShield = Item.reduceDur dmg oldShield
                           if Item.isDestroyed newShield
                              then do
                                 let newEnemy = Cha.dropItem oldShield nextEnemy
                                 return $ Right (shieldDestroyedMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                              else do
                                 let newEnemy = Cha.modifyInv True oldShield newShield nextEnemy
                                 return $ Right (shieldHitMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                     -- You can't reach the enemy
                     else return $ Right (failMsg, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
               else case playerWeapon of
                  Nothing -> case enemyShield of
                     Nothing -> do
                        let newEnemy = Cha.reduceHealth Item.fistDmg nextEnemy
                        if Cha.isDead newEnemy
                           then return $ Right (winMsg, roomMap, winPos, (player, oldPlayerPos), filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                           else return $ Right (hitMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                     Just oldShield -> do
                        let newShield = Item.reduceDur Item.fistDmg oldShield
                        if Item.isDestroyed newShield
                           then do
                              let newEnemy = Cha.dropItem oldShield nextEnemy
                              return $ Right (shieldDestroyedMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                           else do
                              let newEnemy = Cha.modifyInv True oldShield newShield nextEnemy
                              return $ Right (shieldHitMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                  Just (_, _, Item.Weapon dmg range) -> case enemyShield of
                     Nothing -> do
                        let newEnemy = Cha.reduceHealth dmg nextEnemy
                        if Cha.isDead newEnemy
                           then return $ Right (winMsg, roomMap, winPos, (player, oldPlayerPos), filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                           else return $ Right (hitMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                     Just oldShield -> do
                        let newShield = Item.reduceDur dmg oldShield
                        if Item.isDestroyed newShield
                           then do
                              let newEnemy = Cha.dropItem oldShield nextEnemy
                              return $ Right (shieldDestroyedMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
                           else do
                              let newEnemy = Cha.modifyInv True oldShield newShield nextEnemy
                              return $ Right (shieldHitMsg, roomMap, winPos, (player, oldPlayerPos), (newEnemy, nextEnemyPos) : filter (\x -> x /= (nextEnemy, nextEnemyPos)) enemyList, (ladder, oldLadderPos), itemList)
   --
   -- Picking up items
   | str == "pickup item" = do
      -- get a list of the items at the current location
      failStr <- getNoItemHereMsg
      let placeItemList = filter (\(_, x) -> x `hasWon` oldPlayerPos) itemList
      if null placeItemList
         then return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         else do
            let (newItem, _) = head placeItemList
            let newPlayer = Cha.pickupItem False newItem player
            successStr <- getItemPickedUpMsg $ Item.name newItem
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), filter (/= (newItem, oldPlayerPos)) itemList)
   --
   -- Dropping the currently equipped weapon
   | str == "drop weapon" = do
      failStr <- getNoWeaponMsg
      let playerWeapon = Cha.equippedWeapon player
      case playerWeapon of
         Nothing -> return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         Just weapon -> do
            successStr <- getWeaponDroppedMsg $ Item.name weapon
            let newPlayer = Cha.dropItem weapon player
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), (weapon, oldPlayerPos) : itemList)
   --
   -- Dropping the currently equipped shield
   | str == "drop shield" = do
      failStr <- getNoShieldMsg
      let playerShield = Cha.equippedShield player
      case playerShield of
         Nothing -> return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         Just shield -> do
            successStr <- getShieldDroppedMsg $ Item.name shield
            let newPlayer = Cha.dropItem shield player
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), (shield, oldPlayerPos) : itemList)
   --
   -- Swapping the currently equipped weapon with the next available one
   | str == "swap weapon" = if Cha.numWeapons == 0 || (Cha.numWeapons player == 1 && isJust (Cha.equippedWeapon player))
      then do
         failStr <- getWeaponSwapNotEnoughMsg
         return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos),  enemyList, (ladder, oldLadderPos), itemList)
      else do
         let unUsedWeaponList = map fst . filter (not . snd) $ Cha.weaponList player
         gen <- newStdGen
         let newWeapon = (unUsedWeaponList!!) . head . randomRs (0, length unUsedWeaponList - 1) $ gen
         let oldWeapon = Cha.equippedWeapon player
         case oldWeapon of
            Nothing -> do
               let newPlayer = Cha.setEquipped True newWeapon player
               successStr <- getWeaponSwapOneMsg $ Item.name newWeapon
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            Just weapon -> do
               let newPlayer = Cha.setEquipped True newWeapon . Cha.setEquipped False weapon $ player
               successStr <- getWeaponSwapTwoMsg (Item.name weapon) (Item.name newWeapon)
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   --
   -- The same for the shield
   | str == "swap shield" = if Cha.numShields == 0 || (Cha.numShields player == 1 && isJust (Cha.equippedShield player))
      then do
         failStr <- getShieldSwapNotEnoughMsg
         return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos),  enemyList, (ladder, oldLadderPos), itemList)
      else do
         let unUsedShieldList = map fst . filter (not . snd) $ Cha.shieldList player
         gen <- newStdGen
         let newShield = (unUsedShieldList!!) . head . randomRs (0, length unUsedShieldList - 1) $ gen
         let oldShield = Cha.equippedShield player
         case oldShield of
            Nothing -> do
               let newPlayer = Cha.setEquipped True newShield player
               successStr <- getShieldSwapOneMsg $ Item.name newShield
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            Just shield -> do
               let newPlayer = Cha.setEquipped True newShield . Cha.setEquipped False shield $ player
               successStr <- getShieldSwapTwoMsg (Item.name shield) (Item.name newShield)
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)

   --
   -- Getting a list of commands:
   | str == "help" = do
      let newNaStr =  "Possible commands:\n"
                   ++ "w for going forward"
                   ++ "d for going right"
                   ++ "s for going back"
                   ++ "a for going left\n"
                   ++ "f for turning right"
                   ++ "r for turning around"
                   ++ "e for turning left\n"
                   ++ "h for help"
                   ++ "q for quitting\n"
                   ++ getMapKey
      return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   --
   -- Quitting:
   | str == "quit" = exitSuccess
   --
   -- Unknown commands:
   | otherwise = do
      let tempStr = "This command was not recognized!"
      Right (tempNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList) <- playerAction "help" oldGame
      return $ Right (tempStr ++ tempNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)


-- and now the possible actions for enemies
enemyAction :: String -> Enemy -> Game -> IO (Either String Game)
enemyAction str (enemy, oldEnemyPos@(oldEnemyRoom, oldEnemyInner, oldEnemyDir)) oldGame@(narratorstr, roomMap, winPos, (player, playerPos@(playerRoom, playerInner, _)), enemyList, (ladder, ladderPos@(ladderRoom, ladderInner, _)), itemList)
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
               Right (narratorstr, roomMap, winPos, (player, playerPos), (enemy, newEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
            -- checks if the obstacle is the player
            | hasWon newEnemyPos playerPos -> enemyAction "attack" (enemy, oldEnemyPos) oldGame
            | otherwise -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
   | str == "sprint forward" = do
      tempGameRes <- enemyAction "go forward" (enemy, oldEnemyPos) oldGame
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame@(_, _, _, _, (newEnemy, newEnemyPos) : _, _, _) -> if hasWon oldEnemyPos newEnemyPos
            then return . Right $ filterEnemy False (enemy, oldEnemyPos) tempGame
            else enemyAction "sprint forward" (newEnemy, newEnemyPos) $ filterEnemy False (enemy, oldEnemyPos) tempGame
   | str == "go back" = do
      (Right tempGame1@(_, _, _, _, (_, tempEnemyPos1) : _, _, _)) <- enemyAction "turn around" (enemy, oldEnemyPos) oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) $ filterEnemy False (enemy, oldEnemyPos) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame2@(_, _, _, _, (tempEnemy, tempEnemyPos2) : _, _, _) -> enemyAction "turn around" (tempEnemy, tempEnemyPos2) $ filterEnemy False (enemy, tempEnemyPos1) tempGame2
   | str == "go left" = do
      (Right tempGame1@(_, _, _, _, (_, tempEnemyPos1) : _, _, _)) <- enemyAction "turn left" (enemy, oldEnemyPos) oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) $ filterEnemy False (enemy, oldEnemyPos) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame2@(_, _, _, _, (tempEnemy, tempEnemyPos2) : _, _, _) -> enemyAction "turn right" (tempEnemy, tempEnemyPos2) $ filterEnemy False (enemy, tempEnemyPos1) tempGame2
   | str == "go right" = do
      (Right tempGame1@(_, _, _, _, (_, tempEnemyPos1) : _, _, _)) <- enemyAction "turn right" (enemy, oldEnemyPos) oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) $ filterEnemy False (enemy, oldEnemyPos) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame2@(_, _, _, _, (tempEnemy, tempEnemyPos2) : _, _, _) -> enemyAction "turn left" (tempEnemy, tempEnemyPos2) $ filterEnemy False (enemy, tempEnemyPos1) tempGame2
   | str == "turn left" = do
      let (Right newEnemyPos) = Mov.move oldEnemyPos Mov.TurnLeft
      return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (enemy, newEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
   | str == "turn right" = do
      let (Right newEnemyPos) = Mov.move oldEnemyPos Mov.TurnRight
      return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (enemy, newEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
   | str == "turn around" = do
      (Right tempGame@(_, _, _, _, (_, tempEnemyPos) : _, _, _)) <- enemyAction "turn left" (enemy, oldEnemyPos) oldGame
      enemyAction "turn left" (enemy, tempEnemyPos) $ filterEnemy False (enemy, oldEnemyPos) tempGame
   | str == "turn randomly" = do
      gen <- newStdGen
      let option = head . randomRs (1,3) $ gen :: Int
      case option of
         1 -> enemyAction "turn left" (enemy, oldEnemyPos) oldGame
         2 -> enemyAction "turn right" (enemy, oldEnemyPos) oldGame
         3 -> enemyAction "turn around" (enemy, oldEnemyPos) oldGame
   | str == "attack" = do
      let enemyWeapon = Cha.equippedWeapon enemy
      let playerShield = Cha.equippedShield player
      if playerPos `Mov.inFrontOf` oldEnemyPos -- the player is directly in front of the enemy
         then case enemyWeapon of -- check if the enemy has a weapon equipped
            Nothing -> case playerShield of -- check if the player has a shield equipped
               Nothing -> do
                  let newPlayer = Cha.reduceHealth Item.fistDmg player
                  if Cha.isDead newPlayer
                     then return . Left $ "You were dealt one blow too many by that vicious " ++ Cha.name enemy ++ ". You have perished.\n"
                     else return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
               Just oldShield -> do
                  let newShield = Item.reduceDur Item.fistDmg oldShield
                  if Item.isDestroyed newShield
                     then do
                        let newPlayer = Cha.dropItem oldShield player
                        return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
                     else do
                        let newPlayer = Cha.modifyInv True oldShield newShield player
                        return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
            Just (_, _, Item.Weapon dmg _) -> case playerShield of
               Nothing -> do
                  let newPlayer = Cha.reduceHealth dmg player
                  if Cha.isDead newPlayer
                     then return . Left $ "You were dealt one blow too many by that vicious " ++ Cha.name enemy ++ ". You have perished.\n"
                     else return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
               Just oldShield -> do
                  let newShield = Item.reduceDur dmg oldShield
                  if Item.isDestroyed newShield
                     then do
                        let newPlayer = Cha.dropItem oldShield player
                        return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
                     else do
                        let newPlayer = Cha.modifyInv True oldShield newShield player
                        return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList) 
      else if playerPos `Mov.inLOS` oldEnemyPos -- the player isn't directly in front of the enemy, but still in its line of sight
         then if Cha.name enemy == "Berserker" -- berserkers gonna berserk
            then enemyAction "sprint forward" (enemy, oldEnemyPos) oldGame
            else case enemyWeapon of
               Nothing -> enemyAction "random action" (enemy, oldEnemyPos) oldGame -- no weapon means no way they can reach the player
               Just (_, _, Item.Weapon dmg range) -> if (playerPos `Mov.distanceTo` oldEnemyPos) <= range -- check if the range is long enough
                     then case playerShield of
                        Nothing -> do
                           let newPlayer = Cha.reduceHealth dmg player
                           if Cha.isDead newPlayer
                              then return . Left $ "You were dealt one blow too many by that vicious " ++ Cha.name enemy ++ ". You have perished.\n"
                              else return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
                        Just oldShield -> do
                           let newShield = Item.reduceDur dmg oldShield
                           if Item.isDestroyed newShield
                              then do
                                 let newPlayer = Cha.dropItem oldShield player
                                 return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
                              else do
                                 let newPlayer = Cha.modifyInv True oldShield newShield player
                                 return $ Right (narratorstr, roomMap, winPos, (newPlayer, playerPos), (enemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
                     else enemyAction "random action" (enemy, oldEnemyPos) oldGame -- if it isn't, do something else
      else enemyAction "random action" (enemy, oldEnemyPos) oldGame -- if the enemy doesn't see a player, there's no point in attacking
   --
   -- Picking up items
   | str == "pickup item" = do
      -- get a list of the items at the current location that aren't keys
      let placeItemList = filter (\(x, _) -> not (Item.isKey x)) . filter (\(_, x) -> x `hasWon` oldEnemyPos) $ itemList
      if null placeItemList
         then enemyAction "random action" (enemy, oldEnemyPos) oldGame
         else do
            let (newItem, _) = head placeItemList
            let newEnemy = Cha.pickupItem False newItem enemy
            return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), filter (/= (newItem, oldEnemyPos)) itemList)
   --
   -- Dropping the currently equipped weapon
   | str == "drop weapon" = do
      let enemyWeapon = Cha.equippedWeapon enemy
      case enemyWeapon of
         Nothing -> enemyAction "random action" (enemy, oldEnemyPos) oldGame
         Just weapon -> do
            let newEnemy = Cha.dropItem weapon enemy
            return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), (weapon, oldEnemyPos) : itemList)
   --
   -- Dropping the currently equipped shield
   | str == "drop shield" = do
      let enemyShield = Cha.equippedShield enemy
      case enemyShield of
         Nothing -> enemyAction "random action" (enemy, oldEnemyPos) oldGame
         Just shield -> do
            let newEnemy = Cha.dropItem shield enemy
            return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), (shield, oldEnemyPos) : itemList)
   | str == "swap weapon" = if Cha.numWeapons enemy < 2
      then enemyAction "random action" (enemy, oldEnemyPos) oldGame
      else do
         let unUsedWeaponList = map fst . filter (not . snd) $ Cha.weaponList enemy
         gen <- newStdGen
         let newWeapon = (unUsedWeaponList!!) . head . randomRs (0, length unUsedWeaponList - 1) $ gen
         let oldWeapon = Cha.equippedWeapon enemy
         case oldWeapon of
            Nothing -> do
               let newEnemy = Cha.setEquipped True newWeapon enemy
               return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
            Just weapon -> do
               let newEnemy = Cha.setEquipped True newWeapon . Cha.setEquipped False weapon $ enemy
               return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
   | str == "swap shield" = if Cha.numShields enemy < 2
      then enemyAction "random action" (enemy, oldEnemyPos) oldGame
      else do
         let unUsedShieldList = map fst . filter (not . snd) $ Cha.shieldList enemy
         gen <- newStdGen
         let newShield = (unUsedShieldList!!) . head . randomRs (0, length unUsedShieldList - 1) $ gen
         let oldShield = Cha.equippedShield enemy
         case oldShield of
            Nothing -> do
               let newEnemy = Cha.setEquipped True newShield enemy
               return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
            Just shield -> do
               let newEnemy = Cha.setEquipped True newShield . Cha.setEquipped False shield $ enemy
               return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)




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
