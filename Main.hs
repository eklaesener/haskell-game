import Data.Array (listArray, (!))
import Data.Char (toLower)
import Data.List (minimumBy, nubBy)
import Data.Maybe (isJust, isNothing)
import Data.Either (fromRight)
import System.IO
import System.Exit
import System.Posix.Process (exitImmediately)
import System.Random
import Control.Concurrent (threadDelay, forkFinally)
--import Control.Concurrent.MVar.Strict
import Control.Concurrent.MVar
import qualified Control.Monad.Random as Rand
import qualified Movement as Mov
import qualified Character as Cha
import qualified Messages as Msg
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
      if any (\(_, x) -> comparePos x pos) enemies -- checks if there already is another enemy at that position
         then createEnemies n checkForLocked roomMap enemies
         else return $ enemyWithPos : enemies  -}
   | otherwise = do
      enemyWithPos@(_, pos) <- createCharacter checkForLocked roomMap
      if any (\(_, x) -> comparePos x pos) enemies
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
createItem :: Bool -> Mov.Map -> Items -> IO Item
createItem checkForLocked roomMap items = do
   let keyList = filter (\(x, _) -> Item.isKey x) items
   gen <- newStdGen
   let item@(_, _, attr) = (Item.invItemList!!) . head $ randomRs (0, length Item.invItemList - 1) gen
   pos@(itemRoom, _, _) <- randomPosition checkForLocked roomMap
   case attr of
      Item.Key {Item.room = keyRoom} -- if the item is a key, some further checks are necessary
         -- the key was spawned in the room that it unlocks, rendering it inaccessible
         | itemRoom == keyRoom -> createItem checkForLocked roomMap items
         -- the key is useless, because the room it unlocks already is unlocked
         | not (roomMap ! keyRoom) -> createItem checkForLocked roomMap items
         -- the key already exists
         | any (\((_, _, Item.Key x), _) -> x == keyRoom) keyList -> createItem checkForLocked roomMap items
      _ -> return (item, pos)

-- creates a list of new items with length n
createItems :: Int -> Bool -> Mov.Map -> Items -> IO Items
createItems n checkForLocked roomMap items
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return items
   | otherwise = do
      itemWithPos@(_, pos) <- createItem checkForLocked roomMap items
      if any (\(_, x) -> comparePos x pos) items
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
randomPosition checkForLocked roomMap = do
   gen <- newStdGen
   let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen -- get a random Mov.InnerLocation
   gen2 <- newStdGen
   let dir = head $ randoms gen2 :: Mov.Direction -- get a random Mov.Direction
   if checkForLocked
      then do
         gen3 <- newStdGen
         let loc = head . filter (\x -> not (roomMap ! x)) . randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) $ gen3 -- get a random unlocked room
         let newPos = (loc, innerLoc, dir)
         return newPos
      else do
         gen3 <- newStdGen
         let loc = head $ randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen3 :: (Int, Int) -- get a random room
         let newPos = (loc, innerLoc, dir)
         return newPos






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



getMapKey :: String
getMapKey =  "Key:\nYour position: ⯅ ⯈ ⯆ ⯇\n"
          ++ "The doors: ▯\n"
          ++ "The ladder: ☷\n"
          ++ "And the cave-in: ⭙\n"


-- compares the two given positions, but disregards the direction (since it's not important here)
infix 4 `comparePos`
comparePos :: Mov.Position -> Mov.Position -> Bool
comparePos (room1, inner1, _) (room2, inner2, _) = room1 == room2 && inner1 == inner2


-- creates an empty Game so playerAction "help" can be run
emptyGame :: Mov.Map -> Game
emptyGame roomMap = ("", roomMap, Mov.nullPosition, (Cha.nullCharacter, Mov.nullPosition), [], (("", False, Item.Nil), Mov.nullPosition), [])





-- sets the starting parameters for the game like positions and control settings
initialize :: IO (MVar Game)
initialize = do
   roomMap <- createMap
   let empty = emptyGame roomMap
   (Right (narrStr, _, _, _, _, _, _)) <- playerAction "help" empty
   putStrLn narrStr
   playerWithPos@(player, winPosition) <- createPlayer roomMap
   gen <- newStdGen
   gen2 <- getStdGen
   let numEnemies = head $ randomRs (0, Mov.numRooms * 2) gen
   enemies <- createEnemies numEnemies False roomMap []
   let numItems = head $ randomRs (0, Mov.numRooms * 3) gen2
   items <- createItems numItems False roomMap []
   ladderWithPos <- createLadder True roomMap
   putStr $ "\nWell, " ++ Cha.name player
   putStrLn ", you're in quite a pickle right now. Remember? You were exploring a cave, but the floor you were standing on fell down and you with it... Maybe there's a ladder here somewhere? And are those shrieks I hear?"
   putStrLn "Press Enter to start..."
   _ <- getLine
   newMVar ("", roomMap, winPosition, playerWithPos, enemies, ladderWithPos, items)


-- uses the module Draw to create a representation of the room's contents on the command line
drawMap :: Game -> IO ()
drawMap (narratorStr, roomMap, winPos@(winRoom, winInner, _), (player, (playerRoom, playerInner, playerDir)), enemyList, (_, ladderPos@(ladderRoom, ladderInner, _)), itemList) = do
   let playerHP = Cha.hp player
   let playerInv = Cha.inv player
   let equipPlayer = filter snd playerInv
   let tempItemList = nubBy (\(_, x) (_, y) -> x == y) itemList
   let tempRoomContents = filter (\((x, _, _), _) -> x == playerRoom) $ map convertItem tempItemList ++ map convertEnemy enemyList
   let roomContents = filter (\((x, _, _), _) -> x == playerRoom) [(ladderPos, Draw.ladder), (winPos, Draw.win)] ++ tempRoomContents
   let drawList = (playerInner, Draw.player playerDir) : map (\((_, x, _), str) -> (x, str)) roomContents
   Draw.draw narratorStr player drawList
  where
   convertItem ((_, _, Item.Weapon _ _), pos) = (pos, Draw.weapon)
   convertItem ((_, _, Item.Shield _), pos) = (pos, Draw.shield)
   convertItem ((_, _, Item.Key _), pos) = (pos, Draw.key)
   convertEnemy (enemy, pos) = (pos, Draw.enemy $ Cha.name enemy)




quit :: IO ()
quit = do
   hSetEcho stdin True
   hSetBuffering stdin LineBuffering
   putStrLn "\n"
   exitImmediately ExitSuccess


-- tracks the state of the game, returns only if you've won or lost
gameState :: MVar Game -> IO ()
gameState gameVar = do
   forkFinally (enemyGameState gameVar) (\x -> exitSuccess)
   playerGameState gameVar
   


playerGameState :: MVar Game -> IO ()
playerGameState gameVar = do
   tempGame <- readMVar gameVar
   putStrLn . take 70 $ repeat '\n'
   drawMap tempGame
   isNewInput <- hWaitForInput stdin 500
   if isNewInput
      then do
         input <- getChar
         game <- takeMVar gameVar
         playerResultUnformatted <- playerAction (shortInput input) game
         case playerResultUnformatted of
            Left str -> do
               putStrLn $ replicate 90 '\n'
               putStrLn str
               quit
            Right newGame -> putMVar gameVar newGame >> playerGameState gameVar
      else playerGameState gameVar
   

enemyGameState :: MVar Game -> IO ()
enemyGameState gameVar = do
   game <- takeMVar gameVar
   enemyResultUnformatted <- cycleEnemies game
   case enemyResultUnformatted of
      Left str -> do
         putStrLn $ replicate 90 '\n'
         putStrLn str
         quit
      Right newGame -> do
         putMVar gameVar newGame
         threadDelay 500000
         enemyGameState gameVar


-- cycles through the list of enemies, performing one action each
cycleEnemies :: Game -> IO (Either String Game)
cycleEnemies game@(_, _, _, _, enemyList, _, _)
   | null enemyList = do
      return $ Right game
   | otherwise = do
      helper (head enemyList) (tail enemyList) game
  where
   helper enemyWithPos rest oldGame@(_, roomMap, winPos, _, _, ladderWithPos, _)
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
               let newGame = (newNarrStr, roomMap, winPos, newPlayerWithPos, newEnemyList, ladderWithPos, newItemList)
               helper (head rest) (tail rest) newGame


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
   ' ' -> "attack"
   'i' -> "pickup item"
   'k' -> "drop weapon"
   'j' -> "drop shield"
   'l' -> "swap weapon"
   'ö' -> "swap shield"
   'h' -> "help"
   'q' -> "quit"
   _ -> "unknown" -- there isn't actually a match for unknown, so it will just fall through to the catch-all
  where inputCaseIns = toLower input



-- decide if the action is allowed and if it is, perform it
playerAction :: String -> Game -> IO (Either String Game)
playerAction str oldGame@(narratorstr, roomMap, winPos, (player, oldPlayerPos@(_, oldPlayerInner, _)), enemyList, (ladder, oldLadderPos@(_, _, ladderDir)), itemList)
   | str == "go forward" = do
      let result = Mov.move oldPlayerPos Mov.Advance
      case result of
         Left resStr -- the move function has caught something to be handled here
            | resStr == "Wall" -> do
               newNaStr <- Msg.getWallMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            | resStr == "Door blocked" -> do
               newNaStr <- Msg.getDoorBlockedMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         Right newPlayerPos@(newPlayerRoom, newPlayerInner, _)
            | roomMap ! newPlayerRoom && not (Cha.hasKey newPlayerRoom player) -> do -- if the new room is locked and the player doesn't have the right key, don't allow the move
               newNaStr <- Msg.getRoomLockedMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            -- we won't collide with anything, so everything is alright
            | not (any (newPlayerPos `comparePos`) (oldLadderPos : map snd enemyList)) -> return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            -- we did collide with something, if it's an enemy, make him attack the player or move
            |  any (newPlayerPos `comparePos`) $ map snd enemyList -> do
               let enemyWithPos = head . filter (\(_, x) -> newPlayerPos `comparePos` x) $ enemyList
               enemyAction "attack" enemyWithPos oldGame
            -- we collided with the ladder
            | otherwise -> do -- we're okay to move, but need to check the same things as before
               let result2 = Mov.move newPlayerPos Mov.Advance
               case result2 of
                  Left resStr
                     | resStr == "Wall" -> do
                        newNaStr <- Msg.getWallPushMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                     | resStr == "Door blocked" -> do
                        newNaStr <- Msg.getDoorBlockedMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                  Right newLadderPos@(newLadderRoom, newLadderInner, _)
                     | roomMap ! newLadderRoom && not (Cha.hasKey newLadderRoom player) -> do
                        newNaStr <- Msg.getRoomLockedMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                     | any (newLadderPos `comparePos`) $ map snd enemyList -> do
                        newNaStr <- Msg.getEnemyPushMsg
                        return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
                     | comparePos newLadderPos winPos -> return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n"
                     | Mov.isCorner newLadderInner -> return $ Left "You've actually managed to maneuver the ladder into an unrecoverable location. Great Job. Guess you're not going to escape this cavern after all...\n"
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
            failMsg <- Msg.getOutOfReachMsg $ Cha.name nextEnemy
            winMsg <- Msg.getVictorMsg $ Cha.name nextEnemy
            hitMsg <- Msg.getHitMsg $ Cha.name nextEnemy
            shieldHitMsg <- Msg.getShieldHitMsg $ Cha.name nextEnemy
            shieldDestroyedMsg <- Msg.getShieldDestroyedMsg $ Cha.name nextEnemy
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
                  Just (_, _, Item.Weapon dmg _) -> case enemyShield of
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
      failStr <- Msg.getNoItemHereMsg
      let placeItemList = filter (\(_, x) -> x `comparePos` oldPlayerPos) itemList
      if null placeItemList
         then return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         else do
            let (newItem, itemPos) = head placeItemList
            let newPlayer = Cha.pickupItem False newItem player
            successStr <- Msg.getItemPickedUpMsg $ Item.name newItem
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), filter (/= (newItem, itemPos)) itemList)
   --
   -- Dropping the currently equipped weapon
   | str == "drop weapon" = do
      failStr <- Msg.getNoWeaponMsg
      let playerWeapon = Cha.equippedWeapon player
      case playerWeapon of
         Nothing -> return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         Just weapon -> do
            successStr <- Msg.getWeaponDroppedMsg $ Item.name weapon
            let newPlayer = Cha.dropItem weapon player
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), (weapon, oldPlayerPos) : itemList)
   --
   -- Dropping the currently equipped shield
   | str == "drop shield" = do
      failStr <- Msg.getNoShieldMsg
      let playerShield = Cha.equippedShield player
      case playerShield of
         Nothing -> return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
         Just shield -> do
            successStr <- Msg.getShieldDroppedMsg $ Item.name shield
            let newPlayer = Cha.dropItem shield player
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), (shield, oldPlayerPos) : itemList)
   --
   -- Swapping the currently equipped weapon with the next available one
   | str == "swap weapon" = if Cha.numWeapons player == 0 || (Cha.numWeapons player == 1 && isJust (Cha.equippedWeapon player))
      then do
         failStr <- Msg.getWeaponSwapNotEnoughMsg
         return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos),  enemyList, (ladder, oldLadderPos), itemList)
      else do
         let unUsedWeaponList = map fst . filter (not . snd) $ Cha.weaponList player
         let newWeapon = head unUsedWeaponList
         let oldWeapon = Cha.equippedWeapon player
         case oldWeapon of
            Nothing -> do
               let newPlayer = Cha.setEquipped True newWeapon player
               successStr <- Msg.getWeaponSwapOneMsg $ Item.name newWeapon
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            Just weapon -> do
               let newPlayer = Cha.setEquipped True newWeapon . Cha.setEquipped False weapon $ player
               successStr <- Msg.getWeaponSwapTwoMsg (Item.name weapon) (Item.name newWeapon)
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   --
   -- The same for the shield
   | str == "swap shield" = if Cha.numShields player == 0 || (Cha.numShields player == 1 && isJust (Cha.equippedShield player))
      then do
         failStr <- Msg.getShieldSwapNotEnoughMsg
         return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos),  enemyList, (ladder, oldLadderPos), itemList)
      else do
         let unUsedShieldList = map fst . filter (not . snd) $ Cha.shieldList player
         let newShield = head unUsedShieldList
         let oldShield = Cha.equippedShield player
         case oldShield of
            Nothing -> do
               let newPlayer = Cha.setEquipped True newShield player
               successStr <- Msg.getShieldSwapOneMsg $ Item.name newShield
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
            Just shield -> do
               let newPlayer = Cha.setEquipped True newShield . Cha.setEquipped False shield $ player
               successStr <- Msg.getShieldSwapTwoMsg (Item.name shield) (Item.name newShield)
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)

   --
   -- Getting a list of commands:
   | str == "help" = do
      let str =    "Possible commands:\n\n"
                   ++ "w for going forward\n"
                   ++ "d for going right\n"
                   ++ "s for going back\n"
                   ++ "a for going left\n\n"
                   ++ "f for turning right\n"
                   ++ "r for turning around\n"
                   ++ "e for turning left\n\n"
                   ++ "space for attacking\n\n"
                   ++ "i for picking up items\n"
                   ++ "k for dropping your weapon\n"
                   ++ "j for dropping your shield\n"
                   ++ "l for swapping your weapon\n"
                   ++ "ö for swapping your shield\n"
                   ++ "h for help\n"
                   ++ "q for quitting\n\n"
                   ++ getMapKey
      putStrLn $ str ++ "\nPress Enter to continue."
      _ <- getLine
      return $ Right (narratorstr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)
   --
   -- Quitting:
   | str == "quit" = return $ Left "You successfully quit the game. Bye!"
   --
   -- Unknown commands:
   | otherwise = do
      let tempStr = "This command was not recognized! "
      Right (tempNaStr, _, _, _, _, _, _) <- playerAction "help" oldGame
      return $ Right (tempStr ++ tempNaStr, roomMap, winPos, (player, oldPlayerPos), enemyList, (ladder, oldLadderPos), itemList)


-- and now the possible actions for enemies
enemyAction :: String -> Enemy -> Game -> IO (Either String Game)
enemyAction str (enemy, oldEnemyPos@(oldEnemyRoom, _, _)) oldGame@(narratorstr, roomMap, winPos, (player, playerPos), enemyList, (ladder, ladderPos), itemList)
   | str == "go forward" = do -- much the same as above
      let result = Mov.move oldEnemyPos Mov.Advance
      case result of
         Left resStr
            | resStr == "Wall" -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
            | resStr == "Door blocked" -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
         Right newEnemyPos@(newEnemyRoom, _, _)
            -- TODO: decide if enemies should be able to enter locked rooms
            | roomMap ! newEnemyRoom && not (roomMap ! oldEnemyRoom) -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
            -- makes sure there isn't anything on the new position
            | not (any (newEnemyPos `comparePos`) (ladderPos : playerPos : map snd enemyList)) -> return $
               Right (narratorstr, roomMap, winPos, (player, playerPos), (enemy, newEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), itemList)
            -- checks if the obstacle is the player
            | comparePos newEnemyPos playerPos -> enemyAction "attack" (enemy, oldEnemyPos) oldGame
            | otherwise -> enemyAction "turn randomly" (enemy, oldEnemyPos) oldGame
   | str == "sprint forward" = do
      tempGameRes <- enemyAction "go forward" (enemy, oldEnemyPos) oldGame
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right tempGame@(_, _, _, _, (newEnemy, newEnemyPos) : _, _, _) -> if comparePos oldEnemyPos newEnemyPos
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
            then enemyAction "go forward" (enemy, oldEnemyPos) oldGame
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
      let placeItemList = filter (\(x, _) -> not (Item.isKey x)) . filter (\(_, x) -> x `comparePos` oldEnemyPos) $ itemList
      if null placeItemList
         then enemyAction "random action" (enemy, oldEnemyPos) oldGame
         else do
            let (newItem, itemPos) = head placeItemList
            let newEnemy = Cha.pickupItem False newItem enemy
            return $ Right (narratorstr, roomMap, winPos, (player, playerPos), (newEnemy, oldEnemyPos) : filter (/= (enemy, oldEnemyPos)) enemyList, (ladder, ladderPos), filter (/= (newItem, itemPos)) itemList)
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
   --
   -- choose a random action
   | str == "random action" = do
      gen <- newStdGen
      let randOption = head . randomRs (1 :: Int, 13) $ gen
      case randOption of
         1 -> enemyAction "go forward" (enemy, oldEnemyPos) oldGame
         2 -> enemyAction "go back" (enemy, oldEnemyPos) oldGame
         3 -> enemyAction "go left" (enemy, oldEnemyPos) oldGame
         4 -> enemyAction "go right" (enemy, oldEnemyPos) oldGame
         5 -> enemyAction "turn right" (enemy, oldEnemyPos) oldGame
         6 -> enemyAction "turn around" (enemy, oldEnemyPos) oldGame
         7 -> enemyAction "turn left" (enemy, oldEnemyPos) oldGame
         8 -> enemyAction "attack" (enemy, oldEnemyPos) oldGame
         9 -> enemyAction "pickup item" (enemy, oldEnemyPos) oldGame
         10 -> enemyAction "drop weapon" (enemy, oldEnemyPos) oldGame
         11 -> enemyAction "drop shield" (enemy, oldEnemyPos) oldGame
         12 -> enemyAction "swap weapon" (enemy, oldEnemyPos) oldGame
         13 -> enemyAction "swap shield" (enemy, oldEnemyPos) oldGame




main :: IO ()
main = do 
   gameVar <- initialize
   hSetEcho stdin False
   hSetBuffering stdin NoBuffering
   gameState gameVar
