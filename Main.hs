import Data.Array (listArray, (!), assocs)
import Data.Char (toLower)
import Data.List (minimumBy, nubBy)
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HM

import System.IO (stdin, hWaitForInput, hSetEcho, hSetBuffering, BufferMode(..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Exit (ExitCode(..))
import System.Posix.Process (exitImmediately)
import System.Random (Random(..), RandomGen, newStdGen, getStdGen)

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, takeMVar, putMVar)
import qualified Control.Monad.Random as Rand (evalRand, fromList)

import qualified Debug.Trace as Trace

import qualified Movement as Mov
import qualified Character as Cha
import qualified Messages as Msg
import qualified Config as Cfg (config, initConfig, Config(..))
import qualified Draw
import qualified Item


-- defining some type synonyms for easier tracking
type Character = (Cha.Character, Mov.Position)
type Player = Character
type Enemy = Character
type Enemies = HM.HashMap Mov.Location [Enemy]

type Item = (Item.Item, Mov.Position)
type Ladder = Item
type Items = HM.HashMap Mov.Location [Item]

type Game = (String, Mov.Map, Mov.Position, Player, Enemies, Ladder, Items)




-- get the needed config values
-- we can use unsafeDupablePerformIO here because we only intend to read from the config values, not to change them

config :: Cfg.Config
config = unsafeDupablePerformIO Cfg.config

lockedRooms :: Rational
lockedRooms = realToFrac . Cfg.lockedRooms $ config

startingHealth :: Float
startingHealth = Cfg.startingHP config

numEnemyRange :: (Int, Int)
numEnemyRange = Cfg.enemyRange config

numItemRange :: (Int, Int)
numItemRange = Cfg.itemRange config

timeScreen :: Int
timeScreen = Cfg.timeScreen config

-- we need to multiply by 1000 here so that the value is in microseconds, which is what threadDelay expects
timeEnemy :: Int
timeEnemy = 1000 * Cfg.timeEnemies config



-- will be used for creating and modifying the hashmaps
addHashMap :: [(a, Mov.Position)] -> [(a, Mov.Position)] -> [(a, Mov.Position)]
addHashMap x y = x ++ y



-- creates weighted random lists to create a map where comparatively few rooms are locked
weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weightList = Rand.evalRand m gen
   where m = sequence . repeat . Rand.fromList $ weightList


-- creates a new (room) map with randomly locked rooms
createMap :: IO Mov.Map
createMap = do
   gen <- newStdGen
   let weightList = [(True, lockedRooms), (False, 10 - lockedRooms)]
   let randomBools = take Mov.numRooms (weightedList gen weightList)
   return (listArray ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) randomBools)


-- creates a new character
createCharacter :: Bool -> Mov.Map -> IO Character
createCharacter checkForLocked roomMap = do
   name <- getEnemyName
   let char = Cha.Character name startingHealth False []
   pos <- randomPosition checkForLocked roomMap
   return (char, pos)


createEnemies :: Int -> Bool -> Mov.Map -> Enemies -> IO Enemies
createEnemies n checkForLocked roomMap enemyMap
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return enemyMap
   | otherwise = do
      enemyWithPos@(_, (loc, _, _)) <- createCharacter checkForLocked roomMap
      let newEnemyMap = HM.insertWith addHashMap loc [enemyWithPos] enemyMap
      createEnemies (n - 1) checkForLocked roomMap newEnemyMap


-- creates a new player character
createPlayer :: Mov.Map -> IO Player
createPlayer roomMap = do
   (char, pos) <- createCharacter True roomMap
   putStrLn "What is your name?"
   name <- getLine
   let player = Cha.setPlayerCharacter True . Cha.setName name $ char
   return (player, pos)


-- creates a new, random item that isn't a ladder or a key
createItem :: Bool -> Mov.Map -> IO Item
createItem checkForLocked roomMap = do
   gen <- newStdGen
--   let item = Trace.trace ". \n" . Trace.traceShowId . (list!!) . Trace.trace "with resulting item " . Trace.traceShowId . Trace.trace "Using function createItem with index " . head $ randomRs (0, length list - 1) gen
   let item = (list!!) . head $ randomRs (0, length list - 1) gen
   pos <- randomPosition checkForLocked roomMap
   return (item, pos)
 where
   list = filter (not . Item.isKey) Item.invItemList

createItems :: Int -> Bool -> Mov.Map -> Items -> IO Items
createItems n checkForLocked roomMap itemMap
   | n < 0 = error $ "Negative value for n: " ++ show n
   | n == 0 = return itemMap
   | otherwise = do
      itemWithPos@(_, (loc, _, _)) <- createItem checkForLocked roomMap
      let newItemMap = HM.insertWith addHashMap loc [itemWithPos] itemMap
      createItems (n - 1) checkForLocked roomMap newItemMap


createKeys :: Bool -> Mov.Map -> Items -> IO Items
createKeys checkForLocked roomMap = helper (map fst . filter snd . assocs $ roomMap)
  where
   helper [] itemMap = return itemMap
   helper (room : rest) itemMap = do
      pos@(loc, _, _) <- randomPosition checkForLocked roomMap
      if loc == room
         then helper (room : rest) itemMap
         else do
            let key = Item.genKey room
            let newItemMap = HM.insertWith addHashMap loc [(key, pos)] itemMap
            helper rest newItemMap


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
   let innerLoc = head $ randomRs ((Mov.lowInnerBoundNS, Mov.lowInnerBoundWE), (Mov.highInnerBoundNS, Mov.highInnerBoundWE)) gen -- get a random Mov.InnerLocation
   gen2 <- newStdGen
   let dir = head $ randoms gen2 :: Mov.Direction -- get a random Mov.Direction
   gen3 <- newStdGen
   if checkForLocked
      then do
         let loc = head . filter (\x -> not (roomMap ! x)) . randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) $ gen3 -- get a random unlocked room
         let newPos = (loc, innerLoc, dir)
         return newPos
      else do
         let loc = head $ randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen3 :: (Int, Int) -- get a random room
         let newPos = (loc, innerLoc, dir)
         return newPos


insertEnemy :: Enemy -> Enemies -> Enemies
insertEnemy enemy@(_, (loc, _, _)) = HM.adjust (enemy:) loc

updateEnemy :: Enemy -> Enemy -> Enemies -> Enemies
updateEnemy oldEnemy newEnemy = insertEnemy newEnemy . deleteEnemy oldEnemy

deleteEnemy :: Enemy -> Enemies -> Enemies
deleteEnemy enemy@(_, (loc, _, _)) = HM.adjust (filterEnemy False enemy) loc


insertItem :: Item -> Items -> Items
insertItem item@(_, (loc, _, _)) = HM.adjust (item:) loc

deleteItem :: Item -> Items -> Items
deleteItem item@(_, (loc, _, _)) = HM.adjust (Cha.filterFirst (/= item)) loc





-- filters the given enemy out of the enemyList and, if wanted, prepends them to it
filterEnemy :: Bool -> Enemy -> [Enemy] -> [Enemy]
filterEnemy bool enemyWithPos enemyList
   | bool = enemyWithPos : filteredList
   | otherwise = filteredList
  where filteredList = filter (/= enemyWithPos) enemyList



getEnemyName :: IO String
getEnemyName = do
   gen <- newStdGen
--   return . Trace.trace ". \n" . Trace.traceShowId . (enemyNames!!) . Trace.trace "with resulting name " . Trace.traceShowId . Trace.trace "Using function getEnemyName with index " . head $ randomRs (0, length enemyNames - 1) gen
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
          ++ "The ladder: ☷\n\n"
          ++ "The enemies:\n"
          ++ "Berserkers: ᕕ ᕗ ᕓ ᕙ\n"
          ++ "Cave Trolls: ᘯ ᘰ ᘮ ᘳ\n"
          ++ "Ghouls: ᘺ ᘿ ᘻ ᘼ\n"
          ++ "Hobgoblins: ᗑ ᗒ ᗐ ᗕ\n"
          ++ "Orcs: ᕱ ᕲ ᕰ ᕳ\n"
          ++ "Wraiths: ᗅ ᗆ ᗄ ᗉ\n\n"
          ++ "Items:\n"
          ++ "Weapons: 🗡️\n"
          ++ "Shields: 🛡\n"
          ++ "Keys: 🔑\n\n"
          ++ "And the cave-in: ⭙\n"


-- compares the two given positions, but disregards the direction (since it's not important here)
infix 4 `comparePos`
comparePos :: Mov.Position -> Mov.Position -> Bool
comparePos (room1, inner1, _) (room2, inner2, _) = room1 == room2 && inner1 == inner2


-- creates an empty Game so playerAction "help" can be run
emptyGame :: Mov.Map -> Game
emptyGame roomMap = ("", roomMap, Mov.nullPosition, (Cha.nullCharacter, Mov.nullPosition), HM.empty, (("", False, Item.Nil), Mov.nullPosition), HM.empty)





-- sets the starting parameters for the game like monsters and items
initialize :: IO (MVar Game)
initialize = do
   _ <- Cfg.initConfig -- display warnings if any config options aren't accessible
   roomMap <- createMap
   let empty = emptyGame roomMap
   _ <- playerAction "help" empty
   playerWithPos@(player, winPosition) <- createPlayer roomMap
   gen <- newStdGen
   gen2 <- getStdGen
   let numEnemies = head $ randomRs numEnemyRange gen
   enemyMap <- createEnemies numEnemies False roomMap HM.empty
   let numItems = head $ randomRs numItemRange gen2
   tempItemMap <- createItems numItems False roomMap HM.empty
   itemMap <- createKeys False roomMap tempItemMap
   ladderWithPos <- createLadder True roomMap
   putStrLn $ Msg.introMsg (Cha.name player)
   putStrLn "Press Enter to start..."
   _ <- getLine
   newMVar ("", roomMap, winPosition, playerWithPos, enemyMap, ladderWithPos, itemMap)


-- uses the module Draw to create a representation of the room's contents on the command line
drawMap :: Game -> IO ()
drawMap (narratorStr, _, winPos, (player, (playerRoom, playerInner, playerDir)), enemyMap, (_, ladderPos), itemMap) = do
   -- get the items in the current room
   let itemList = HM.lookupDefault [] playerRoom itemMap
   let tempItemList = nubBy (\(_, x) (_, y) -> x == y) itemList
   -- get the enemies in the current room
   let enemyList = HM.lookupDefault [] playerRoom enemyMap
   let tempRoomContents = map convertItem tempItemList ++ map convertEnemy enemyList
   -- check if the ladder and/or the win position are in the current room
   let roomContents = filter (\((x, _, _), _) -> x == playerRoom) [(ladderPos, Draw.ladder), (winPos, Draw.win)] ++ tempRoomContents
   let drawList = (playerInner, Draw.player playerDir) : map (\((_, x, _), str) -> (x, str)) roomContents
   Draw.draw narratorStr player drawList
  where
   convertItem ((_, _, Item.Weapon _ _), pos) = (pos, Draw.weapon)
   convertItem ((_, _, Item.Shield _), pos) = (pos, Draw.shield)
   convertItem ((_, _, Item.Key _), pos) = (pos, Draw.key)
   convertEnemy (enemy, pos@(_, _, dir)) = (pos, Draw.enemy (Cha.name enemy) dir)




quit :: String -> IO ()
quit str = do
   hSetEcho stdin True
   hSetBuffering stdin LineBuffering
   putStrLn $ replicate 90 '\n'
   putStrLn $ str ++ "\n"
   exitImmediately ExitSuccess


-- tracks the state of the game, returns only if you've won or lost
gameState :: MVar Game -> IO ()
gameState gameVar = do
   _ <- forkIO (enemyGameState gameVar)
   playerGameState gameVar
   

-- the player thread
playerGameState :: MVar Game -> IO ()
playerGameState gameVar = do
   tempGame <- readMVar gameVar
   putStrLn . take 70 $ repeat '\n'
   drawMap tempGame
   isNewInput <- hWaitForInput stdin timeScreen
   if isNewInput
      then do
         input <- getChar
         game <- takeMVar gameVar
         playerResultUnformatted <- playerAction (shortInput input) game
         case playerResultUnformatted of
            Left str -> quit str
            Right newGame -> putMVar gameVar newGame >> playerGameState gameVar
      else playerGameState gameVar
   

-- the enemy thread
enemyGameState :: MVar Game -> IO ()
enemyGameState gameVar = do
   game <- takeMVar gameVar
   enemyResultUnformatted <- cycleEnemies game
   case enemyResultUnformatted of
      Left str -> quit str
      Right newGame -> do
         putMVar gameVar newGame
         threadDelay timeEnemy
         enemyGameState gameVar


-- cycles through the list of enemies, performing one action each
cycleEnemies :: Game -> IO (Either String Game)
cycleEnemies game@(_, _, _, _, enemyMap, _, _)
   | null enemyList = return $ Right game
   | otherwise = helper (head enemyList) (tail enemyList) game
  where
   enemyList = concat $ HM.elems enemyMap
   helper enemyWithPos rest oldGame@(_, roomMap, winPos, _, _, ladderWithPos, _)
      | null rest = do
         result <- enemyAction "attack" enemyWithPos oldGame
         case result of
            Left str -> return $ Left str
            Right ((newNarrStr, _, _, newPlayerWithPos, tempEnemyMap, _, newItemMap), newEnemyWithPos) -> do
               let newEnemyMap = updateEnemy enemyWithPos newEnemyWithPos tempEnemyMap
               return $ Right (newNarrStr, roomMap, winPos, newPlayerWithPos, newEnemyMap, ladderWithPos, newItemMap)
      | otherwise = do
         result <- enemyAction "attack" enemyWithPos oldGame
         case result of
            Left str -> return $ Left str
            Right ((newNarrStr, _, _, newPlayerWithPos, tempEnemyMap, _, newItemMap), newEnemyWithPos) -> do
               let newEnemyMap = updateEnemy enemyWithPos newEnemyWithPos tempEnemyMap
               let newGame = (newNarrStr, roomMap, winPos, newPlayerWithPos, newEnemyMap, ladderWithPos, newItemMap)
               helper (head rest) (tail rest) newGame


-- translates the keys into playerAction strings
shortInput :: Char -> String
shortInput input = case inputCaseIns of
   'w' -> "go forward"
   'd' -> "go right"
   's' -> "go back"
   'a' -> "go left"
   'e' -> "turn right"
   'r' -> "turn around"
   'q' -> "turn left"
   ' ' -> "attack"
   'i' -> "pickup item"
   'k' -> "drop weapon"
   'j' -> "drop shield"
   'l' -> "swap weapon"
   'ö' -> "swap shield"
   'h' -> "help"
   '\ESC' -> "quit"
   _ -> "unknown" -- there isn't actually a match for unknown, so it will just fall through to the catch-all
  where inputCaseIns = toLower input



-- decide if the action is allowed and if it is, perform it
playerAction :: String -> Game -> IO (Either String Game)
playerAction str oldGame@(narratorstr, roomMap, winPos, (player, oldPlayerPos@(oldPlayerRoom, _, _)), enemyMap, (ladder, oldLadderPos@(_, _, ladderDir)), itemMap)
   | str == "go forward" = do
      let result = Mov.move oldPlayerPos Mov.Advance
      case result of
         Left resStr -- the move function has caught something to be handled here
            | resStr == "Wall" -> do
               newNaStr <- Msg.getWallMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
            | resStr == "Door blocked" -> do
               newNaStr <- Msg.getDoorBlockedMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
         Right newPlayerPos@(newPlayerRoom, _, _) -> do
            let enemyList = HM.lookupDefault [] newPlayerRoom enemyMap
            case () of
             ()
               | roomMap ! newPlayerRoom && not (Cha.hasKey newPlayerRoom player) -> do -- if the new room is locked and the player doesn't have the right key, don't allow the move
               newNaStr <- Msg.getRoomLockedMsg
               return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
               -- we won't collide with anything, so everything is alright
               | not (any (newPlayerPos `comparePos`) (oldLadderPos : map snd enemyList)) -> return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
               -- we did collide with something, if it's an enemy, make him attack the player or move
               | any (newPlayerPos `comparePos`) $ map snd enemyList -> do
                  let enemyWithPos = head . filter (\(_, x) -> newPlayerPos `comparePos` x) $ enemyList
                  result <- enemyAction "attack" enemyWithPos oldGame
                  case result of
                     Left resStr -> return $ Left resStr
                     Right ((_, _, _, newPlayerWithPos, tempEnemyMap, _, newItemMap), newEnemyWithPos) -> do
                        let newEnemyMap = updateEnemy enemyWithPos newEnemyWithPos tempEnemyMap
                        return $ Right (narratorstr, roomMap, winPos, newPlayerWithPos, newEnemyMap, (ladder, oldLadderPos), newItemMap)
               -- we collided with the ladder
               | otherwise -> do -- we're okay to move, but need to check the same things as before
                  let result2 = Mov.move newPlayerPos Mov.Advance
                  case result2 of
                     Left resStr
                        | resStr == "Wall" -> do
                           newNaStr <- Msg.getWallPushMsg
                           return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
                        | resStr == "Door blocked" -> do
                           newNaStr <- Msg.getDoorBlockedMsg
                           return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
                     Right newLadderPos@(newLadderRoom, newLadderInner, _) -> do
                        let tempEnemyList = HM.lookupDefault [] newLadderRoom enemyMap
                        case () of
                         ()
                           | roomMap ! newLadderRoom && not (Cha.hasKey newLadderRoom player) -> do
                              newNaStr <- Msg.getRoomLockedMsg
                              return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
                           | any (newLadderPos `comparePos`) $ map snd tempEnemyList -> do
                              newNaStr <- Msg.getEnemyPushMsg
                              return $ Right (newNaStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
                           | comparePos newLadderPos winPos -> return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't explore caves like this one anymore...\n"
                           | Mov.isCorner newLadderInner -> return $ Left "You've actually managed to maneuver the ladder into an unrecoverable location. Great Job. Guess you're not going to escape this cavern after all...\n"
                           -- update ladder and player positions and return them
                           | otherwise -> return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyMap, (ladder, (newLadderRoom, newLadderInner, ladderDir)), itemMap)
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
      return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
   | str == "turn right" = do
      let (Right newPlayerPos) = Mov.move oldPlayerPos Mov.TurnRight
      return $ Right (narratorstr, roomMap, winPos, (player, newPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
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
   -- Attacking:
   | str == "attack" = do
      let enemyList = HM.lookupDefault [] oldPlayerRoom enemyMap
      let losList = filter (\(_, pos) -> pos `Mov.inLOS` oldPlayerPos) enemyList
      let frontList = filter (\(_, pos) -> pos `Mov.inFrontOf` oldPlayerPos) losList
      if null losList
         then return $ Right ("I don't see anyone standing there, do you?", roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
         else do
            let (nextEnemy, nextEnemyPos) = minimumBy (\(_, x) (_, y) -> compare (x `Mov.distanceTo` oldPlayerPos) (y `Mov.distanceTo` oldPlayerPos)) losList
            let playerWeapon = Cha.equippedWeapon player
            let enemyShield = Cha.equippedShield nextEnemy
            let enemyInv = map fst $ Cha.inv nextEnemy
            failMsg <- Msg.getOutOfReachMsg $ Cha.name nextEnemy
            winMsg <- Msg.getVictorMsg $ Cha.name nextEnemy
            hitMsg <- Msg.getHitMsg $ Cha.name nextEnemy
            shieldHitMsg <- Msg.getShieldHitMsg $ Cha.name nextEnemy
            shieldDestroyedMsg <- Msg.getShieldDestroyedMsg $ Cha.name nextEnemy
            let killEnemyMap = deleteEnemy (nextEnemy, nextEnemyPos) enemyMap
            let killItemMap = HM.adjust (map (\x -> (x, nextEnemyPos)) enemyInv ++) oldPlayerRoom itemMap
            if null frontList
               then case playerWeapon of
                  Nothing -> return $ Right (failMsg, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
                  Just (_, _, Item.Weapon dmg range) -> if (nextEnemyPos `Mov.distanceTo` oldPlayerPos) <= range
                     then case enemyShield of
                        Nothing -> do
                           let newEnemy = Cha.reduceHealth dmg nextEnemy
                           let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                           if Cha.isDead newEnemy
                              then return $ Right (winMsg, roomMap, winPos, (player, oldPlayerPos), killEnemyMap, (ladder, oldLadderPos), killItemMap)
                              else return $ Right (hitMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                        Just oldShield -> do
                           let newShield = Item.reduceDur dmg oldShield
                           if Item.isDestroyed newShield
                              then do
                                 let newEnemy = Cha.dropItem oldShield nextEnemy
                                 let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                                 return $ Right (shieldDestroyedMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                              else do
                                 let newEnemy = Cha.modifyInv True oldShield newShield nextEnemy
                                 let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                                 return $ Right (shieldHitMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                     -- You can't reach the enemy
                     else return $ Right (failMsg, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
               else case playerWeapon of
                  Nothing -> case enemyShield of
                     Nothing -> do
                        let newEnemy = Cha.reduceHealth Item.fistDmg nextEnemy
                        let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                        if Cha.isDead newEnemy
                           then return $ Right (winMsg, roomMap, winPos, (player, oldPlayerPos), killEnemyMap, (ladder, oldLadderPos), killItemMap)
                           else return $ Right (hitMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                     Just oldShield -> do
                        let newShield = Item.reduceDur Item.fistDmg oldShield
                        if Item.isDestroyed newShield
                           then do
                              let newEnemy = Cha.dropItem oldShield nextEnemy
                              let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                              return $ Right (shieldDestroyedMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                           else do
                              let newEnemy = Cha.modifyInv True oldShield newShield nextEnemy
                              let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                              return $ Right (shieldHitMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                  Just (_, _, Item.Weapon dmg _) -> case enemyShield of
                     Nothing -> do
                        let newEnemy = Cha.reduceHealth dmg nextEnemy
                        let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                        if Cha.isDead newEnemy
                           then return $ Right (winMsg, roomMap, winPos, (player, oldPlayerPos), killEnemyMap, (ladder, oldLadderPos), killItemMap)
                           else return $ Right (hitMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                     Just oldShield -> do
                        let newShield = Item.reduceDur dmg oldShield
                        if Item.isDestroyed newShield
                           then do
                              let newEnemy = Cha.dropItem oldShield nextEnemy
                              let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                              return $ Right (shieldDestroyedMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
                           else do
                              let newEnemy = Cha.modifyInv True oldShield newShield nextEnemy
                              let newEnemyMap = updateEnemy (nextEnemy, nextEnemyPos) (newEnemy, nextEnemyPos) enemyMap
                              return $ Right (shieldHitMsg, roomMap, winPos, (player, oldPlayerPos), newEnemyMap, (ladder, oldLadderPos), itemMap)
   --
   -- Picking up items
   | str == "pickup item" = do
      -- get a list of the items at the current location
      failStr <- Msg.getNoItemHereMsg
      let itemList = HM.lookupDefault [] oldPlayerRoom itemMap
      let placeItemList = filter (\(_, x) -> x `comparePos` oldPlayerPos) itemList
      if null placeItemList
         then return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
         else do
            let (newItem, itemPos) = head placeItemList
            let newPlayer = Cha.pickupItem False newItem player
            let newItemMap = deleteItem (newItem, itemPos) itemMap
            successStr <- Msg.getItemPickedUpMsg $ Item.name newItem
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), newItemMap)
   --
   -- Dropping the currently equipped weapon
   | str == "drop weapon" = do
      failStr <- Msg.getNoWeaponMsg
      let playerWeapon = Cha.equippedWeapon player
      case playerWeapon of
         Nothing -> return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
         Just weapon -> do
            successStr <- Msg.getWeaponDroppedMsg $ Item.name weapon
            let newPlayer = Cha.dropItem weapon player
            let newItemMap = insertItem (weapon, oldPlayerPos) itemMap
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), newItemMap)
   --
   -- Dropping the currently equipped shield
   | str == "drop shield" = do
      failStr <- Msg.getNoShieldMsg
      let playerShield = Cha.equippedShield player
      case playerShield of
         Nothing -> return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
         Just shield -> do
            successStr <- Msg.getShieldDroppedMsg $ Item.name shield
            let newPlayer = Cha.dropItem shield player
            let newItemMap = insertItem (shield, oldPlayerPos) itemMap
            return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), newItemMap)
   --
   -- Swapping the currently equipped weapon with the next available one
   | str == "swap weapon" = if Cha.numWeapons player == 0 || (Cha.numWeapons player == 1 && isJust (Cha.equippedWeapon player))
      then do
         failStr <- Msg.getWeaponSwapNotEnoughMsg
         return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
      else do
         let unUsedWeaponList = map fst . filter (not . snd) $ Cha.weaponList player
         let newWeapon = head unUsedWeaponList
         let oldWeapon = Cha.equippedWeapon player
         case oldWeapon of
            Nothing -> do
               let newPlayer = Cha.setEquipped True newWeapon player
               successStr <- Msg.getWeaponSwapOneMsg $ Item.name newWeapon
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
            Just weapon -> do
               let newPlayer = Cha.setEquipped True newWeapon . Cha.setEquipped False weapon $ player
               successStr <- Msg.getWeaponSwapTwoMsg (Item.name weapon) (Item.name newWeapon)
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
   --
   -- The same for the shield
   | str == "swap shield" = if Cha.numShields player == 0 || (Cha.numShields player == 1 && isJust (Cha.equippedShield player))
      then do
         failStr <- Msg.getShieldSwapNotEnoughMsg
         return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
      else do
         let unUsedShieldList = map fst . filter (not . snd) $ Cha.shieldList player
         let newShield = head unUsedShieldList
         let oldShield = Cha.equippedShield player
         case oldShield of
            Nothing -> do
               let newPlayer = Cha.setEquipped True newShield player
               successStr <- Msg.getShieldSwapOneMsg $ Item.name newShield
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)
            Just shield -> do
               let newPlayer = Cha.setEquipped True newShield . Cha.setEquipped False shield $ player
               successStr <- Msg.getShieldSwapTwoMsg (Item.name shield) (Item.name newShield)
               return $ Right (successStr, roomMap, winPos, (newPlayer, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)

   --
   -- Getting a list of commands:
   | str == "help" = do
      let help =   "Possible commands:\n\n"
                   ++ "w for going forward\n"
                   ++ "d for going right\n"
                   ++ "s for going back\n"
                   ++ "a for going left\n\n"
                   ++ "e for turning right\n"
                   ++ "r for turning around\n"
                   ++ "q for turning left\n\n"
                   ++ "space for attacking\n\n"
                   ++ "i for picking up items\n"
                   ++ "k for dropping your weapon\n"
                   ++ "j for dropping your shield\n"
                   ++ "l for swapping your weapon\n"
                   ++ "ö for swapping your shield\n"
                   ++ "h for help\n"
                   ++ "escape for quitting\n\n"
                   ++ getMapKey
      putStrLn $ help ++ "\nPress Enter to continue."
      _ <- getLine
      return $ Right oldGame
   --
   -- Quitting:
   | str == "quit" = return $ Left "You successfully quit the game. Bye!"
   --
   -- Unknown commands:
   | otherwise = do
      let failStr = "This command was not recognized!"
      _ <- playerAction "help" oldGame
      return $ Right (failStr, roomMap, winPos, (player, oldPlayerPos), enemyMap, (ladder, oldLadderPos), itemMap)


-- and now the possible actions for enemies
enemyAction :: String -> Enemy -> Game -> IO (Either String (Game, Enemy))
enemyAction str oldEnemyWithPos@(enemy, oldEnemyPos@(oldEnemyRoom, _, _)) oldGame@(narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), itemMap)
   | str == "go forward" = do -- much the same as above
      let result = Mov.move oldEnemyPos Mov.Advance
      case result of
         Left resStr
            | resStr == "Wall" -> enemyAction "turn randomly" oldEnemyWithPos oldGame
            | resStr == "Door blocked" -> enemyAction "turn randomly" oldEnemyWithPos oldGame
         Right newEnemyPos@(newEnemyRoom, _, _) -> do
            let enemyList = HM.lookupDefault [] newEnemyRoom enemyMap
            case () of
             ()
               | roomMap ! newEnemyRoom && not (roomMap ! oldEnemyRoom) -> enemyAction "turn randomly" oldEnemyWithPos oldGame
               -- makes sure there isn't anything on the new position
               | not (any (newEnemyPos `comparePos`) (ladderPos : playerPos : map snd enemyList)) -> return $
                  Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), itemMap), (enemy, newEnemyPos))
{-               -- checks if the obstacle is the player
               | comparePos newEnemyPos playerPos -> enemyAction "attack" oldEnemyWithPos oldGame -}
               | otherwise -> enemyAction "turn randomly" oldEnemyWithPos oldGame
   | str == "teleport forward" = do
      tempGameRes <- enemyAction "go forward" oldEnemyWithPos oldGame
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right (tempGame@(_, _, _, (newPlayer, _), _, _, _), (newEnemy, newEnemyPos)) -> if comparePos oldEnemyPos newEnemyPos
            then return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), (newEnemy, newEnemyPos))
            else enemyAction "teleport forward" (newEnemy, newEnemyPos) tempGame
   | str == "go back" = do
      (Right (tempGame1, (_, tempEnemyPos1))) <- enemyAction "turn around" oldEnemyWithPos oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right (tempGame2, tempEnemyWithPos) -> enemyAction "turn around" tempEnemyWithPos tempGame2
   | str == "go left" = do
      (Right (tempGame1, (_, tempEnemyPos1))) <- enemyAction "turn left" oldEnemyWithPos oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right (tempGame2, tempEnemyWithPos) -> enemyAction "turn right" tempEnemyWithPos tempGame2
   | str == "go right" = do
      (Right (tempGame1, (_, tempEnemyPos1))) <- enemyAction "turn right" oldEnemyWithPos oldGame
      tempGameRes <- enemyAction "go forward" (enemy, tempEnemyPos1) tempGame1
      case tempGameRes of
         Left resStr -> return $ Left resStr
         Right (tempGame2, tempEnemyWithPos) -> enemyAction "turn left" tempEnemyWithPos tempGame2
   | str == "turn left" = do
      let (Right newEnemyPos) = Mov.move oldEnemyPos Mov.TurnLeft
      return $ Right (oldGame, (enemy, newEnemyPos))
   | str == "turn right" = do
      let (Right newEnemyPos) = Mov.move oldEnemyPos Mov.TurnRight
      return $ Right (oldGame, (enemy, newEnemyPos))
   | str == "turn around" = do
      (Right (tempGame, (_, tempEnemyPos))) <- enemyAction "turn left" oldEnemyWithPos oldGame
      enemyAction "turn left" (enemy, tempEnemyPos) tempGame
   | str == "turn randomly" = do
      gen <- newStdGen
      let option = head . randomRs (1,3) $ gen :: Int
      case option of
         1 -> enemyAction "turn left" oldEnemyWithPos oldGame
         2 -> enemyAction "turn right" oldEnemyWithPos oldGame
         3 -> enemyAction "turn around" oldEnemyWithPos oldGame
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
                     else return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
               Just oldShield -> do
                  let newShield = Item.reduceDur Item.fistDmg oldShield
                  if Item.isDestroyed newShield
                     then do
                        let newPlayer = Cha.dropItem oldShield player
                        return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
                     else do
                        let newPlayer = Cha.modifyInv True oldShield newShield player
                        return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
            Just (_, _, Item.Weapon dmg _) -> case playerShield of
               Nothing -> do
                  let newPlayer = Cha.reduceHealth dmg player
                  if Cha.isDead newPlayer
                     then return . Left $ "You were dealt one blow too many by that vicious " ++ Cha.name enemy ++ ". You have perished.\n"
                     else return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
               Just oldShield -> do
                  let newShield = Item.reduceDur dmg oldShield
                  if Item.isDestroyed newShield
                     then do
                        let newPlayer = Cha.dropItem oldShield player
                        return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
                     else do
                        let newPlayer = Cha.modifyInv True oldShield newShield player
                        return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
      else if playerPos `Mov.inLOS` oldEnemyPos -- the player isn't directly in front of the enemy, but still in its line of sight
         then case Cha.name enemy of
            "Berserker" -> enemyAction "go forward" oldEnemyWithPos oldGame --berserkers gonna berserk
            "Wraith" -> enemyAction "teleport forward" oldEnemyWithPos oldGame
            _ -> case enemyWeapon of
               Nothing -> enemyAction "random action" oldEnemyWithPos oldGame -- no weapon means no way they can reach the player
               Just (_, _, Item.Weapon dmg range) -> if (playerPos `Mov.distanceTo` oldEnemyPos) <= range -- check if the range is long enough
                     then case playerShield of
                        Nothing -> do
                           let newPlayer = Cha.reduceHealth dmg player
                           if Cha.isDead newPlayer
                              then return . Left $ "You were dealt one blow too many by that vicious " ++ Cha.name enemy ++ ". You have perished.\n"
                              else return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
                        Just oldShield -> do
                           let newShield = Item.reduceDur dmg oldShield
                           if Item.isDestroyed newShield
                              then do
                                 let newPlayer = Cha.dropItem oldShield player
                                 return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
                              else do
                                 let newPlayer = Cha.modifyInv True oldShield newShield player
                                 return $ Right ((narratorstr, roomMap, winPos, (newPlayer, playerPos), enemyMap, (ladder, ladderPos), itemMap), oldEnemyWithPos)
                     else enemyAction "random action" oldEnemyWithPos oldGame -- if it isn't, do something else
      else enemyAction "random action" oldEnemyWithPos oldGame -- if the enemy doesn't see a player, there's no point in attacking
   --
   -- Picking up items
   | str == "pickup item" = do
      -- get a list of the items at the current location that aren't keys
      let itemList = HM.lookupDefault [] oldEnemyRoom itemMap
      let placeItemList = filter (\(x, y) -> not (Item.isKey x) && y `comparePos` oldEnemyPos) itemList
      if null placeItemList
         then enemyAction "random action" oldEnemyWithPos oldGame
         else do
            let (newItem, itemPos) = head placeItemList
            let enemyWeapon = Cha.equippedWeapon enemy
            let enemyShield = Cha.equippedShield enemy
            if Item.isWeapon newItem
               then case enemyWeapon of
                  Nothing -> do
                     let newEnemy = Cha.pickupItem True newItem enemy
                     let newItemMap = deleteItem (newItem, itemPos) itemMap
                     return $ Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), newItemMap), (newEnemy, oldEnemyPos))
                  Just weapon -> do
                     let newEnemy = Cha.pickupItem True newItem . Cha.setEquipped False weapon $ enemy
                     let newItemMap = deleteItem (newItem, itemPos) itemMap
                     return $ Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), newItemMap), (newEnemy, oldEnemyPos))
               else case enemyShield of
                  Nothing -> do
                     let newEnemy = Cha.pickupItem True newItem enemy
                     let newItemMap = deleteItem (newItem, itemPos) itemMap
                     return $ Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), newItemMap), (newEnemy, oldEnemyPos))
                  Just shield -> do
                     let newEnemy = Cha.pickupItem True newItem . Cha.setEquipped False shield $ enemy
                     let newItemMap = deleteItem (newItem, itemPos) itemMap
                     return $ Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), newItemMap), (newEnemy, oldEnemyPos))
   --
   -- Dropping the currently equipped weapon
   | str == "drop weapon" = do
      let enemyWeapon = Cha.equippedWeapon enemy
      case enemyWeapon of
         Nothing -> enemyAction "random action" oldEnemyWithPos oldGame
         Just weapon -> do
            let newEnemy = Cha.dropItem weapon enemy
            let newItemMap = insertItem (weapon, oldEnemyPos) itemMap
            return $ Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), newItemMap), (newEnemy, oldEnemyPos))
   --
   -- Dropping the currently equipped shield
   | str == "drop shield" = do
      let enemyShield = Cha.equippedShield enemy
      case enemyShield of
         Nothing -> enemyAction "random action" oldEnemyWithPos oldGame
         Just shield -> do
            let newEnemy = Cha.dropItem shield enemy
            let newItemMap = insertItem (shield, oldEnemyPos) itemMap
            return $ Right ((narratorstr, roomMap, winPos, (player, playerPos), enemyMap, (ladder, ladderPos), newItemMap), (newEnemy, oldEnemyPos))
   | str == "swap weapon" = if Cha.numWeapons enemy < 2
      then enemyAction "random action" oldEnemyWithPos oldGame
      else do
         let unusedWeaponList = map fst . filter (not . snd) $ Cha.weaponList enemy
         gen <- newStdGen
--         let newWeapon = Trace.trace ". \n" . Trace.traceShowId . (unusedWeaponList!!) . Trace.trace "with resulting weapon " . Trace.traceShowId . Trace.trace "Using function enemyAction, part \"swap weapon\", function unusedWeaponList with index " . head . randomRs (0, length unusedWeaponList - 1) $ gen
         let newWeapon = (unusedWeaponList!!) . head $ randomRs (0, length unusedWeaponList - 1) gen
         let oldWeapon = Cha.equippedWeapon enemy
         case oldWeapon of
            Nothing -> do
               let newEnemy = Cha.setEquipped True newWeapon enemy
               return $ Right (oldGame, (newEnemy, oldEnemyPos))
            Just weapon -> do
               let newEnemy = Cha.setEquipped True newWeapon . Cha.setEquipped False weapon $ enemy
               return $ Right (oldGame, (newEnemy, oldEnemyPos))
   | str == "swap shield" = if Cha.numShields enemy < 2
      then enemyAction "random action" oldEnemyWithPos oldGame
      else do
         let unusedShieldList = map fst . filter (not . snd) $ Cha.shieldList enemy
         gen <- newStdGen
--         let newShield = Trace.trace ". \n" . Trace.traceShowId . (unusedShieldList!!) . Trace.trace "with resulting shield " . Trace.traceShowId . Trace.trace "Using function enemyAction, part \"swap shield\", function unusedShieldList with index " . head . randomRs (0, length unusedShieldList - 1) $ gen
         let newShield = (unusedShieldList!!) . head $ randomRs (0, length unusedShieldList - 1) gen
         let oldShield = Cha.equippedShield enemy
         case oldShield of
            Nothing -> do
               let newEnemy = Cha.setEquipped True newShield enemy
               return $ Right (oldGame, (newEnemy, oldEnemyPos))
            Just shield -> do
               let newEnemy = Cha.setEquipped True newShield . Cha.setEquipped False shield $ enemy
               return $ Right (oldGame, (newEnemy, oldEnemyPos))
   --
   -- choose a random action
   | str == "random action" = do
      gen <- newStdGen
      let randOption = head $ randomRs (1 :: Int, 13) gen
      case randOption of
         1 -> enemyAction "go forward" oldEnemyWithPos oldGame
         2 -> enemyAction "go back" oldEnemyWithPos oldGame
         3 -> enemyAction "go left" oldEnemyWithPos oldGame
         4 -> enemyAction "go right" oldEnemyWithPos oldGame
         5 -> enemyAction "turn right" oldEnemyWithPos oldGame
         6 -> enemyAction "turn around" oldEnemyWithPos oldGame
         7 -> enemyAction "turn left" oldEnemyWithPos oldGame
         8 -> enemyAction "attack" oldEnemyWithPos oldGame
         9 -> enemyAction "pickup item" oldEnemyWithPos oldGame
         10 -> enemyAction "drop weapon" oldEnemyWithPos oldGame
         11 -> enemyAction "drop shield" oldEnemyWithPos oldGame
         12 -> enemyAction "swap weapon" oldEnemyWithPos oldGame
         13 -> enemyAction "swap shield" oldEnemyWithPos oldGame




main :: IO ()
main = do 
   gameVar <- initialize
   hSetEcho stdin False
   hSetBuffering stdin NoBuffering
   gameState gameVar
