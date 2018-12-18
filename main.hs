import Data.Array
import System.IO
import System.Exit
import System.Random
import Control.Monad.Random
import qualified Data.Map.Strict as Map
import qualified Movement as Mov
import qualified Character as Cha
import qualified Draw

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
   random = randomR (Mov.North, Mov.East)



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
   putStrLn "What is your name?"
   name <- getLine
   let (Just (char, pos)) = Map.lookup id tempMap
   let player = Cha.setPlayerCharacter True . Cha.changeName name $ char
   let newMap = Map.insert id (player,pos) tempMap
   return (id, newMap)


createItem :: Bool ->  Mov.Map -> ItemMap -> IO (Int, ItemMap)
createItem checkForLocked roomMap itemMap = do
   gen <- newStdGen
   let id = head $ filter (\x -> not (Map.member x itemMap)) (randoms gen :: [Int])
   gen2 <- newStdGen
   let itemID = head (randomRs (0,Cha.itemCount-2) gen :: [Int])
   let item = Cha.itemList !! itemID
   pos <- randomPosition checkForLocked roomMap
   let newMap = Map.insert id (item, pos) itemMap
   return (id, newMap)

createLadder :: Bool -> Mov.Map -> ItemMap -> IO (Int, ItemMap)
createLadder checkForLocked roomMap itemMap = do
   gen <- newStdGen
   let id = head $ filter (\x -> not (Map.member x itemMap)) (randoms gen :: [Int])
   gen2 <- newStdGen
   let item = last Cha.itemList
   pos@(_, inner, _) <- randomPosition checkForLocked roomMap
   if Mov.isWall inner
      then createLadder checkForLocked roomMap itemMap
      else do
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
      let dir = head $ randomRs (Mov.North, Mov.East) gen3 -- get a random Mov.Direction
      let newPos = (loc, innerLoc, dir)
      return newPos
   | otherwise = do
      gen <- newStdGen
      let loc = head $ randomRs ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) gen :: (Int, Int) -- get a random room
      gen2 <- newStdGen
      let innerLoc = head $ randomRs ((0,0), (Mov.roomSize,Mov.roomSize)) gen2 :: (Int, Int)
      gen3 <- newStdGen
      let dir = head $ randomRs (Mov.North, Mov.East) gen3
      let newPos = (loc, innerLoc, dir)
      return newPos


-- to sort the list that gets sent to the Draw.draw function
mergeSort :: [(Mov.InnerLocation, String)] -> [(Mov.InnerLocation, String)]
mergeSort [] = []
mergeSort [a] = [a]
mergeSort = mergeRuns . ascRun


ascRun run b [] = [reverse (b:run)]
ascRun run b (x:xs)
   | b<=x = ascRun (b:run) x xs
   | otherwise = (reverse (b:run)) : runs (x:xs)





type Player = Int
type CharMap = Map.Map Int (Cha.Character, Mov.Position)

type Ladder = Int
type ItemMap = Map.Map Int (Cha.Item, Mov.Position)

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
   newStdGen
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
   newStdGen
   gen <- getStdGen
   return . (wallPushMsgs!!) . head $ randomRs (0, length wallPushMsgs - 1) gen

wallPushMsgs :: [String]
wallPushMsgs =
   ["Try as you might, you still can't push the ladder through the wall."
   ,"Maybe try to push the ladder through a door, not through a wall?"
   ,"This is a wall. It has the property of not allowing big enough things to go\nthrough it. You should probably remember that."
   ]

hasWon :: Mov.Position -> Mov.Position -> Bool
hasWon (room1, inner1, _) (room2, inner2, _) = room1 == room2 && inner1 == inner2

emptyGame :: Mov.Map -> Game
emptyGame map = (False, map, ((0,0),(0,0),Mov.North), 0, Map.empty, 0, Map.empty)


initialize :: IO Game
initialize = do
   putStrLn "Do you want to use long or short controls? Enter long or short, respectively:"
   controlStr <- getLine
   case controlStr of
      str
         | str == "long" -> do
            roomMap <- createMap
            let empty = emptyGame roomMap
            action "help" empty
            (playerID, charMap1) <- createPlayer roomMap Map.empty
            let (Just (player, winPosition)) = Map.lookup playerID charMap1
            gen <- newStdGen
            let numItems = head $ randomRs (0,Mov.numRooms) gen
            (itemID1, itemMap1) <- createItem False roomMap Map.empty
            (itemID2, itemMap2) <- createItem False roomMap itemMap1
            (ladderID, itemMap3) <- createLadder True roomMap itemMap2
            let (Just (_, ladderPos)) = Map.lookup ladderID itemMap3
            let itemMap4 = Map.insert ladderID (last Cha.itemList, ladderPos) itemMap3
            putStrLn $ "\nWell, " ++ Cha.name player ++ ", you're in quite a pickle right now."
            putStrLn "Remember? You were exploring a\ncave, but the floor fell away under you... \nMaybe there's a ladder here somewhere?"
            return (False, roomMap, winPosition, playerID, charMap1, ladderID, itemMap4)
         | str == "short" -> do
            roomMap <- createMap
            let empty = emptyGame roomMap
            action "help" empty
            (playerID, charMap1) <- createPlayer roomMap Map.empty
            let (Just (player, winPosition)) = Map.lookup playerID charMap1
            gen <- newStdGen
            let numItems = head $ randomRs (0,Mov.numRooms) gen
            (itemID1, itemMap1) <- createItem False roomMap Map.empty
            (itemID2, itemMap2) <- createItem False roomMap itemMap1
            (ladderID, itemMap3) <- createLadder True roomMap itemMap2
            let (Just (_, ladderPos)) = Map.lookup ladderID itemMap3
            let itemMap4 = Map.insert ladderID (last Cha.itemList, ladderPos) itemMap3
            putStrLn $ "\nWell, " ++ Cha.name player ++ ", you're in quite a pickle right now. Remember? You were exploring a cave, but the floor you were standing on fell down and you with it... Maybe there's a ladder here somewhere?"
            return (True, roomMap, winPosition, playerID, charMap1, ladderID, itemMap4)
         | otherwise -> do
            putStrLn "Unrecognized input! Please try again..."
            initialize


drawMap :: Game -> IO ()
drawMap game@(_, _, (winRoom, (winX, winY), _), playerID, charMap, ladderID, itemMap) = do
   let (Just (_, (playerRoom, (playerX,playerY), playerDir))) = Map.lookup playerID charMap
   let (Just (_, (ladderRoom, (ladderX, ladderY), ladderDir))) = Map.lookup ladderID itemMap
   if playerRoom == ladderRoom
      then if playerRoom == winRoom
         then do
            let list = [((playerX, playerY), Draw.player), ((ladderX, ladderY), Draw.ladder), ((winX, winY), Draw.win), ] ++ [((x,y), Draw.dot) | x <- [0 .. Mov.roomSize], y <- [0 .. Mov.roomSize], (x,y) /= (playerX, playerY), (x,y) /= (ladderX, ladderY), (x,y) /= (winX, winY)]
            let sortedList = mergeSort list
            Draw.draw sortedList
         else do
            let list = [((playerX, playerY), Draw.player playerDir), ((ladderX, ladderY), Draw.ladder)] ++ [((x,y), Draw.dot) | x <- [0 .. Mov.roomSize], y <- [0 .. Mov.roomSize], (x,y) /= (playerX, playerY), (x,y) /= (ladderX, ladderY)]
            let sortedList = mergeSort list
            Draw.draw sortedList
      else if playerRoom == winRoom
         then do
            let list = [((playerX, playerY), Draw.player playerDir), ((winX, winY), Draw.win)] ++ [((x,y), Draw.dot) | x <- [0 .. Mov.roomSize], y <- [0 .. Mov.roomSize], (x,y) /= (playerX, playerY), (x,y) /= (winX, winY)]
            let sortedList = mergeSort list
            Draw.draw sortedList
         else do
            let list = [((playerX, playerY), Draw.player playerDir)] ++ [((x,y), Draw.dot) | x <- [0 .. Mov.roomSize], y <- [0 .. Mov.roomSize], (x,y) /= (playerX, playerY)]
            let sortedList = mergeSort list
            Draw.draw sortedList






-- Here, we finally get to play the game!
gameState :: Game -> IO String
gameState game@(control, _, winPos, playerID, charMap, ladderID, itemMap) = do
   putStrLn $ "The win position is " ++ show winPos ++ "\n"
   let (Just (player, pos)) = Map.lookup playerID charMap
   putStrLn $ "Your stats are " ++ show player ++ "\nand your position is " ++ show pos ++ "\n"
   let (Just (_, ladderPos)) = Map.lookup ladderID itemMap
   putStrLn $ "The ladder position is " ++ show ladderPos ++ "\n"
   if control
      then do
         resultUnformatted <- shortInput game
         case resultUnformatted of
            (Left str) -> return str
            (Right game2) -> gameState game2
      else do
         resultUnformatted <- longInput game
         case resultUnformatted of
            (Left str) -> return str
            (Right game2) -> gameState game2


longInput :: Game -> IO (Either String Game)
longInput game = do
   putStrLn "What do you want to do next?\n"
   input <- getLine
   putStrLn "\n"
   action input game


getKey :: IO Char
getKey = do
   hSetEcho stdin False
   hSetBuffering stdin NoBuffering
   x <- getChar
   hSetEcho stdin True
   hSetBuffering stdin LineBuffering
   return x


shortInput :: Game -> IO (Either String Game)
shortInput game = do
   input <- getKey
   putStrLn "\n"
   case input of
      'w' -> action "go forward" game
      'd' -> action "go right" game
      's' -> action "go back" game
      'a' -> action "go left" game
      'f' -> action "turn right" game
      'r' -> action "turn around" game
      'e' -> action "turn left" game
      'h' -> action "help" game
      'q' -> action "quit" game
      key -> action "unknown" game -- there isn't actually a match for unknown, so it will just fall through to the catch-all




action :: String -> Game -> IO (Either String Game)
action str oldGame@(control, roomMap, winPos, playerID, charMap, ladderID, itemMap)
   | str == "go forward" = do
      let (Just (player, oldPos@(oldRoom, oldInner, oldDir))) = Map.lookup playerID charMap
      let (Just (ladder, ladderPos@(ladderRoom, ladderInner, ladderDir))) = Map.lookup ladderID itemMap
      let result = Mov.move oldPos Mov.Advance
      case result of
         Left str -- the move function has caught something to be handled here
            | str == "Wall" -> do
               msg <- getWallMsg
               putStrLn msg
               return $ Right oldGame
            | str == "Door blocked" -> do
               msg <- getDoorBlockedMsg
               putStrLn msg
               return $ Right oldGame
         Right newPos@(newRoom, newInner, newDir)
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
                  then return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't do an exploration in caves like this one anymore...\n\n"
                  else if Mov.isCorner newLadderInner
                     then return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all..."
                     else do
                        -- update ladder and player positions and return them
                        let newItemMap = Map.insert ladderID (ladder, (newLadderRoom, newLadderInner, ladderDir)) itemMap
                        let newCharMap = Map.insert playerID (player, newPos) charMap
                        return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, newItemMap)
            | otherwise -> do -- we're okay to move, but need to check the same things as before
               let result2 = Mov.move newPos Mov.Advance
               case result2 of
                  Left str
                     | str == "Wall" -> do
                        msg <- getWallPushMsg
                        putStrLn msg
                        return $ Right oldGame
                     | str == "Door blocked" -> do
                        msg <- getDoorBlockedMsg
                        putStrLn msg
                        return $ Right oldGame
                  Right newLadderPos@(newLadderRoom, newLadderInner, _)
                     | roomMap ! newLadderRoom -> do
                        msg <- getRoomLockedMsg
                        putStrLn msg
                        return $ Right oldGame
                     | hasWon newLadderPos winPos -> return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't do an exploration in caves like this one anymore...\n\n"
                     | Mov.isCorner newLadderInner -> return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all..."
                     | otherwise -> do
                        -- update ladder and player positions and return them
                        let newItemMap = Map.insert ladderID (ladder, (newLadderRoom, newLadderInner, ladderDir)) itemMap
                        let newCharMap = Map.insert playerID (player, newPos) charMap
                        return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, newItemMap)
--
-- same procedure as above, just in the other direction
   | str == "go back" = do
      let (Just (player, oldPos@(oldRoom, oldInner, oldDir))) = Map.lookup playerID charMap
      let (Just (ladder, ladderPos@(ladderRoom, ladderInner, ladderDir))) = Map.lookup ladderID itemMap
      let result = Mov.move oldPos Mov.BackOff
      case result of
         Left str -- the move function has caught something to be handled here
            | str == "Wall" -> do
               msg <- getWallMsg
               putStrLn msg
               return $ Right oldGame
            | str == "Door blocked" -> do
               msg <- getDoorBlockedMsg
               putStrLn msg
               return $ Right oldGame
         Right newPos@(newRoom, newInner, newDir)
            | roomMap ! newRoom -> do -- if the new room is locked, don't allow the move
               msg <- getRoomLockedMsg
               putStrLn msg
               return $ Right oldGame
            | newRoom /= ladderRoom || newInner /= ladderInner -> do -- we won't collide with the ladder, so everything is alright
               let newCharMap = Map.insert playerID (player, newPos) charMap
               return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, itemMap)
            | Mov.isWall oldInner || not (Mov.isWall newInner) -> do
               -- we know that the push can't go wrong, so we don't need case
               let (Right newLadderPos@(newLadderRoom, newLadderInner, _)) = Mov.move newPos Mov.BackOff
               if hasWon newLadderPos winPos
                  then return $ Left "You've finally gotten out of the caverns! Though you probably shouldn't do an exploration in caves like this one anymore...\n\n"
                  else if Mov.isCorner newLadderInner
                     then return $ Left "Idiot! You've maneuvered the ladder into an unrecoverable location. Guess you're not going to escape this cavern after all..."
                     else do
                        -- update ladder and player positions and return them
                        let newItemMap = Map.insert ladderID (ladder, (newLadderRoom, newLadderInner, ladderDir)) itemMap
                        let newCharMap = Map.insert playerID (player, newPos) charMap
                        return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, newItemMap)
            | otherwise -> do -- we're okay to move, but need to check the same things as before
               let result2 = Mov.move newPos Mov.Advance
               case result2 of
                  Left str
                     | str == "Wall" -> do
                        msg <- getWallPushMsg
                        putStrLn msg
                        return $ Right oldGame
                     | str == "Door blocked" -> do
                        msg <- getDoorBlockedMsg
                        putStrLn msg
                        return $ Right oldGame
                  Right newLadderPos@(newLadderRoom, newLadderInner, _)
                     | roomMap ! newLadderRoom -> do
                        msg <- getRoomLockedMsg
                        putStrLn msg
                        return $ Right oldGame
                     | otherwise -> do
                        -- update ladder and player positions and return them
                        let newItemMap = Map.insert ladderID (ladder, (newLadderRoom, newLadderInner, ladderDir)) itemMap
                        let newCharMap = Map.insert playerID (player, newPos) charMap
                        return $ Right (control, roomMap, winPos, playerID, newCharMap, ladderID, newItemMap)
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
         Left str -> return $ Left str
         (Right tempGame2) -> action "turn right" tempGame2
   | str == "go right" = do
      (Right tempGame1) <- action "turn right" oldGame
      tempResult2 <- action "go forward" tempGame1
      case tempResult2 of
         Left str -> return $ Left str
         (Right tempGame2) -> action "turn left" tempGame2
--
-- Getting a list of commands:
   | str == "help" = do
      putStrLn "Possible commands:\n"
      putStrLn "go forward // w"
      putStrLn "go right // d"
      putStrLn "go back // s"
      putStrLn "go left // a\n"
      putStrLn "turn right // f"
      putStrLn "turn around // r"
      putStrLn "turn left // e\n"
      putStrLn "help // h"
      putStrLn "quit // q\n"
      return $ Right oldGame
--
-- Quitting:
   | str == "quit" = exitSuccess
--
-- Unknown commands:
   | otherwise = do
      putStrLn "This command was not recognized!"
      action "help" oldGame






main = do
   game@(_, roomMap, _, _, _, _, _) <- initialize
   putStrLn $ "Which rooms are locked (True) and which aren't (False)\n" ++ show roomMap ++ "\n"
   state <- gameState game
   putStrLn state
