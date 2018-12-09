import Data.Array
import System.IO
import System.Random
import qualified Data.Map as Map
import qualified Movement as Mov
import qualified Character as Cha



createMap :: IO Mov.Map -- creates a new map with randomly locked rooms
createMap = do gen <- newStdGen
               let randomBools = take Mov.numRooms (randoms gen :: [Bool])
               return (listArray ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) randomBools)


createCharacter :: Map.Map -> IO (Integer, Map.Map) -- creates an empty character with a unique id - that's why we have to pass that map around!
createCharacter map = do gen <- newStdGen
                          let randomIDs = randoms gen :: [Integer]
                          let id = head $ filter (\x -> not (Map.member x map)) randomIDs
                          let player = Cha.Character "" 100 False []
                          let map = Map.insert id player map
                          return (id, map)


createPlayer :: Map.Map -> IO (Integer, Map.Map) -- creates a new player character
createPlayer map = do (id, newMap) <- createCharacter map
                       print "How would you like to be called?"
                       name <- getLine
                       print ("Very well, " ++ name ++ " it is then.")
                       let (Just char) = Map.lookup id newMap
                       let player = Cha.setPlayerCharacter True . Cha.changeName name $ char
                       let map = Map.insert id player newMap
                       return (id, map)






type Game = ([, Mov.Map, [Cha.Item], Map.Map)


-- play :: Game -> ???


main = do map <- createMap
          (playerID, charMap) <- createPlayer Map.empty
          (charID, charMap) <- createCharacter charMap
--          result <- play charIDList [player] map
          print map
          print playerID
          print charID
          print charMap
--          putStrLn result
