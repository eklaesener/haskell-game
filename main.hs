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


createCharacter :: Map.Map Integer Cha.Character -> IO (Integer, Map.Map Integer Cha.Character) -- creates an empty character with a unique id - that's why we have to pass that map around!
createCharacter map = do gen <- newStdGen
                         let randomIDs = randoms gen :: [Integer]
                         let id = head $ filter (\x -> not (Map.member x map)) randomIDs
                         let player = Cha.Character "" 100 False []
                         let newMap = Map.insert id player map
                         return (id, newMap)


createPlayer :: Map.Map Integer Cha.Character -> IO (Integer, Map.Map Integer Cha.Character) -- creates a new player character
createPlayer map = do (id, tempMap) <- createCharacter map
                      print "How would you like to be called?"
                      name <- getLine
                      print ("Very well, " ++ name ++ " it is then.")
                      let (Just char) = Map.lookup id tempMap
                      let player = Cha.setPlayerCharacter True . Cha.changeName name $ char
                      let newMap = Map.insert id player tempMap
                      return (id, newMap)




type Characters = [Integer]
type CharMap = Map.Map Integer Cha.Character

type Items = [Integer]
type ItemMap = Map.Map Integer Cha.Item

type Game = (Mov.Map, Characters, CharMap, Items, ItemMap)


-- play :: Game -> ???


main = do map <- createMap
          (playerID, charMap1) <- createPlayer Map.empty
          (charID, charMap2) <- createCharacter charMap1
--          result <- play charIDList [player] map
          print map
          print playerID
          print charID
          print charMap2
--          putStrLn result
