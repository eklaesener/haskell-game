import Data.Array
import System.IO
import System.Random
import qualified Movement as Mov
import qualified Character as Cha



createMap :: IO Mov.Map -- creates a new map with randomly locked rooms
createMap = do gen <- newStdGen
               let randomBools = take Mov.numRooms (randoms gen :: [Bool])
               return (listArray ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) randomBools)


createCharacter :: [Integer] -> IO (Cha.Character, [Integer]) -- creates an empty character with a unique id - that's why we have to pass that list around!
createCharacter list = do gen <- newStdGen
                          let randomIDs = randoms gen :: [Integer]
                          let id = head $ filter (\x -> not (x `elem` list)) randomIDs
                          return ((Cha.Character id "" 100 False []), (id:list))


createPlayer :: [Integer] -> IO (Cha.Character, [Integer]) -- creates a new player character
createPlayer list = do (char, newList) <- createCharacter list
                       print "How would you like to be called?"
                       name <- getLine
                       print ("Very well, " ++ name ++ " it is then.")
                       let player = Cha.setPlayerCharacter True . Cha.changeName name $ char
                       return (player, newList)


-- play :: [Integer] -> [Cha.Character] -> Mov.Map -> ???


main = do map <- createMap
          (player, charIDList) <- createPlayer []
          (char, charIDList) <- createCharacter charIDList
--          result <- play charIDList [player] map
          print map
          print player
          print char
          print charIDList
--          putStrLn result
