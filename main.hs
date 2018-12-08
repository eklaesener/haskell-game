import Data.Array
import System.IO
import System.Random
import qualified Movement as Mov
import Character



createMap :: IO Mov.Map
createMap = do gen <- newStdGen
               let randomBools = take Mov.numRooms (randoms gen :: [Bool])
               return (listArray ((Mov.lowBoundNS, Mov.lowBoundWE), (Mov.highBoundNS, Mov.highBoundWE)) randomBools)









main = do map <- createMap
--          player <- createPlayer
--          result <- play player map
          print map
--          putStrLn result
