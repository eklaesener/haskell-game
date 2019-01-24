module Config where

import Prelude hiding (map)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except

import System.Directory (doesFileExist)
import Data.ConfigFile


data Config = Config { map :: !((Int, Int), (Int, Int))
                     , room :: !((Int, Int), (Int, Int))
                     , startingHP :: !Float
                     , enemyRange :: !(Int, Int)
                     , itemRange :: !(Int, Int)
                     , lockedRooms :: !Int
                     , timeEnemies :: !Int
                     , timeScreen :: !Int
                     } deriving (Show, Read, Eq)



-- reads the config values from settings.cfg
initConfig :: IO Config
initConfig = do
   -- check if the file actually exists
   fileExists <- doesFileExist "settings.cfg"
   rv <- runExceptT $ if fileExists
      then do
         -- open and parse the file
         cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
         -- separate the parsed options
         map1 <- get cp "DEFAULT" "mapfirstrow"
         map2 <- get cp "DEFAULT" "mapfirstcolumn"
         map3 <- get cp "DEFAULT" "maplastrow" 
         map4 <- get cp "DEFAULT" "maplastcolumn"
         let vMap = ((map1, map2), (map3, map4))
         
         room1 <- get cp "DEFAULT" "roomfirstrow"
         room2 <- get cp "DEFAULT" "roomfirstcolumn"
         room3 <- get cp "DEFAULT" "roomlastrow" 
         room4 <- get cp "DEFAULT" "roomlastcolumn"
         let vRoom = ((room1, room2), (room3, room4))
         
         hp <- get cp "DEFAULT" "startinghealthpoints"
         
         minEnemy <- get cp "DEFAULT" "minenemies"
         maxEnemy <- get cp "DEFAULT" "maxenemies"
         let vEnemy = (minEnemy, maxEnemy)
         
         minItem <- get cp "DEFAULT" "minitems"
         maxItem <- get cp "DEFAULT" "maxitems"
         let vItem = (minItem, maxItem)
         
         locked <- get cp "DEFAULT" "lockedrooms"
         
         timeEnemy <- get cp "DEFAULT" "timeenemies"
         timeScr <- get cp "DEFAULT" "timescreen"
         
         return Config { map = vMap
                       , room = vRoom
                       , startingHP = hp
                       , enemyRange = vEnemy
                       , itemRange = vItem
                       , lockedRooms = locked
                       , timeEnemies = timeEnemy
                       , timeScreen = timeScr
                       }
      else throwError (OtherProblem "FileNotAvailableError", "The config file is not available")
   case rv of
      Left (ParseError str1, str2) -> error $ "Something went wrong while parsing the file at " ++ str1 ++ "! Description: " ++ str2
      Left (NoOption opt, _) -> error $ "Very funny, you deleted or renamed the key \"" ++ opt ++ "\" or commented it out. Hooray!"
      Left (OtherProblem "FileNotAvailableError", _) -> error "You deleted the config file. Wow."
      Left _ -> error "Something went very, very wrong here."
      Right cfg -> return cfg
