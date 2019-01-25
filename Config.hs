module Config where

import Prelude hiding (map)
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except

import System.Directory (doesFileExist)
import Data.ConfigFile


data Config = Config { map :: !(Int, Int)
                     , room :: !(Int, Int)
                     , startingHP :: !Float
                     , enemyRange :: !(Int, Int)
                     , itemRange :: !(Int, Int)
                     , lockedRooms :: !Int
                     , timeEnemies :: !Int
                     , timeScreen :: !Int
                     } deriving (Show, Read, Eq)




fullDefaults :: IO Config
fullDefaults = return $ Config (4,4) (8,8) 100.0 (2,20) (5, 25) 2 500 100

initFullDefaults :: IO Config
initFullDefaults = do
   putStrLn "Warning!\nThe config file \"settings.cfg\" could not be found or read. Using default values.\nPress Enter to continue..."
   _ <- getLine
   fullDefaults


confMapRows :: ExceptT CPError IO Int
confMapRows = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "maprows"

confMapColumns :: ExceptT CPError IO Int
confMapColumns = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "mapcolumns"


getMap :: IO (Int, Int)
getMap = do
   rv1 <- runExceptT confMapRows
   rv2 <- runExceptT confMapColumns
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> return (4, 4)
         Right cols -> return (4, cols)
      Right rows -> case rv2 of
         Left _ -> return (rows, 4)
         Right cols -> return (rows, cols)


initGetMap :: IO (Int, Int)
initGetMap = do
   rv1 <- runExceptT confMapRows
   rv2 <- runExceptT confMapColumns
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe map size options are unavailable, defaulting to (4,4).\nPress Enter to continue..."
            _ <- getLine
            return (4, 4)
         Right cols -> do
            putStrLn "Warning!\nThe \"maprows\" option is unavailable, defaulting to 4.\nPress Enter to continue..."
            _ <- getLine
            return (4, cols)
      Right rows -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe \"mapcolumns\" option is unavailable, defaulting to 4.\nPress Enter to continue..."
            _ <- getLine
            return (rows, 4)
         Right cols -> return (rows, cols)



confRoomRows :: ExceptT CPError IO Int
confRoomRows = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "roomrows"

confRoomColumns :: ExceptT CPError IO Int
confRoomColumns = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "roomcolumns"


getRoom :: IO (Int, Int)
getRoom = do
   rv1 <- runExceptT confRoomRows
   rv2 <- runExceptT confRoomColumns
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> return (8, 8)
         Right cols -> return (8, cols)
      Right rows -> case rv2 of
         Left _ -> return (rows, 8)
         Right cols -> return (rows, cols)


initGetRoom :: IO (Int, Int)
initGetRoom = do
   rv1 <- runExceptT confRoomRows
   rv2 <- runExceptT confRoomColumns
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe room size options are unavailable, defaulting to (8,8).\nPress Enter to continue..."
            _ <- getLine
            return (8, 8)
         Right cols -> do
            putStrLn "Warning!\nThe \"roomrows\" option is unavailable, defaulting to 8.\nPress Enter to continue..."
            _ <- getLine
            return (8, cols)
      Right rows -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe \"roomcolumns\" option is unavailable, defaulting to 8.\nPress Enter to continue..."
            _ <- getLine
            return (rows, 8)
         Right cols -> return (rows, cols)



confStartHP :: ExceptT CPError IO Float
confStartHP = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "startinghealthpoints"

getStartHP :: IO Float
getStartHP = do
   rv <- runExceptT confStartHP
   case rv of
      Left _ -> return 100.0
      Right hp -> return hp

initGetStartHP :: IO Float
initGetStartHP = do
   rv <- runExceptT confStartHP
   case rv of
      Left _ -> do
         putStrLn "Warning!\nThe \"startinghealthpoints\" option is unavailable, defaulting to 100.0.\nPress Enter to continue..."
         _ <- getLine
         return 100.0
      Right hp -> return hp


confMinEnemy :: ExceptT CPError IO Int
confMinEnemy = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "minenemies"

confMaxEnemy :: ExceptT CPError IO Int
confMaxEnemy = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "maxenemies"


getEnemies :: IO (Int, Int)
getEnemies = do
   rv1 <- runExceptT confMinEnemy
   rv2 <- runExceptT confMaxEnemy
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> return (2, 20)
         Right n -> return (2, n)
      Right m -> case rv2 of
         Left _ -> return (m, 20)
         Right n -> return (m, n)

initGetEnemies :: IO (Int, Int)
initGetEnemies = do
   rv1 <- runExceptT confMinEnemy
   rv2 <- runExceptT confMaxEnemy
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe \"minenemies\" and \"maxenemies\" options are unavailable, defaulting to (2, 20).\nPress Enter to continue..."
            _ <- getLine
            return (2, 20)
         Right n -> do
            putStrLn "Warning!\nThe \"minenemies\" option is unavailable, defaulting to 2.\nPress Enter to continue..."
            _ <- getLine
            return (2, n)
      Right m -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe \"maxenemies\" option is unavailable, defaulting to 20.\nPress Enter to continue..."
            _ <- getLine
            return (m, 20)
         Right n -> return (m, n)


confMinItem :: ExceptT CPError IO Int
confMinItem = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "minitems"

confMaxItem :: ExceptT CPError IO Int
confMaxItem = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "maxitems"


getItems :: IO (Int, Int)
getItems = do
   rv1 <- runExceptT confMinItem
   rv2 <- runExceptT confMaxItem
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> return (5, 25)
         Right n -> return (5, n)
      Right m -> case rv2 of
         Left _ -> return (m, 25)
         Right n -> return (m, n)

initGetItems :: IO (Int, Int)
initGetItems = do
   rv1 <- runExceptT confMinItem
   rv2 <- runExceptT confMaxItem
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe \"minitems\" and \"maxitems\" options are unavailable, defaulting to (5, 25).\nPress Enter to continue..."
            _ <- getLine
            return (5, 25)
         Right n -> do
            putStrLn "Warning!\nThe \"minitems\" option is unavailable, defaulting to 5.\nPress Enter to continue..."
            _ <- getLine
            return (5, n)
      Right m -> case rv2 of
         Left _ -> do
            putStrLn "Warning!\nThe \"maxitems\" option is unavailable, defaulting to 25.\nPress Enter to continue..."
            _ <- getLine
            return (m, 25)
         Right n -> return (m, n)


confLocked :: ExceptT CPError IO Int
confLocked = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "lockedrooms"


getLockedRooms :: IO Int
getLockedRooms = do
   rv <- runExceptT confLocked
   case rv of
      Left _ -> return 2
      Right n -> return n

initGetLockedRooms :: IO Int
initGetLockedRooms = do
   rv <- runExceptT confLocked
   case rv of
      Left _ -> do
         putStrLn "Warning!\nThe \"lockedrooms\" option is unavailable, defaulting to 2.\nPress Enter to continue..."
         _ <- getLine
         return 2
      Right n -> return n


confTimeEnemy :: ExceptT CPError IO Int
confTimeEnemy = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "timeenemies"


getTimeEnemy :: IO Int
getTimeEnemy = do
   rv <- runExceptT confTimeEnemy
   case rv of
      Left _ -> return 500
      Right n -> return n

initGetTimeEnemy :: IO Int
initGetTimeEnemy = do
   rv <- runExceptT confTimeEnemy
   case rv of
      Left _ -> do
         putStrLn "Warning!\nThe \"timeenemies\" option is unavailable, defaulting to 500.\nPress Enter to continue..."
         _ <- getLine
         return 500
      Right n -> return n


confTimeScreen :: ExceptT CPError IO Int
confTimeScreen = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "timescreen"


getTimeScreen :: IO Int
getTimeScreen = do
   rv <- runExceptT confTimeScreen
   case rv of
      Left _ -> return 100
      Right n -> return n

initGetTimeScreen :: IO Int
initGetTimeScreen = do
   rv <- runExceptT confTimeScreen
   case rv of
      Left _ -> do
         putStrLn "Warning!\nThe \"timescreen\" option is unavailable, defaulting to 100.\nPress Enter to continue..."
         _ <- getLine
         return 100
      Right n -> return n

-- reads the config values from settings.cfg, loads defaults if the file or some options aren't readable
config :: IO Config
config = do
   -- check if the file actually exists
   fileExists <- doesFileExist "settings.cfg"
   if fileExists
      then do
         vMap <- getMap
         vRoom <- getRoom
         vStartHP <- getStartHP
         vEnemy <- getEnemies
         vItem <- getItems
         vLocked <- getLockedRooms
         vTimeEnemy <- getTimeEnemy
         vTimeScreen <- getTimeScreen
         return Config { map = vMap
                       , room = vRoom
                       , startingHP = vStartHP
                       , enemyRange = vEnemy
                       , itemRange = vItem
                       , lockedRooms = vLocked
                       , timeEnemies = vTimeEnemy
                       , timeScreen = vTimeScreen
                       }
      else fullDefaults



-- also reads the config values, but additionally displays warnings if defaults will be used
initConfig :: IO Config
initConfig = do
   fileExists <- doesFileExist "settings.cfg"
   if fileExists
      then do
         vMap <- initGetMap
         vRoom <- initGetRoom
         vStartHP <- initGetStartHP
         vEnemy <- initGetEnemies
         vItem <- initGetItems
         vLocked <- initGetLockedRooms
         vTimeEnemy <- initGetTimeEnemy
         vTimeScreen <- initGetTimeScreen
         return Config { map = vMap
                       , room = vRoom
                       , startingHP = vStartHP
                       , enemyRange = vEnemy
                       , itemRange = vItem
                       , lockedRooms = vLocked
                       , timeEnemies = vTimeEnemy
                       , timeScreen = vTimeScreen
                       }
      else initFullDefaults
