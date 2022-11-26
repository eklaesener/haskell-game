module Config where

import Prelude hiding (map)
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



pressEnterStr :: String
pressEnterStr = ".\nPress Enter to continue..."

notFoundStr :: String -> String
notFoundStr opt = "Warning!\nThe \"" ++ opt ++ "\" option is unavailable, defaulting to "



defaultMap :: (Int, Int)
defaultMap = (4, 4)

defaultRoom :: (Int, Int)
defaultRoom = (8, 8)

defaultStartHP :: Float
defaultStartHP = 100.0

defaultEnemyRange :: (Int, Int)
defaultEnemyRange = (2, 20)

defaultItemRange :: (Int, Int)
defaultItemRange = (5, 25)

defaultLockedRooms :: Int
defaultLockedRooms = 20

defaultTimeEnemies :: Int
defaultTimeEnemies = 500

defaultTimeScreen :: Int
defaultTimeScreen = 100


fullDefaults :: Config
fullDefaults = Config
    defaultMap
    defaultRoom
    defaultStartHP
    defaultEnemyRange
    defaultItemRange
    defaultLockedRooms
    defaultTimeEnemies
    defaultTimeScreen

initFullDefaults :: IO Config
initFullDefaults = do
   putStrLn $ "Warning!\nThe config file \"settings.cfg\" could not be found or read. Using default values" ++ pressEnterStr
   _ <- getLine
   return fullDefaults


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
         Left _ -> return def
         Right cols -> return (x, cols)
      Right rows -> case rv2 of
         Left _ -> return (rows, y)
         Right cols -> return (rows, cols)
  where
   def@(x,y) = defaultMap
   


initGetMap :: IO (Int, Int)
initGetMap = do
   rv1 <- runExceptT confMapRows
   rv2 <- runExceptT confMapColumns
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn $ "Warning!\nThe map size options are unavailable, defaulting to " ++ show def ++ pressEnterStr
            _ <- getLine
            return def
         Right cols -> do
            putStrLn $ notFoundStr "maprows" ++ show x ++ pressEnterStr
            _ <- getLine
            return (x, cols)
      Right rows -> case rv2 of
         Left _ -> do
            putStrLn $ notFoundStr "mapcolumns" ++ show y ++ pressEnterStr
            _ <- getLine
            return (rows, y)
         Right cols -> return (rows, cols)
  where
   def@(x, y) = defaultMap



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
         Left _ -> return def
         Right cols -> return (x, cols)
      Right rows -> case rv2 of
         Left _ -> return (rows, y)
         Right cols -> return (rows, cols)
  where
   def@(x, y) = defaultRoom


initGetRoom :: IO (Int, Int)
initGetRoom = do
   rv1 <- runExceptT confRoomRows
   rv2 <- runExceptT confRoomColumns
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn $ "Warning!\nThe room size options are unavailable, defaulting to " ++ show def ++ pressEnterStr
            _ <- getLine
            return def
         Right cols -> do
            putStrLn $ notFoundStr "roomrows" ++ show x ++ pressEnterStr
            _ <- getLine
            return (x, cols)
      Right rows -> case rv2 of
         Left _ -> do
            putStrLn $ notFoundStr "roomcolumns" ++ show y ++ pressEnterStr
            _ <- getLine
            return (rows, y)
         Right cols -> return (rows, cols)
  where
   def@(x, y) = defaultRoom



confStartHP :: ExceptT CPError IO Float
confStartHP = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "startinghealthpoints"

getStartHP :: IO Float
getStartHP = do
   rv <- runExceptT confStartHP
   case rv of
      Left _ -> return defaultStartHP
      Right hp -> return hp

initGetStartHP :: IO Float
initGetStartHP = do
   rv <- runExceptT confStartHP
   case rv of
      Left _ -> do
         putStrLn $ notFoundStr "startinghealthpoints" ++ show def ++ pressEnterStr
         _ <- getLine
         return def
      Right hp -> return hp
  where
   def = defaultStartHP


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
         Left _ -> return def
         Right n -> return (a, n)
      Right m -> case rv2 of
         Left _ -> return (m, b)
         Right n -> return (m, n)
  where
   def@(a, b) = defaultEnemyRange

initGetEnemies :: IO (Int, Int)
initGetEnemies = do
   rv1 <- runExceptT confMinEnemy
   rv2 <- runExceptT confMaxEnemy
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn $ "Warning!\nThe \"minenemies\" and \"maxenemies\" options are unavailable, defaulting to " ++ show def ++ pressEnterStr
            _ <- getLine
            return def
         Right n -> do
            putStrLn $ notFoundStr "minenemies" ++ show a ++ pressEnterStr
            _ <- getLine
            return (a, n)
      Right m -> case rv2 of
         Left _ -> do
            putStrLn $ notFoundStr "maxenemies" ++ show b ++ pressEnterStr
            _ <- getLine
            return (m, b)
         Right n -> return (m, n)
  where
   def@(a, b) = defaultEnemyRange


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
         Left _ -> return def
         Right n -> return (a, n)
      Right m -> case rv2 of
         Left _ -> return (m, b)
         Right n -> return (m, n)
  where
   def@(a, b) = defaultItemRange

initGetItems :: IO (Int, Int)
initGetItems = do
   rv1 <- runExceptT confMinItem
   rv2 <- runExceptT confMaxItem
   case rv1 of
      Left _ -> case rv2 of
         Left _ -> do
            putStrLn $ "Warning!\nThe \"minitems\" and \"maxitems\" options are unavailable, defaulting to " ++ show def ++ pressEnterStr
            _ <- getLine
            return def
         Right n -> do
            putStrLn $ notFoundStr "minitems" ++ show a ++ pressEnterStr
            _ <- getLine
            return (a, n)
      Right m -> case rv2 of
         Left _ -> do
            putStrLn $ notFoundStr "maxitems" ++ show b ++ pressEnterStr
            _ <- getLine
            return (m, b)
         Right n -> return (m, n)
  where
   def@(a, b) = defaultItemRange


confLocked :: ExceptT CPError IO Int
confLocked = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "lockedrooms"


getLockedRooms :: IO Int
getLockedRooms = do
   rv <- runExceptT confLocked
   case rv of
      Left _ -> return defaultLockedRooms
      Right n -> return n

initGetLockedRooms :: IO Int
initGetLockedRooms = do
   rv <- runExceptT confLocked
   case rv of
      Left _ -> do
         putStrLn $ notFoundStr "lockedrooms" ++ show def ++ pressEnterStr
         _ <- getLine
         return def
      Right n -> return n
  where
   def = defaultLockedRooms


confTimeEnemies :: ExceptT CPError IO Int
confTimeEnemies = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "timeenemies"


getTimeEnemies :: IO Int
getTimeEnemies = do
   rv <- runExceptT confTimeEnemies
   case rv of
      Left _ -> return defaultTimeEnemies
      Right n -> return n

initGetTimeEnemies :: IO Int
initGetTimeEnemies = do
   rv <- runExceptT confTimeEnemies
   case rv of
      Left _ -> do
         putStrLn $ notFoundStr "timeenemies" ++ show def ++ pressEnterStr
         _ <- getLine
         return def
      Right n -> return n
  where
   def = defaultTimeEnemies


confTimeScreen :: ExceptT CPError IO Int
confTimeScreen = do
   cp <- join $ liftIO $ readfile emptyCP "settings.cfg"
   get cp "DEFAULT" "timescreen"


getTimeScreen :: IO Int
getTimeScreen = do
   rv <- runExceptT confTimeScreen
   case rv of
      Left _ -> return defaultTimeScreen
      Right n -> return n

initGetTimeScreen :: IO Int
initGetTimeScreen = do
   rv <- runExceptT confTimeScreen
   case rv of
      Left _ -> do
         putStrLn $ notFoundStr "timescreen" ++ show def ++ pressEnterStr
         _ <- getLine
         return def
      Right n -> return n
  where
   def = defaultTimeScreen

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
         vTimeEnemies <- getTimeEnemies
         vTimeScreen <- getTimeScreen
         return Config { map = vMap
                       , room = vRoom
                       , startingHP = vStartHP
                       , enemyRange = vEnemy
                       , itemRange = vItem
                       , lockedRooms = vLocked
                       , timeEnemies = vTimeEnemies
                       , timeScreen = vTimeScreen
                       }
      else return fullDefaults



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
         vTimeEnemies <- initGetTimeEnemies
         vTimeScreen <- initGetTimeScreen
         return Config { map = vMap
                       , room = vRoom
                       , startingHP = vStartHP
                       , enemyRange = vEnemy
                       , itemRange = vItem
                       , lockedRooms = vLocked
                       , timeEnemies = vTimeEnemies
                       , timeScreen = vTimeScreen
                       }
      else initFullDefaults
