module Movement where

import Data.Array (Array, (!))
import System.Random (Random(..))
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Config as Cfg (config, Config(..))


-- | The four compass directions.
data Direction = North | West | South | East
    deriving (Show, Read, Eq, Ord, Bounded)


-- writing this instance explicitly so we can make it a circle (pred North == East and succ East == North)
instance Enum Direction where

    fromEnum North = 0
    fromEnum West = 1
    fromEnum South = 2
    fromEnum East = 3

    toEnum 0 = North
    toEnum 1 = West
    toEnum 2 = South
    toEnum 3 = East
    toEnum n = toEnum $ n `mod` 4



-- making Directions an instance of Random
instance Random Direction where

    randomR (low, high) gen = (toEnum rndInt, newGen)
      where
        (rndInt, newGen) = randomR (fromEnum low, fromEnum high) gen

    random = randomR (minBound, maxBound)


-- | The different ways characters can move.
data Movement =
    GoForward
    | GoLeft
    | GoBack
    | GoRight
    | TurnLeft
    | TurnAround
    | TurnRight
    deriving (Show, Eq)


-- | Which room you're in, numbers are the same as the index of the specific room.
type Location = (Int, Int)

-- | A representation of the actual room map. True if the room is locked.
type Map = Array Location Bool

{-|
 - All rooms are the same size, so a simple coordinate pair
 - from lowInnerBound to highInnerBound will suffice.
 -}
type InnerLocation = (Int, Int)

{-|
 - Your position consists of the room you're in, your position in that room
 - and the direction you're currently facing.
 -}
type Position = (Location, InnerLocation, Direction)



data Error =
    NoDoor
    | Wall
    | DoorLocked
    | Occupied
    deriving (Show, Read, Eq)


-- getting the values out of the config file
-- similar to Main.hs
config :: Cfg.Config
config = unsafeDupablePerformIO Cfg.config


-- | A few "constants" that are set when needed.
lowBoundNS, lowInnerBoundNS, lowBoundWE, lowInnerBoundWE :: Int
highBoundNS, highInnerBoundNS, highBoundWE, highInnerBoundWE :: Int

lowBoundNS = 1
highBoundNS = fst . Cfg.map $ config

lowBoundWE = 1
highBoundWE = snd . Cfg.map $ config

lowInnerBoundNS = 1
highInnerBoundNS = fst . Cfg.room $ config

lowInnerBoundWE = 1
highInnerBoundWE = snd . Cfg.room $ config


leftDoorCoordNS :: Int
leftDoorCoordNS =
    floor (fromIntegral (highInnerBoundNS + lowInnerBoundNS) / 2 :: Double)

rightDoorCoordNS :: Int
rightDoorCoordNS =
    ceiling (fromIntegral (highInnerBoundNS + lowInnerBoundNS) / 2 :: Double)


leftDoorCoordWE :: Int
leftDoorCoordWE =
    floor (fromIntegral (highInnerBoundWE + lowInnerBoundWE) / 2 :: Double)

rightDoorCoordWE :: Int
rightDoorCoordWE =
    ceiling (fromIntegral (highInnerBoundWE + lowInnerBoundWE) / 2 :: Double)


numRooms :: Int
numRooms = (highBoundNS - lowBoundNS + 1 ) * (highBoundWE - lowBoundWE + 1)


oppositeDirection :: Direction -> Direction
oppositeDirection = succ . succ


-- | Checks if you're standing in front of a door.
isDoorNS :: Int -> Bool
isDoorNS x = x >= leftDoorCoordNS && x <= rightDoorCoordNS

isDoorWE :: Int -> Bool
isDoorWE x = x >= leftDoorCoordWE && x <= rightDoorCoordWE


{-|
 - Also checks for a door, but unlike above,
 - the other coordinate isn't disregarded.
 -}
isDoorFull :: InnerLocation -> Bool
isDoorFull (x,y) = isWall (x,y) && (isDoorNS x || isDoorWE y)


-- | Checks if you're standing in a corner.
isCorner :: InnerLocation -> Bool
isCorner (x,y) =
    (x == lowInnerBoundNS || x == highInnerBoundNS)
    && (y == lowInnerBoundWE || y == highInnerBoundWE)


-- | Checks if you're standing in front of a wall or door.
isWall :: InnerLocation -> Bool
isWall (x,y) =
    x == lowInnerBoundNS
    || y == lowInnerBoundWE
    || x == highInnerBoundNS
    || y == highInnerBoundWE


-- | A default position.
nullPosition :: Position
nullPosition =
    ((lowBoundNS, lowBoundWE), (lowInnerBoundNS, lowInnerBoundWE), North)


{-|
 - Takes two positions and checks
 - if the first one is directly in front of the second one.
 -}
inFrontOf :: Position -> Position -> Bool
(room1, (x1, y1), _) `inFrontOf` (room2, (x2, y2), dir2)
    | room1 /= room2 = False
    | x1 == x2 && y1 == y2 - 1 && dir2 == West = True
    | x1 == x2 && y1 == y2 + 1 && dir2 == East = True
    | x1 == x2 - 1 && y1 == y2 && dir2 == North = True
    | x1 == x2 + 1 && y1 == y2 && dir2 == South = True
    | otherwise = False


{-|
 - Takes two positions and checks if the second one
 - has a direct line of sight to the first one.
 -}
inLOS :: Position -> Position -> Bool
(room1, (x1, y1), _) `inLOS` (room2, (x2, y2), dir2)
    | room1 /= room2 = False
    | x1 == x2 && y1 < y2 && dir2 == West = True
    | x1 == x2 && y1 > y2 && dir2 == East = True
    | x1 < x2 && y1 == y2 && dir2 == North = True
    | x1 > x2 && y1 == y2 && dir2 == South = True
    | otherwise = False


{-|
 - Takes two positions and returns the distance between them
 - (rounded to the nearest Int), errors if they aren't in the same room.
 -}
distanceTo :: Position -> Position -> Int
(room1, (x1, y1), _) `distanceTo` (room2, (x2, y2), _)
    | room1 /= room2 = error "Positions not in the same room!"
    | otherwise = round . root . fromIntegral $ sqr (x1 - x2) + sqr (y1 - y2)
  where
    sqr :: Int -> Int
    sqr x = x * x
    root :: Double -> Double
    root = sqrt


{-|
 - Applies a sequence of movements and returns the end result.
 - See move for input specifications.
 -}
moveMany :: Map -> [Location] -> Position -> [Movement] -> Either Error Position
moveMany roomMap keys start = helper (Right start)
  where
    helper m [] = m
    helper e@(Left _) _ = e
    helper (Right pos) (x:xs) = helper (move roomMap keys pos x) xs

{-|
 - Takes a room map, a list of rooms for which we have keys,
 - the position we're currently at and a movement.
 - Returns Left Error if something went wrong
 - and Right Position if the move is allowed.
 -}
move :: Map -> [Location] -> Position -> Movement -> Either Error Position

move _ _ (l, il, d) TurnRight = Right (l, il, pred d)

move _ _ (l, il, d) TurnLeft = Right (l, il, succ d)

move _ _ (l, il, d) TurnAround = Right (l, il, oppositeDirection d)


move roomMap keyList (room@(roomX, roomY), (x, y), North) GoForward
    | x == lowInnerBoundNS = case () of
      ()
        | isDoorWE y -> case () of
          ()
            | roomX == lowBoundNS -> Left NoDoor
            | roomMap ! room && room `notElem` keyList -> Left DoorLocked
            | otherwise ->
                Right ((roomX - 1, roomY), (highInnerBoundNS, y), North)
        | otherwise -> Left Wall
    | otherwise = Right (room, (x - 1, y), North)

move roomMap keyList (room@(roomX, roomY), (x, y), West) GoForward
    | y == lowInnerBoundWE = case () of
      ()
        | isDoorNS x -> case () of
          ()
            | roomY == lowBoundWE -> Left NoDoor
            | roomMap ! room && room `notElem` keyList -> Left DoorLocked
            | otherwise ->
                Right ((roomX, roomY - 1), (x, highInnerBoundWE), West)
        | otherwise -> Left Wall
    | otherwise = Right (room, (x, y - 1), West)

move roomMap keyList (room@(roomX, roomY), (x, y), South) GoForward
    | x == highInnerBoundNS = case () of
      ()
        | isDoorWE y -> case () of
          ()
            | roomX == highBoundNS -> Left NoDoor
            | roomMap ! room && room `notElem` keyList -> Left DoorLocked
            | otherwise ->
                Right ((roomX + 1, roomY), (lowInnerBoundNS, y), South)
        | otherwise -> Left Wall
    | otherwise = Right (room, (x + 1, y), South)

move roomMap keyList (room@(roomX, roomY), (x, y), East) GoForward
    | y == highInnerBoundWE = case () of
      ()
        | isDoorNS x -> case () of
          ()
            | roomY == highBoundWE -> Left NoDoor
            | roomMap ! room && room `notElem` keyList -> Left DoorLocked
            | otherwise ->
                Right ((roomX, roomY + 1), (x, lowInnerBoundWE), East)
        | otherwise -> Left Wall
    | otherwise = Right (room, (x, y + 1), East)


-- going back is just turning around, going forward, then turning around again
move roomMap keys pos GoBack =
    moveMany roomMap keys pos [TurnAround, GoForward, TurnAround]

-- similarly here...
move roomMap keys pos GoLeft =
    moveMany roomMap keys pos [TurnLeft, GoForward, TurnRight]

-- ... and here
move roomMap keys pos GoRight =
    moveMany roomMap keys pos [TurnRight, GoForward, TurnLeft]
