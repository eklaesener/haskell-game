module Movement where

import Data.Array (Array)
import System.Random (Random(..))



data Direction = North | West | South | East
   deriving (Show, Read, Eq, Ord, Enum)

-- making Directions an instance of Random
instance Random Direction where
   randomR (low, high) gen = (toEnum . fst $ randomR (fromEnum low, fromEnum high) gen, snd $ randomR (fromEnum low, fromEnum high) gen)
   random = randomR (North, East)


data Movement = Advance | TurnLeft | BackOff | TurnRight
   deriving (Show, Eq, Enum)


type Location = (Int, Int) -- which room you're in, numbers are the same as the index of the specific room

type Map = Array (Int, Int) Bool -- a representation of the actual room map

type InnerLocation = (Int, Int) -- all rooms are the same size, so a simple coordinate pair from lowInnerBound to highInnerBound will suffice

type Position = (Location, InnerLocation, Direction) -- your position consists of the room you're in, your position in that room and the direction you're facing currently




-- naming some values so, if necessary, changing them afterwards is easier

leftDoorCoordNS :: Int
leftDoorCoordNS = floor (fromIntegral (highInnerBoundNS + lowInnerBoundNS) / 2 :: Double)

rightDoorCoordNS :: Int
rightDoorCoordNS = ceiling (fromIntegral (highInnerBoundNS + lowInnerBoundNS) / 2 :: Double)


leftDoorCoordWE :: Int
leftDoorCoordWE = floor (fromIntegral (highInnerBoundWE + lowInnerBoundWE) / 2 :: Double)

rightDoorCoordWE :: Int
rightDoorCoordWE = ceiling (fromIntegral (highInnerBoundWE + lowInnerBoundWE) / 2 :: Double)


numRooms :: Int
numRooms = (highBoundNS - lowBoundNS + 1 ) * (highBoundWE - lowBoundWE + 1)


lowBoundNS :: Int
lowBoundNS = 0

lowBoundWE :: Int
lowBoundWE = 0

highBoundNS :: Int
highBoundNS = 3

highBoundWE :: Int
highBoundWE = 3


lowInnerBoundNS :: Int
lowInnerBoundNS = 0

lowInnerBoundWE :: Int
lowInnerBoundWE = 0

highInnerBoundNS :: Int
highInnerBoundNS = 7

highInnerBoundWE :: Int
highInnerBoundWE = 7


-- a simple check if you're standing in front of a door
isDoorNS :: Int -> Bool
isDoorNS x = x >= leftDoorCoordNS && x <= rightDoorCoordNS

isDoorWE :: Int -> Bool
isDoorWE x = x >= leftDoorCoordWE && x <= rightDoorCoordWE


-- also checks for a door, but unlike above, the other coordinate isn't disregarded
isDoorFull :: InnerLocation -> Bool
isDoorFull (x,y)
   | isWall (x,y) && (isDoorNS x || isDoorWE y) = True
   | otherwise = False


-- Are you standing in a corner?
isCorner :: InnerLocation -> Bool
isCorner (x,y)
   | (x == lowInnerBoundNS || x == highInnerBoundNS) && (y == lowInnerBoundWE || y == highInnerBoundWE) = True
   | otherwise = False

-- Are you standing in front of a wall or door?
isWall :: InnerLocation -> Bool
isWall (x,y)
   | x == lowInnerBoundNS || y == lowInnerBoundWE || x == highInnerBoundNS || y == highInnerBoundWE = True
   | otherwise = False


-- the most basic position, for when positions need to be available before they are known
nullPosition :: Position
nullPosition = ((lowBoundNS, lowBoundWE), (lowInnerBoundNS, lowInnerBoundWE), North)


-- takes two positions and checks if the first one is directly in front of the second one
inFrontOf :: Position -> Position -> Bool
((roomX1, roomY1), (x1, y1), _) `inFrontOf` ((roomX2, roomY2), (x2, y2), dir2)
   | roomX1 /= roomX2 || roomY1 /= roomY2 = False
   | x1 == x2 && y1 == y2 - 1 && dir2 == West = True
   | x1 == x2 && y1 == y2 + 1 && dir2 == East = True
   | x1 == x2 - 1 && y1 == y2 && dir2 == North = True
   | x1 == x2 + 1 && y1 == y2 && dir2 == South = True
   | otherwise = False


-- takes two positions and checks if the second one has a direct line of sight to the first one
inLOS :: Position -> Position -> Bool
((roomX1, roomY1), (x1, y1), _) `inLOS` ((roomX2, roomY2), (x2, y2), dir2)
   | roomX1 /= roomX2 || roomY1 /= roomY2 = False
   | x1 == x2 && y1 < y2 && dir2 == West = True
   | x1 == x2 && y1 > y2 && dir2 == East = True
   | x1 < x2 && y1 == y2 && dir2 == North = True
   | x1 > x2 && y1 == y2 && dir2 == South = True
   | otherwise = False


-- takes two positions and returns the distance between them (rounded to the nearest Int), errors if they aren't in the same room
distanceTo :: Position -> Position -> Int
((roomX1, roomY1), (x1, y1), _) `distanceTo` ((roomX2, roomY2), (x2, y2), _)
   | roomX1 /= roomX2 || roomY1 /= roomY2 = error "Positions not in the same room!"
   | otherwise = round . sqrt $ (fromIntegral x1 - fromIntegral x2)^2 + (fromIntegral y1 - fromIntegral y2)^2



-- returns Left String if something went wrong and Right Position if the move is allowed
move :: Position -> Movement -> Either String Position

move (l, il, North) TurnRight = Right (l, il, East)
move (l, il, d) TurnRight = Right (l, il, toEnum (fromEnum d - 1))


move (l, il, East) TurnLeft = Right (l, il, North)
move (l, il, d) TurnLeft = Right (l, il, toEnum (fromEnum d + 1))


move ((ns, we), (x, y), North) Advance
   | (x == lowInnerBoundNS) && (ns == lowBoundNS) = if isDoorWE y then Left "Door blocked" else Left "Wall"
   | x == lowInnerBoundNS = if isDoorWE y then Right ((ns - 1, we), (highInnerBoundNS, y), North) else Left "Wall"
   | otherwise = Right ((ns, we), (x - 1, y), North)

move ((ns, we), (x, y), West) Advance
   | (y == lowInnerBoundWE) && (we == lowBoundWE) = if isDoorNS x then Left "Door blocked" else Left "Wall"
   | y == lowInnerBoundWE = if isDoorNS x then Right ((ns, we - 1), (x, highInnerBoundWE), West) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y - 1), West)

move ((ns, we), (x, y), South) Advance
   | (x == highInnerBoundNS) && (ns == highBoundNS) = if isDoorWE y then Left "Door blocked" else Left "Wall"
   | x == highInnerBoundNS = if isDoorWE y then Right ((ns + 1, we), (lowInnerBoundNS, y), South) else Left "Wall"
   | otherwise = Right ((ns, we), (x + 1, y), South)

move ((ns, we), (x, y), East) Advance
   | (y == highInnerBoundWE) && (we == highBoundWE) = if isDoorNS x then Left "Door blocked" else Left "Wall"
   | y == highInnerBoundWE = if isDoorNS x then Right ((ns, we + 1), (x, lowInnerBoundWE), East) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y + 1), East)


move ((ns, we), (x, y), North) BackOff
   | (x == highInnerBoundNS) && (ns == highBoundNS) = if isDoorWE y then Left "Door blocked" else Left "Wall"
   | x == highInnerBoundNS = if isDoorWE y then Right ((ns + 1, we), (lowInnerBoundNS, y), North) else Left "Wall"
   | otherwise = Right ((ns, we), (x + 1, y), North)

move ((ns, we), (x, y), West) BackOff
   | (y == highInnerBoundWE) && (we == highBoundWE) = if isDoorNS x then Left "Door blocked" else Left "Wall"
   | y == highInnerBoundWE = if isDoorNS x then Right ((ns, we + 1), (x, lowInnerBoundWE), West) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y + 1), West)

move ((ns, we), (x, y), South) BackOff
   | (x == lowInnerBoundNS) && (ns == lowBoundNS) = if isDoorWE y then Left "Door blocked" else Left "Wall"
   | x == lowInnerBoundNS = if isDoorWE y then Right ((ns - 1, we), (highInnerBoundNS, y), South) else Left "Wall"
   | otherwise = Right ((ns, we), (x - 1, y), South)

move ((ns, we), (x, y), East) BackOff
   | (y == lowInnerBoundWE) && (we == lowBoundWE) = if isDoorNS x then Left "Door blocked" else Left "Wall"
   | y == lowInnerBoundWE = if isDoorNS x then Right ((ns, we - 1), (x, highInnerBoundWE), East) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y - 1), East)
