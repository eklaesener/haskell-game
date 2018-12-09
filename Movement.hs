module Movement where

import Data.Array




-- naming some values so, if necessary, changing them afterwards is easier
roomSize = 7
leftDoorCoord = toInteger (floor (fromIntegral roomSize / 2))
rightDoorCoord = toInteger (ceiling (fromIntegral roomSize / 2))
numRooms = ( highBoundNS - lowBoundNS + 1 ) * ( highBoundWE - lowBoundWE + 1)

lowBoundNS = 0
lowBoundWE = 0
highBoundNS = 2
highBoundWE = 3

isDoor x = (x >= leftDoorCoord) && (x <= rightDoorCoord) -- a simple check if you're standing in front of a door

data Direction = North | West | South | East
   deriving (Show, Read, Eq, Ord, Enum)


data Movement = Advance | TurnLeft | BackOff | TurnRight
   deriving (Show, Eq, Enum)


type Location = (Int, Int) -- which room you're in, numbers are the same as the index of the specific room

type Map = Array (Int, Int) Bool


-- choosing which rooms are locked - True means locked, False means unlocked

oldGameMap :: Map
oldGameMap = array ((0,0), (2,3)) [
   ((0,0), True)
   ,((0,1), False)
   ,((0,2), False)
   ,((0,3), False)
   ,((1,0), False)
   ,((1,1), True)
   ,((1,2), False)
   ,((1,3), False)
   ,((2,0), True)
   ,((2,1), False)
   ,((2,2), True)
   ,((2,3), True)
   ]

-- this is way better than constantly having to type those functions
oldLowBoundNS = fst $ fst $ bounds oldGameMap
oldLowBoundWE = snd $ fst $ bounds oldGameMap
oldHighBoundNS = (fst . snd . bounds) oldGameMap
oldHighBoundWE = (snd . snd . bounds) oldGameMap


type InnerLocation = (Integer, Integer) -- all rooms are the same size, so a simple coordinate pair from 0 to roomSize will suffice

type Position = (Location, InnerLocation, Direction) -- your position consists of the room you're in, your position in that room and the direction you're facing currently


move :: Position -> Movement -> Either String Position

move (l, il, North) TurnRight = Right (l, il, East)
move (l, il, d) TurnRight = Right (l, il, toEnum (fromEnum d - 1))




move (l, il, East) TurnLeft = Right (l, il, North)
move (l, il, d) TurnLeft = Right (l, il, toEnum (fromEnum d - 1))




move ((ns, we), (x, y), North) Advance
   | (x == 0) && (ns == lowBoundNS) = if (isDoor y) then Left "Door blocked" else Left "Wall"
   | x == 0 = if (isDoor y) then Right ((ns - 1, we), (roomSize, y), North) else Left "Wall"
   | otherwise = Right ((ns, we), (x - 1, y), North)

move ((ns, we), (x, y), West) Advance
   | (y == 0) && (we == lowBoundWE) = if (isDoor x) then Left "Door blocked" else Left "Wall"
   | y == 0 = if (isDoor x) then Right ((ns, we - 1), (x, roomSize), West) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y - 1), West)

move ((ns, we), (x, y), South) Advance
   | (x == roomSize) && (ns == highBoundNS) = if (isDoor y) then Left "Door blocked" else Left "Wall"
   | x == roomSize = if (isDoor y) then Right ((ns + 1, we), (0, y), South) else Left "Wall"
   | otherwise = Right ((ns, we), (x + 1, y), South)

move ((ns, we), (x, y), East) Advance
   | (y == roomSize) && (we == highBoundWE) = if (isDoor x) then Left "Door blocked" else Left "Wall"
   | y == roomSize = if (isDoor x) then Right ((ns, we + 1), (x, 0), East) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y + 1), East)


move ((ns, we), (x, y), North) BackOff
   | (x == roomSize) && (ns == highBoundNS) = if (isDoor y) then Left "Door blocked" else Left "Wall"
   | x == roomSize = if (isDoor y) then Right ((ns + 1, we), (0, y), North) else Left "Wall"
   | otherwise = Right ((ns, we), (x + 1, y), North)

move ((ns, we), (x, y), West) BackOff
   | (y == roomSize) && (we == highBoundWE) = if (isDoor x) then Left "Door blocked" else Left "Wall"
   | y == roomSize = if (isDoor x) then Right ((ns, we + 1), (x, 0), West) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y + 1), West)

move ((ns, we), (x, y), South) BackOff
   | (x == 0) && (ns == lowBoundNS) = if (isDoor y) then Left "Door blocked" else Left "Wall"
   | x == 0 = if (isDoor y) then Right ((ns - 1, we), (roomSize, y), South) else Left "Wall"
   | otherwise = Right ((ns, we), (x - 1, y), South)

move ((ns, we), (x, y), East) BackOff
   | (y == 0) && (we == lowBoundWE) = if (isDoor x) then Left "Door blocked" else Left "Wall"
   | y == 0 = if (isDoor x) then Right ((ns, we - 1), (x, roomSize), East) else Left "Wall"
   | otherwise = Right ((ns, we), (x, y - 1), East)




