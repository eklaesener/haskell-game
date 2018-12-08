import Data.Array
module Mapping where


-- naming some values so, if necessary, changing them afterwards is easier
roomSize = 7
leftDoorCoord = 3
rightDoorCoord = 4

lowBoundNS = 0
lowBoundWE = 0
highBoundWE = 2
highBoundWE = 3

isDoor x = (x >= leftDoorCoord) && (x <= rightDoorCoord) -- a simple check if you're standing in front of a door

data Direction = North | West | South | East
   deriving (Show, Read, Eq, Ord, Enum)


data Movement = Advance | TurnLeft | BackOff | TurnRight
   deriving (Show, Eq, Enum)


type Location = (Int, Int) -- which room you're in, numbers are the same as the index of the specific room


gameMap = array ((0,0), (2,3)) [
   ((0,0), "mountains"),
   ((0,1), "home sweet home"),
   ((0,2), "dark forest"),
   ((0,3), "sandy beach"),
   ((1,0), "mountains lake"),
   ((1,1), "green clearing"),
   ((1,2), "cave entrance"),
   ((1,3), "dark cave"),
   ((2,0), "vibrant village"),
   ((2,1), "deadly desert"),
   ((2,2), "bridge over river"),
   ((2,3), "deserted island") ]

-- this is way better than constantly having to type those functions
oldLowBoundNS = fst $ fst $ bounds oldGameMap
oldLowBoundWE = snd $ fst $ bounds oldGameMap
oldHighBoundNS = (fst . snd . bounds) oldGameMap
oldHighBoundWE = (snd . snd . bounds) oldGameMap


type InnerLocation = (Int, Int) -- all rooms are the same size, so a simple coordinate pair from 0 to roomSize will suffice

type Position = (Location, InnerLocation, Direction) -- your position consists of the room you're in, your position in that room and the direction you're facing currently


move :: Position -> Movement -> Either String Position

move (l, il, North) TurnRight = Right (l, il, East)
move (l, il, d) TurnRight = Right (l, il, toEnum (fromEnum d - 1))




move (l, il, East) TurnLeft = Right (l, il, North)
move (l, il, d) TurnLeft = Right (l, il, toEnum (fromEnum d - 1))




move ((lowBoundNS, _), (0, y), North) Advance = if (isDoor y) then Left "Door blocked" else Left "Wall"
move ((ns, we), (0, y), North) Advance = if (isDoor y) then Right ((ns - 1, we), (roomSize, y), North) else Left "Wall"
move ((ns, we), (x, y), North) Advance = Right ((ns, we), (x - 1, y), North)

move ((_, lowBoundWE), (x, 0), West) Advance = if (isDoor x) then Left "Door blocked" else Left "Wall"
move ((ns, we), (x, 0), West) Advance = if (isDoor x) then Right ((ns, we - 1), (x, roomSize), West) else Left "Wall"
move ((ns, we), (x, y), West) Advance = Right ((ns, we), (x, y - 1), West)

move ((highBoundNS, _), (roomSize, y), South) Advance = if (isDoor y) then Left "Door blocked" else Left "Wall"
move ((ns, we), (roomSize, y), South) Advance = if (isDoor y) then Right ((ns + 1, we), (0, y), South) else Left "Wall"
move ((ns, we), (x, y), South) Advance = Right ((ns, we), (x + 1, y), South)

move ((_, highBoundWE), (x, roomSize), East) Advance = if (isDoor x) then Left "Door blocked" else Left "Wall"
move ((ns, we), (x, roomSize), East) Advance = if (isDoor x) then Right ((ns, we + 1), (x, 0), East) else Left "Wall"
move ((ns, we), (x, y), East) Advance = Right ((ns, we), (x, y + 1), East)



move ((highBoundNS, _), (roomSize, y), North) Backoff = if (isDoor y) then Left "Door blocked" else Left "Wall"
move ((ns, we), (roomSize, y), North) Backoff = if (isDoor y) then Right ((ns + 1, we), (0, y), North) else Left "Wall"
move ((ns, we), (x, y), North) Backoff = Right ((ns, we), (x + 1, y), North)

move ((_, highBoundWE), (x, roomSize), West) Backoff = if (isDoor x) then Left "Door blocked" else Left "Wall"
move ((ns, we), (x, roomSize), West) Backoff = if (isDoor x) then Right ((ns, we + 1), (x, 0), West) else Left "Wall"
move ((ns, we), (x, y), West) Backoff = Right ((ns, we), (x, y + 1), West)

move ((lowBoundNS, _), (0, y), South) Backoff = if (isDoor y) then Left "Door blocked" else Left "Wall"
move ((ns, we), (0, y), South) Backoff = if (isDoor y) then Right ((ns - 1, we), (roomSize, y), South) else Left "Wall"
move ((ns, we), (x, y), South) Backoff = Right ((ns, we), (x - 1, y), South)

move ((_, lowBoundWE), (x, 0), East) Backoff = if (isDoor x) then Left "Door blocked" else Left "Wall"
move ((ns, we), (x, 0), East) Backoff = if (isDoor x) then Right ((ns, we - 1), (x, roomSize), East) else Left "Wall"
move ((ns, we), (x, y), East) Backoff = Right ((ns, we), (x, y - 1), East)
