import Data.Array
module Mapping where

data Direction = North | West | South | East
   deriving (Show, Read, Eq, Ord, Enum)


data Movement = Advance | TurnLeft | BackOff | TurnRight
   deriving (Show, Eq, Enum)


type Location = (Int, Int)


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

data InnerLocation = Door | Corner | Middle
   deriving (Show, Read, Eq)

type Position = (Location, InnerLocation, Direction)


move :: Position -> Movement -> Either String Position

move (l, il, North) TurnRight = Right (l, il, East)
move (l, il, d) TurnRight = Right (l, il, toEnum (fromEnum d - 1))

move (l, il, East) TurnLeft = Right (l, il, North)
move (l, il, d) TurnLeft = Right (l, il, toEnum (fromEnum d - 1))

move ((ns, we), Door, d) Advance = if ((d `mod` 2) == 1)
   then if ( ((fst $ snd $ bounds gameMap) <= (d - 1)) && (fst $ fst $ bounds gameMap) >= (d - 1) ) then Right (
