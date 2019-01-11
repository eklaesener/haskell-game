module Messages where

import System.Random

getWallMsg :: IO String
getWallMsg = do
   gen <- newStdGen
   return . (wallMsgs!!) . head $ randomRs (0, length wallMsgs - 1) gen

wallMsgs :: [String]
wallMsgs = 
   ["You don't possess the ability to phase through walls yet, unfortunately."
   ,"I'm afraid I can't let you do that."
   ,"Why don't you try to walk through a door instead of a wall next time?"
   ]


getDoorBlockedMsg :: IO String
getDoorBlockedMsg = do
   _ <- newStdGen
   gen <- getStdGen
   return . (doorBlockedMsgs!!) . head $ randomRs (0, length doorBlockedMsgs - 1) gen

doorBlockedMsgs :: [String]
doorBlockedMsgs =
   ["Oh no! This tunnel has fallen in!"
   ,"You see multiple cracks in the ceiling. You decide you don't want to enter this doorway after all, fearing it might fall in."
   ,"The floor in front of you - well, \"floor\" - turns out to be a deep hole."
   ]


getRoomLockedMsg :: IO String
getRoomLockedMsg = do
   gen <- newStdGen
   return . (roomLockedMsgs!!) . head $ randomRs (0, length roomLockedMsgs - 1) gen

roomLockedMsgs :: [String]
roomLockedMsgs =
   ["This door is locked! You'll need a key to unlock it."
   ,"You cannot pass. I want the key of this room to unlock myself. You cannot pass."
   ,"You try the door, but it won't budge. Maybe a key would help..."
   ]


getWallPushMsg :: IO String
getWallPushMsg = do
   _ <- newStdGen
   gen <- getStdGen
   return . (wallPushMsgs!!) . head $ randomRs (0, length wallPushMsgs - 1) gen

wallPushMsgs :: [String]
wallPushMsgs =
   ["Try as you might, you still can't push the ladder through the wall."
   ,"Maybe try to push the ladder through a door, not through a wall?"
   ,"This is a wall. It has the property of not allowing big enough things to go\nthrough it. You should know that."
   ]



getOutOfReachMsg :: String -> IO String
getOutOfReachMsg name = do
   gen <- newStdGen
   return . (outOfReachMsgs name !!) . head $ randomRs (0, length outOfReachMsgs - 1) gen

outOfReachMsgs :: String -> [String]
outOfReachMsgs name =
   [""
   ]


getVictorMsg :: String -> IO String
getVictorMsg name = do
   gen <- newStdGen
   return . (victorMsgs name !!) . head $ randomRs (0, length victorMsgs - 1) gen

victorMsgs :: String -> [String]
victorMsgs name =
   [""
   ]


getHitMsg :: String -> IO String
getHitMsg name = do
   gen <- newStdGen
   return . (hitMsgs name !!) . head $ randomRs (0, length hitMsgs - 1) gen

hitMsgs :: String -> [String]
hitMsgs name =
   [""
   ]


getShieldHitMsg :: String -> IO String
getShieldHitMsg name = do
   gen <- newStdGen
   return . (shieldHitMsgs name !!) . head $ randomRs (0, length shieldHitMsgs - 1) gen

shieldHitMsgs :: String -> [String]
shieldHitMsgs name =
   [""
   ]


getShieldDestroyedMsg :: String -> IO String
getShieldDestroyedMsg name = do
   gen <- newStdGen
   return . (shieldDestroyedMsgs name !!) . head $ randomRs (0, length shieldDestroyedMsgs - 1) gen

shieldDestroyedMsgs :: String -> [String]
shieldDestroyedMsgs name =
   [""
   ]


getNoItemHereMsg :: IO String
getNoItemHereMsg = do
   gen <- newStdGen
   return . (noItemHereMsgs!!) . head $ randomRs (0, length noItemHereMsgs - 1) gen

noItemHereMsgs :: [String]
noItemHereMsgs =
   [""
   ]


getItemPickedUpMsg :: String -> IO String
getItemPickedUpMsg name = do
   gen <- newStdGen
   return . (itemPickedUpMsgs name !!) . head $ randomRs (0, length itemPickedUpMsgs - 1) gen

itemPickedUpMsgs :: String -> [String]
itemPickedUpMsgs name =
   [""
   ]


getNoWeaponMsg :: IO String
getNoWeaponMsg = do
   gen <- newStdGen
   return . (noWeaponMsgs!!) . head $ randomRs (0, length noWeaponMsgs - 1) gen

noWeaponMsgs :: [String]
noWeaponMsgs =
   [""
   ]
