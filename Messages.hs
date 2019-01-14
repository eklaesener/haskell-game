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


getEnemyPushMsg :: IO String
getEnemyPushMsg = do
   _ <- newStdGen
   gen <- getStdGen
   return . (enemyPushMsgs!!) . head $ randomRs (0, length enemyPushMsgs - 1) gen

enemyPushMsgs :: [String]
enemyPushMsgs = 
   ["You can't go through an enemy, dummy!"
   ,"Why don't you just try going around?"
   ]


getOutOfReachMsg :: String -> IO String
getOutOfReachMsg name = do
   gen <- newStdGen
   return . (outOfReachMsgs name !!) . head $ randomRs (0, length (outOfReachMsgs name) - 1) gen

outOfReachMsgs :: String -> [String]
outOfReachMsgs name =
   ["You reach out your arms, but you still can't reach that  " ++ name ++ "!"
   ,"Nope, that " ++ name ++ " is out of your reach!"
   ]


getVictorMsg :: String -> IO String
getVictorMsg name = do
   gen <- newStdGen
   return . (victorMsgs name !!) . head $ randomRs (0, length (victorMsgs name) - 1) gen

victorMsgs :: String -> [String]
victorMsgs name =
   ["You easily defeated that " ++ name ++ ". On to the next task!"
   ,"Congrats, you slew the evil " ++ name ++ "!"
   ]


getHitMsg :: String -> IO String
getHitMsg name = do
   gen <- newStdGen
   return . (hitMsgs name !!) . head $ randomRs (0, length (hitMsgs name) - 1) gen

hitMsgs :: String -> [String]
hitMsgs name =
   ["That was a hit! That " ++ name ++ " should take care."
   ,"You actually managed to hit that " ++ name ++ ". Finally."
   ]


getShieldHitMsg :: String -> IO String
getShieldHitMsg name = do
   gen <- newStdGen
   return . (shieldHitMsgs name !!) . head $ randomRs (0, length (shieldHitMsgs name) - 1) gen

shieldHitMsgs :: String -> [String]
shieldHitMsgs name =
   ["Oh no! That " ++ name ++ "'s shield absorbed the blow!"
   ,"You hit " ++ name ++ "'s shield. Good job."
   ]


getShieldDestroyedMsg :: String -> IO String
getShieldDestroyedMsg name = do
   gen <- newStdGen
   return . (shieldDestroyedMsgs name !!) . head $ randomRs (0, length (shieldDestroyedMsgs name) - 1) gen

shieldDestroyedMsgs :: String -> [String]
shieldDestroyedMsgs name =
   ["That was it! You destroyed " ++ name ++ "'s shield."
   ]


getNoItemHereMsg :: IO String
getNoItemHereMsg = do
   gen <- newStdGen
   return . (noItemHereMsgs!!) . head $ randomRs (0, length noItemHereMsgs - 1) gen

noItemHereMsgs :: [String]
noItemHereMsgs =
   ["There aren't any items here, as proven by that map."
   ]


getItemPickedUpMsg :: String -> IO String
getItemPickedUpMsg name = do
   gen <- newStdGen
   return . (itemPickedUpMsgs name !!) . head $ randomRs (0, length (itemPickedUpMsgs name) - 1) gen

itemPickedUpMsgs :: String -> [String]
itemPickedUpMsgs name =
   ["You picked up a mighty " ++ name ++ "!"
   ,"Congrats! You just picked up a useful " ++ name ++ "."
   ]


getNoWeaponMsg :: IO String
getNoWeaponMsg = do
   gen <- newStdGen
   return . (noWeaponMsgs!!) . head $ randomRs (0, length noWeaponMsgs - 1) gen

noWeaponMsgs :: [String]
noWeaponMsgs =
   ["You can't drop your fists, but if you really want to try, go stand in front of a monster..."
   ,"You don't have a weapon you could drop!"
   ]


getWeaponDroppedMsg :: String -> IO String
getWeaponDroppedMsg name = do
   gen <- newStdGen
   return . (weaponDroppedMsgs name !!) . head $ randomRs (0, length (weaponDroppedMsgs name) - 1) gen

weaponDroppedMsgs :: String -> [String]
weaponDroppedMsgs name =
   ["You successfully dropped the " ++ name ++ "."
   ]


getNoShieldMsg :: IO String
getNoShieldMsg = do
   gen <- newStdGen
   return . (noShieldMsgs!!) . head $ randomRs (0, length noShieldMsgs - 1) gen

noShieldMsgs :: [String]
noShieldMsgs =
   ["You don't have a shield you could drop!"
   ]


getShieldDroppedMsg :: String -> IO String
getShieldDroppedMsg name = do
   gen <- newStdGen
   return . (shieldDroppedMsgs name !!) . head $ randomRs (0, length (shieldDroppedMsgs name) - 1) gen

shieldDroppedMsgs :: String -> [String]
shieldDroppedMsgs name =
   ["You successfully dropped the " ++ name ++ "."
   ]


getWeaponSwapNotEnoughMsg :: IO String
getWeaponSwapNotEnoughMsg = do
   gen <- newStdGen
   return . (weaponSwapNotEnoughMsgs!!) . head $ randomRs (0, length weaponSwapNotEnoughMsgs - 1) gen

weaponSwapNotEnoughMsgs :: [String]
weaponSwapNotEnoughMsgs =
   ["You don't have any unequipped weapons in your inventory!"
   ]


getWeaponSwapOneMsg :: String -> IO String
getWeaponSwapOneMsg name = do
   gen <- newStdGen
   return . (weaponSwapOneMsgs name !!) . head $ randomRs (0, length (weaponSwapOneMsgs name) - 1) gen

weaponSwapOneMsgs :: String -> [String]
weaponSwapOneMsgs name =
   ["You swapped from your fists to that vicious " ++ name ++ "."
   ]


getWeaponSwapTwoMsg :: String -> String -> IO String
getWeaponSwapTwoMsg oldName newName = do
   gen <- newStdGen
   return . (weaponSwapTwoMsgs oldName newName !!) . head $ randomRs (0, length (weaponSwapTwoMsgs oldName newName) - 1) gen

weaponSwapTwoMsgs :: String -> String -> [String]
weaponSwapTwoMsgs oldName newName =
   ["You swapped that " ++ oldName ++ " for this " ++ newName ++ "."
   ]


getShieldSwapNotEnoughMsg :: IO String
getShieldSwapNotEnoughMsg = do
   gen <- newStdGen
   return . (shieldSwapNotEnoughMsgs!!) . head $ randomRs (0, length shieldSwapNotEnoughMsgs - 1) gen

shieldSwapNotEnoughMsgs :: [String]
shieldSwapNotEnoughMsgs =
   ["You don't have any unequipped shields in your inventory!"
   ]


getShieldSwapOneMsg :: String -> IO String
getShieldSwapOneMsg name = do
   gen <- newStdGen
   return . (shieldSwapOneMsgs name !!) . head $ randomRs (0, length (shieldSwapOneMsgs name) - 1) gen

shieldSwapOneMsgs :: String -> [String]
shieldSwapOneMsgs name =
   ["You equipped that mighty " ++ name ++ "."
   ]


getShieldSwapTwoMsg :: String -> String -> IO String
getShieldSwapTwoMsg oldName newName = do
   gen <- newStdGen
   return . (shieldSwapTwoMsgs oldName newName !!) . head $ randomRs (0, length (shieldSwapTwoMsgs oldName newName) - 1) gen

shieldSwapTwoMsgs :: String -> String -> [String]
shieldSwapTwoMsgs oldName newName =
   ["You swapped that " ++ oldName ++ " for this " ++ newName ++ "."
   ]



