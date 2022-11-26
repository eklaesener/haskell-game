module Messages where

import System.Random (newStdGen, getStdGen, randomRs)



introMsg :: String -> String
introMsg name =  "\nWell, "
    ++ name
    ++ ", you're in quite a pickle right now.\n"
    ++ "Remember? You were exploring a cave, "
    ++ "but the floor you were standing on fell down and you with it...\n"
    ++ "And, I'm afraid, these lower caverns are said "
    ++ "to be haunted by various monsters.\n"
    ++ "Maybe there's a ladder here somewhere, to get back up?"

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
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = doorBlockedMsgs

doorBlockedMsgs :: [String]
doorBlockedMsgs =
    ["Oh no! This tunnel has fallen in!"
    ,"You see multiple cracks in the ceiling. "
        ++ "You decide you don't want to enter "
        ++ "this doorway after all, fearing it might fall in."
    ,"The floor in front of you - well, \"floor\" "
        ++ "- turns out to be a deep hole."
    ]


getRoomLockedMsg :: IO String
getRoomLockedMsg = do
    gen <- newStdGen
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = roomLockedMsgs

roomLockedMsgs :: [String]
roomLockedMsgs =
    ["This door is locked! You'll need a key to unlock it."
    ,"You cannot pass. I want the key of this room to unlock myself."
    ,"You try the door, but it won't budge. Maybe a key would help..."
    ]


getWallPushMsg :: IO String
getWallPushMsg = do
    _ <- newStdGen
    gen <- getStdGen
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = wallPushMsgs

wallPushMsgs :: [String]
wallPushMsgs =
    ["Try as you might, you still can't push the ladder through the wall."
    ,"Maybe try to push the ladder through a door, not through a wall?"
    ,"This is a wall. It has the property of not allowing things to go"
        ++ "\nthrough it. You should know that."
    ]


getEnemyPushMsg :: IO String
getEnemyPushMsg = do
    _ <- newStdGen
    gen <- getStdGen
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = enemyPushMsgs

enemyPushMsgs :: [String]
enemyPushMsgs = 
    ["You can't go through an enemy, dummy!"
    ,"Why don't you just try going around?"
    ]


getOutOfReachMsg :: String -> IO String
getOutOfReachMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = outOfReachMsgs

outOfReachMsgs :: String -> [String]
outOfReachMsgs name =
    ["You reach out your arms, but you still can't reach that  " ++ name ++ "!"
    ,"Nope, that " ++ name ++ " is out of your reach!"
    ]


getVictorMsg :: String -> IO String
getVictorMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = victorMsgs

victorMsgs :: String -> [String]
victorMsgs name =
    ["You easily defeated that " ++ name ++ ". On to the next task!"
    ,"Congrats, you slew the evil " ++ name ++ "!"
    ]


getHitMsg :: String -> IO String
getHitMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = hitMsgs

hitMsgs :: String -> [String]
hitMsgs name =
    ["That was a hit! That " ++ name ++ " should take care."
    ,"You actually managed to hit that " ++ name ++ ". Finally."
    ]


getShieldHitMsg :: String -> IO String
getShieldHitMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = shieldHitMsgs

shieldHitMsgs :: String -> [String]
shieldHitMsgs name =
    ["Oh no! That " ++ name ++ "'s shield absorbed the blow!"
    ,"You hit that " ++ name ++ "'s shield. Good job."
    ]


getShieldDestroyedMsg :: String -> IO String
getShieldDestroyedMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = shieldDestroyedMsgs

shieldDestroyedMsgs :: String -> [String]
shieldDestroyedMsgs name =
    ["That was it! You destroyed that " ++ name ++ "'s shield."
    ]


getNoItemHereMsg :: IO String
getNoItemHereMsg = do
    gen <- newStdGen
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = noItemHereMsgs

noItemHereMsgs :: [String]
noItemHereMsgs =
    ["There aren't any items here, as proven by that map."
    ]


getItemPickedUpMsg :: String -> IO String
getItemPickedUpMsg name = do
   gen <- newStdGen
   return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = itemPickedUpMsgs

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
    ["You can't drop your fists, but if you really want to try, "
        ++ "go stand in front of a monster."
    ,"You don't have a weapon you could drop!"
    ]


getWeaponDroppedMsg :: String -> IO String
getWeaponDroppedMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = weaponDroppedMsgs

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
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = shieldDroppedMsgs

shieldDroppedMsgs :: String -> [String]
shieldDroppedMsgs name =
    ["You successfully dropped the " ++ name ++ "."
    ]


getWeaponSwapNotEnoughMsg :: IO String
getWeaponSwapNotEnoughMsg = do
    gen <- newStdGen
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = weaponSwapNotEnoughMsgs

weaponSwapNotEnoughMsgs :: [String]
weaponSwapNotEnoughMsgs =
   ["You don't have any unequipped weapons in your inventory!"
   ]


getWeaponSwapOneMsg :: String -> IO String
getWeaponSwapOneMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = weaponSwapOneMsgs

weaponSwapOneMsgs :: String -> [String]
weaponSwapOneMsgs name =
    ["You swapped from your fists to that vicious " ++ name ++ "."
    ]


getWeaponSwapTwoMsg :: String -> String -> IO String
getWeaponSwapTwoMsg oldName newName = do
    gen <- newStdGen
    return . (msgs !!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = weaponSwapTwoMsgs oldName newName

weaponSwapTwoMsgs :: String -> String -> [String]
weaponSwapTwoMsgs oldName newName =
    ["You swapped that " ++ oldName ++ " for this " ++ newName ++ "."
    ]


getShieldSwapNotEnoughMsg :: IO String
getShieldSwapNotEnoughMsg = do
    gen <- newStdGen
    return . (msgs!!) . head $ randomRs (0, length msgs - 1) gen
  where
    msgs = shieldSwapNotEnoughMsgs

shieldSwapNotEnoughMsgs :: [String]
shieldSwapNotEnoughMsgs =
    ["You don't have any unequipped shields in your inventory!"
    ]


getShieldSwapOneMsg :: String -> IO String
getShieldSwapOneMsg name = do
    gen <- newStdGen
    return . (msgs name !!) . head $ randomRs (0, length (msgs name) - 1) gen
  where
    msgs = shieldSwapOneMsgs

shieldSwapOneMsgs :: String -> [String]
shieldSwapOneMsgs name =
    ["You equipped that mighty " ++ name ++ "."
    ]


getShieldSwapTwoMsg :: String -> String -> IO String
getShieldSwapTwoMsg oldName newName = do
    gen <- newStdGen
    return . (msgs !!) . head $ randomRs (0, length (msgs) - 1) gen
  where
    msgs = shieldSwapTwoMsgs oldName newName

shieldSwapTwoMsgs :: String -> String -> [String]
shieldSwapTwoMsgs oldName newName =
    ["You swapped that " ++ oldName ++ " for this " ++ newName ++ "."
    ]



