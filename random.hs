import System.Random

main = do
   gen <- newStdGen
   print $ take 20 ( randoms (gen) :: [Bool] )

