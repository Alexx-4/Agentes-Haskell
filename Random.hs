module Random
(
    rand
) 
where
    
import System.Random
import Data.List

-- devuelve una lista de n numeros aleatorios distintos en el
-- intervalo [min,max]
rand :: Int -> (Int, Int) -> StdGen -> [Int]
rand n (min, max) gen =  take n $ nub $ randomRs (min, max) gen :: [Int]