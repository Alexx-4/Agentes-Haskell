module Environment
(
    createPos,
    locateObjects,
    updatePos,
    isValidPos
) 
where

import Objects
import Random
import System.Random


-- direcciones posibles de casillas adyacentes
dirx :: [Int]
dirx = [1, 0, -1,  0]

diry :: [Int]
diry = [0, 1,  0, -1]
                                
                                            
-- devuelve una lista con n objetos ubicados en posiciones aleatorias recibe el nombre 
-- del objeto, la cantidad de objetos a ubicar y la lista con las posiciones que quedan vacias (emptyPos), 
-- ademas del generador que utiliza el random
locateObjects :: String -> Int -> [(Int,Int)] -> StdGen -> [Object]
locateObjects name n emptyPos gen = let indexes = rand n (0, ((length emptyPos) - 1)) gen
                                    in  [createObject name (emptyPos !! i) | i <- indexes]


-- crea una lista con todas las posiciones posibles
-- recibe las dimensiones nxm del tablero
createPos :: Int -> Int -> [(Int,Int)]
createPos n m = [(a,b) | a <- [0..n-1], b <- [0..m-1]]

-- recibe dos listas y elimina en la segunda lista los elementos de la primera
updatePos :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
updatePos [] list = list 
updatePos ((a,b):xs) list = updatePos xs (deletePos (a,b) list)

-- elimina la posicion x,y de la lista de posiciones
deletePos :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
deletePos (x,y) [] = []
deletePos (x,y) ((a,b): xs) = if a == x && b == y then deletePos (x,y) xs else (a,b): deletePos (x,y) xs

-- dada una posicion x,y del ambiente devuelve si esta dentro de los limites de este
isValidPos :: Int -> Int -> Int -> Int -> Bool
isValidPos x y n m = if x >= 0 && x < n && y >= 0 && y < m then True else False

-- devuelve una direccion aleatoria
-- recibe dos generadores
randomDir :: StdGen -> StdGen -> (Int,Int)
randomDir genx geny = (dirx !! xi ,diry !! yi)
                where xi = (rand 1 (0,3) genx) !! 0
                      yi = (rand 1 (0,3) geny) !! 0


-- verifica si un ninno puede moverse en la direccion indicada,
-- en caso que hayan obstaculos en esa direccion, verifica si puede moverlos
canChildMove :: Int -> Int -> Int -> Int -> (Int,Int) -> [(Int,Int)] -> [Object] -> Bool
canChildMove x y n m (dx,dy) freePos obstList 
            | not $ isValidPos x y n m = False
            | elem (x,y) freePos = True
            | elem (x,y) (getPosObjects obstList) = canChildMove (x+dx) (y+dy) n m (dx,dy) freePos obstList
            | otherwise = False



