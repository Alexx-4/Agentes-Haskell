module Environment
(
    createPos,
    locateObjects,
    updatePos,
    isValidPos,
   changeEnvironment
) 
where

import Objects
import Functions

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
randomDir :: StdGen -> (Int,Int,StdGen)
randomDir gen = (dirx !! i, diry !! i, newGen)
                where (i,newGen) = randomR (0,3) gen
                      
-- verifica si un ninno puede moverse en la direccion indicada,
-- en caso que hayan obstaculos en esa direccion, verifica si puede moverlos
canChildMove :: Int -> Int -> Int -> Int -> (Int,Int) -> [(Int,Int)] -> [Object] -> Bool
canChildMove x y n m (dx,dy) freePos obstList 
            | not $ isValidPos x y n m = False
            | elem (x,y) freePos = True
            | elem (x,y) (getPosObjects obstList) = canChildMove (x+dx) (y+dy) n m (dx,dy) freePos obstList
            | otherwise = False


-- simula el comportamiento de cambio del ambiente, donde los ninnos se mueven (o no) aleatoriamente,
-- pueden mover obstaculos y ensuciar.
-- recibe los objetos que representan el ambiente y los modifica segun el cambio aleatorio
changeEnvironment :: [Object] -> [Object] -> [Object] -> [(Int,Int)] -> Int -> Int -> Int -> StdGen -> ([Object],[Object],[Object],[(Int,Int)])
changeEnvironment childList dirtyList obstList freePos n m i gen
        | i == length childList = (childList, dirtyList, obstList, freePos)
        |otherwise = 
            let 
                child = childList !! i
                (dx, dy, newGen) = randomDir gen
                x = row $ location child
                y = column $ location child

                canMove = canChildMove (x+dx) (y+dy) n m (dx,dy) freePos obstList

                newChildList = if canMove
                               then moveChild child dx dy childList
                               else childList

                newObstList = if canMove
                              then moveObstacles (x+dx) (y+dy) (x+dx) (y+dy) obstList freePos dx dy
                              else obstList

                newDirtyList = if canMove
                               then [] --dirtChild
                               else [] --dirtyList

                newfreePos = if canMove
                             then updateFreePos x y (x+dx) (y+dy) dx dy freePos
                             else freePos
            
            in
                changeEnvironment newChildList newDirtyList newObstList newfreePos n m (i+1) newGen
        
updateFreePos :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
updateFreePos x y newx newy dx dy freePos = if elem (newx,newy) freePos 
                                  then (x,y):(updatePos [(newx,newy)] freePos)
                                  else updateFreePos x y (newx + dx) (newy + dy) dx dy freePos