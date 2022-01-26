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

-- casillas posibles de 3x3 donde pueden estar los ninnos
-- una cuadricula de 3x3 esta definida por su esquina superior izquierda
grid3x3 = [(0,0),(0,-1),(0,-2),(-1,0),(-1,-1),(-1,-2),(-2,0),(-2,-1),(-2,-2)]
                                
                                            
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

-- annade la posicion del objeto que se acaba de mover (ahora la casiila esta libre)
-- y elimina la posicion a la q el objeto se movio
updateFreePos :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
updateFreePos x y newx newy dx dy freePos = if elem (newx,newy) freePos 
                                  then (x,y):(updatePos [(newx,newy)] freePos)
                                  else updateFreePos x y (newx + dx) (newy + dy) dx dy freePos


-- devuelve si una cuadricula de 3x3 esta dentro del ambiente
isValidGrid3x3 :: Int -> Int -> Int -> Int -> Bool
isValidGrid3x3 x y n m
            | isValidPos x y n m && isValidPos (x+2) (y+2) n m = True
            | otherwise = False


-- devuelve una cuadricula de 3x3 aleatoria valida
randomGrid3x3 :: Int -> Int -> StdGen -> (Int,Int)
randomGrid3x3 n m gen = let (i,newGen) = randomR (0,8) gen
                            (x,y) = grid3x3 !! i
                        in  if isValidGrid3x3 x y n m
                            then (x,y)
                            else randomGrid3x3 n m newGen

-- devuelve si una posicion esta dentro de la cuadricula indicada o no
insideGrid :: Int -> Int -> Int -> Int -> Bool
insideGrid gx gy x y = if x <= (gx+2) && x >= gx && y <= (gy+2) && y >= gy then True else False

-- dada una lista de posiciones y una cuadricula, filtra la lista devolviendo
-- las posiciones que pertenecen a la cuadricula
filterpos :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
filterpos gx gy [] = []
filterpos gx gy ((x,y):xs) = if insideGrid gx gy x y then (x,y):filterpos gx gy xs else filterpos gx gy xs

-- dada una lista de ninnos y una cuadricula, devuelve la cantidad de ninnos que pertencen a la cuadricula
childrenInGrid ::  Int -> Int -> [Object] -> Int
childrenInGrid gx gy childList = let childPos = getPosObjects childList
                                     posInGrid = filterpos gx gy childPos
                                 in length posInGrid

-- dados el numero de ninnos que hay en una cuadricula devuelve la posible cantidad
-- de casillas sucias que pueden ensuciar de la cuadricula
getDirtyCellsNumber children gen 
        | children == 1 = randomR (0,1) gen
        | children == 2 = randomR (0,3) gen 
        | otherwise = randomR (0,6) gen 

dirtyCells gx gy gridFreePos childList gen = let children = childrenInGrid gx gy childList
                                                (dirty,newGen) = getDirtyCellsNumber children gen
                                                dirtyPos = [gridFreePos !! i | i <- indexes]
        
                                             in (dirtyPos, newGen)
                                            where indexes = rand dirty (0, length gridFreePos) gen
      
                                