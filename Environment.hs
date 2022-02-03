module Environment
(
    createPos,
    locateObjects,
    updatePos,
    changeEnvironment,
) 
where

import Objects
import Functions

import System.Random
import Data.List

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


-- devuelve una direccion aleatoria
randomDir :: StdGen -> (Int,Int,StdGen)
randomDir gen = (dirx !! i, diry !! i, newGen)
                where (i,newGen) = randomR (0,3) gen


-- verifica si un ninno puede moverse en la direccion indicada,
-- en caso que hayan obstaculos en esa direccion, verifica si puede moverlos. Un ninno no puede moverse si esta en 
-- un corral o lo carga un robot.
canChildMove :: Object -> Int -> Int -> Int -> Int -> (Int,Int) -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> Bool
canChildMove child@(Object _ (Location cx cy)) x y n m (dx,dy) freePos obstList robotList playpen
            | elem (cx,cy) (getPosObjects playpen) = False
            | elem (cx,cy) (getPosObjects robotList) && name (getObj (cx,cy) robotList) == "Robot charging" = False 
            | not $ isValidPos x y n m = False
            | elem (x,y) freePos = True
            | elem (x,y) (getPosObjects obstList) = canChildMove child (x+dx) (y+dy) n m (dx,dy) freePos obstList robotList playpen
            | otherwise = False


-- annade la posicion del objeto que se acaba de mover (ahora la casilla esta libre)
-- y elimina la posicion a la q el objeto se movio
updateFreePos :: Int -> Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
updateFreePos x y newx newy dx dy freePos = if elem (newx,newy) freePos 
                                  then (x,y):(updatePos [(newx,newy)] freePos)
                                  else updateFreePos x y (newx + dx) (newy + dy) dx dy freePos


-- devuelve una cuadricula de 3x3 aleatoria valida
randomGrid3x3 :: Int -> Int -> Int -> Int -> StdGen -> (Int,Int)
randomGrid3x3 x y n m gen = let validGrids = filter isValidGrid3x3 [((dx+x),(dy+y)) | (dx,dy) <- grid3x3]
                                (i,_) = randomR (0,((length validGrids)-1)) gen
                            in  validGrids !! i
                            where
                                -- devuelve si una cuadricula de 3x3 esta dentro del ambiente
                                isValidGrid3x3 :: (Int,Int) -> Bool
                                isValidGrid3x3 (x,y)
                                            | isValidPos x y n m && isValidPos (x+2) (y+2) n m = True
                                            | otherwise = False


-- dada una lista de posiciones y una cuadricula, filtra la lista devolviendo
-- las posiciones que pertenecen a la cuadricula
filterpos :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
filterpos gx gy [] = []
filterpos gx gy ((x,y):xs) = if insideGrid gx gy x y then (x,y):filterpos gx gy xs else filterpos gx gy xs
                where
                -- devuelve si una posicion esta dentro de la cuadricula indicada o no
                insideGrid :: Int -> Int -> Int -> Int -> Bool
                insideGrid gx gy x y = if x <= (gx+2) && x >= gx && y <= (gy+2) && y >= gy then True else False


-- dada una lista de objetos y una cuadricula, devuelve la cantidad de objetos que pertencen a la cuadricula
posInGrid ::  Int -> Int -> [Object] -> Int
posInGrid gx gy list =  let pos = getPosObjects list
                            inGrid = filterpos gx gy pos
                        in length inGrid

 
 -- devuelve aleatoriamente la cantidad de casillas a ensuciar en una cuadricula dada 
 -- segun la cantidad de ninnos y de posiciones libres que hay en esta
dirtyCells :: Int -> Int -> [(Int,Int)] -> [Object] -> StdGen -> [(Int,Int)]
dirtyCells gx gy freePos childList gen = let 
                            childrenInGrid = posInGrid gx gy childList
                            freePosInGrid = filterpos gx gy freePos

                            (dirtyNumber,_) = getDirtyCellsNumber childrenInGrid (length freePosInGrid) gen
                            dirtyPos = [freePosInGrid !! i | i <- (indexes dirtyNumber freePosInGrid)]
        
                            in dirtyPos

                where   indexes dirtyNumber freePosInGrid = rand dirtyNumber (0, ((length freePosInGrid)-1)) gen

                    -- dados el numero de ninnos que hay en una cuadricula y la cantidad
                    -- de casillas libres en esta, devuelve la posible cantidad
                    -- de casillas sucias que pueden ensuciar de la cuadricula
                        getDirtyCellsNumber children freePos gen 
                            | children == 1 = randomR (0,(min 1 freePos)) gen
                            | children == 2 = randomR (0,(min 3 freePos)) gen 
                            | otherwise = randomR (0,(min 6 freePos)) gen


-- simula el comportamiento de cambio del ambiente, donde los ninnos se mueven (o no) aleatoriamente,
-- pueden mover obstaculos y ensuciar.
-- recibe los objetos que representan el ambiente y los modifica segun el cambio aleatorio
changeEnvironment :: [Object] -> [Object] -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> Int -> Int -> Int -> StdGen -> ([Object],[Object],[Object],[(Int,Int)])
changeEnvironment childList dirtyList obstList freePos robotList playpen n m i gen
        | i == length childList = (childList, dirtyList, obstList, freePos)
        |otherwise = 
            let 
                child = childList !! i
                (dx, dy, newGen) = randomDir gen
                x = row $ location child
                y = column $ location child

                canMove = canChildMove child (x+dx) (y+dy) n m (dx,dy) freePos obstList robotList playpen

                newChildList = if canMove
                               then moveChild child dx dy childList
                               else childList

                newObstList = if canMove
                              then moveObstacles (x+dx) (y+dy) (x+dx) (y+dy) obstList freePos dx dy
                              else obstList

                newfreePos = if canMove
                             then if elem (x,y) (getPosObjects robotList)
                                  then delete (x,y) (updateFreePos x y (x+dx) (y+dy) dx dy freePos)
                                  else updateFreePos x y (x+dx) (y+dy) dx dy freePos
                             else freePos


                (newDirtyList,newNewFreePos) = 
                    if canMove
                    then let (gx,gy) = randomGrid3x3 x y n m gen
                             posToDirt = dirtyCells gx gy newfreePos childList gen
                             dirtyObjects = [(createObject "Dirty" location) | location <- posToDirt]

                         in ((dirtyList ++ dirtyObjects), (updatePos posToDirt newfreePos))
                             
                    else (dirtyList,newfreePos)
            
            in changeEnvironment newChildList newDirtyList newObstList newNewFreePos robotList playpen n m (i+1) newGen