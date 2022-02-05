module UtilsRobots
(
    canRobotMove,
    bfs,
    getPathToMoveRobot,
    getRobotState,
    moveRobot,
    moveRobotChild,
    moveRobotDirty,
    moveRobotPlay,
    moveRobotInPlay,
    moveRobotLeftChild

)
where

import Objects
import Environment
import Functions

import Data.List
import System.Random

-- un robot puede moverse a casillas que esten libres, sucias o a una casilla 
-- del corral si este no tiene un ninno. Puede moverse tambien a una casilla de ninno si lo esta buscando
canRobotMove :: Int -> Int -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> [Object] -> String -> Bool
canRobotMove x y freePos dirtyList playpen childList robotList name_object
  | elem (x,y) (getPosObjects robotList) = False
  | elem (x,y) freePos || elem (x,y) (getPosObjects dirtyList) = True
  | elem (x,y) (getPosObjects playpen) && not ( elem (x,y) (getPosObjects childList) ) = True
  | elem (x,y) (getPosObjects childList) && name_object == "Child" && not ( elem (x,y) (getPosObjects playpen) ) = True
  | otherwise = False


-- devuelve la menor distancia del objeto que se le pasa en queue (primer parametro) al resto de los
-- objetos, devolviendo en visited el arbol del bfs, retornando cuando encuentra el objeto especificado 
-- en el ultimo parametro. Si no lo encuentra retorna objeto null
bfs :: [Object] -> [(Object,Object)] -> Int -> Int -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> [Object] -> String -> (Object,[(Object,Object)])
bfs [] visited _ _ _ _ _ _ _ _= (Object "null" (Location (-1) (-1)),[])
bfs queue@(obj@(Object name (Location x y)):xs) visited n m freePos dirtyList playpen childList robotList name_object
    | name == name_object = (obj, visited)
    | otherwise =
            let adj = [((a i),(b i)) | i <- [0..3], isValidPos (a i) (b i) n m, 
                                                    canRobotMove (a i) (b i) freePos dirtyList playpen childList robotList name_object, 
                                                    not (isVisited (a i) (b i) visited)]

                objsList = posToObject ((getFreePosAsObject freePos) ++ dirtyList ++ playpen ++ childList) adj
                newqueue = xs ++ objsList
                newVisited = visited ++ zip (repeat obj) objsList

            in bfs newqueue newVisited n m freePos dirtyList playpen childList robotList name_object

            where   a i = x + dirx!!i
                    b i = y + diry!!i

                    -- devuelve si un objeto (dada su posicion) fue visitado
                    isVisited :: Int -> Int -> [(Object,Object)] -> Bool
                    isVisited x y [] = False
                    isVisited x y ((_ , (Object _ (Location lx ly))) : xs)
                            | x == lx && y == ly = True
                            | otherwise = isVisited x y xs


-- devuelve el camino mas corto para llegar al objeto especificado
getPathToMoveRobot :: Object -> [(Object,Object)] -> [Object]
getPathToMoveRobot obj visited
        | name obj == "null" = []
        | head(name parent) == 'R' = [obj]
        | otherwise = getPathToMoveRobot parent visited ++ [obj]

        where 
            parent = parentOf obj visited

            parentOf :: Object -> [(Object,Object)] -> Object
            parentOf _ [] = Object "null" (Location (-1) (-1))
            parentOf obj ((parent,child):xs)
                | obj == child = parent
                | otherwise = parentOf obj xs


-- dado un robot devuelve el estado en el q se encuentra
getRobotState :: Object -> String
getRobotState (Object name _) = (words name) !! 1


-- mueve un robot hacia la posicion indicada, actualizando el ambiente
moveRobot :: Object -> Int -> Int -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> ([Object],[(Int,Int)])
moveRobot (Object name (Location x y)) newx newy childList freePos playpen robotList dirtyList =
        let
                newRobotList = updateObj (newx,newy) (x,y) robotList name
                newFreePos = delete (newx,newy) freePos
                newFreePos' =   if elem (x,y) (getPosObjects (childList ++ playpen ++ dirtyList))
                                then newFreePos
                                else (x,y):newFreePos
        in      
                (newRobotList, newFreePos')


-- mueve el robot buscando el ninno mas cercano, si se mueve a un ninno entonces pasa al estado de
-- cargando ninno.
moveRobotChild :: Object -> [Object] -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> ([Object],[Object],[Object],[(Int,Int)], StdGen)
moveRobotChild robot@(Object name (Location x y)) path childList freePos dirtyList playpen robotList gen =
        let 
                newx = row $ location (path !! 0)
                newy = column $ location (path !! 0)
                newRobot =      if elem (newx,newy) (getPosObjects childList)
                                then Object "Robot charging" (Location x y)
                                else robot
                (newRobotList, newFreePos) = moveRobot newRobot newx newy childList freePos playpen robotList dirtyList

        in (childList, dirtyList, newRobotList, newFreePos, gen)


-- si esta encima de una suciedad, la limpia, en otro se mueve en busca de la suciedad mas cercana
moveRobotDirty :: Object -> [Object] -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> ([Object],[Object],[Object],[(Int,Int)],StdGen)
moveRobotDirty robot@(Object name (Location x y)) path childList freePos dirtyList playpen robotList gen
        | elem (x,y) (getPosObjects dirtyList) = 
                let newDirtyList = delete (Object "Dirty" (Location x y)) dirtyList
                in (childList, newDirtyList, robotList, freePos, gen)
        
        | otherwise = let
                        newx = row $ location (path !! 0)
                        newy = column $ location (path !! 0)
                        (newRobotList, newFreePos) = moveRobot robot newx newy childList freePos playpen robotList dirtyList
               
                      in (childList, dirtyList, newRobotList, newFreePos, gen)


-- mueve el robot con el ninno cargado buscando el corral mas cercano para luego valorar el mejor 
-- lugar donde dejarlo. Trata de moverse con dos pasos siempre que pueda
moveRobotPlay :: Object -> [Object] -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> ([Object],[Object],[Object],[(Int,Int)],StdGen)
moveRobotPlay robot@(Object name (Location x y)) path childList freePos dirtyList playpen robotList gen =
        let twoSteps = (length path) >= 2

            newx = if twoSteps then row $ location (path !! 1) else row $ location (path !! 0)
            newy = if twoSteps then column $ location (path !! 1) else column $ location (path !! 0)

            newChildList = let child = getObj (x,y) childList
                           in moveChild child (newx-x) (newy-y) childList

            (newRobotList, newFreePos) = moveRobot robot newx newy newChildList freePos playpen robotList dirtyList

        in (newChildList, dirtyList, newRobotList, newFreePos, gen)
        


-- busca la mejor manera de acomodar los ninnos en el corral, tratando de evitar 'estancamientos' de ninnos.
moveRobotInPlay :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> ([Object],[Object],[Object],[(Int,Int)],StdGen)
moveRobotInPlay robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen =
        let twoSteps = canRobotMove (x-2) y freePos dirtyList playpen childList robotList ""
            newChildList =  let child = getObj (x,y) childList
                            in  if twoSteps
                                then moveChild child (-2) 0 childList
                                else moveChild child (-1) 0 childList
            (newRobotList, newFreePos) = 
                if twoSteps
                then moveRobot robot (x-2) y newChildList freePos playpen robotList dirtyList
                else moveRobot robot (x-1) y newChildList freePos playpen robotList dirtyList

        in (newChildList, dirtyList, newRobotList, newFreePos, gen)


-- simula el momento en el que un robot deja un ninno
moveRobotLeftChild :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> ([Object],[Object],[Object],[(Int,Int)],StdGen)
moveRobotLeftChild robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen =
        let newRobot = Object "Robot free" (Location x y)
            (newRobotList, newFreePos) = moveRobot newRobot x y childList freePos playpen robotList dirtyList

        in (childList, dirtyList, newRobotList, newFreePos, gen)