module Robots 
(
        randomRobot,
        nannyRobot,
        cleanerRobot,
        balancedRobot
)
where

import Objects
import Environment
import Functions
import UtilsRobots

import Data.List
import System.Random
        


-- la prioridad del robot ninnera es meter a los ninnos en el corral, si no hay ninno alcanzable se pone a limpiar
-- si no puede hacer ninguna de las dos cosas entonces se mueve aleatoriamente por todo el tablero.
nannyRobot :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[Object],[(Int,Int)],StdGen)
nannyRobot robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen n m
        | state == "free" = 
                let
                (child, childVisited) = bfs [robot] [] n m freePos dirtyList playpen childList robotList "Child"
                (dirty, dirtyVisited) = bfs [robot] [] n m freePos dirtyList playpen childList robotList "Dirty"

                childPath = getPathToMoveRobot child childVisited
                dirtyPath = getPathToMoveRobot dirty dirtyVisited

                reachableChild = if childPath == [] then False else True
                reachableDirty = if dirtyPath == [] then False else True

                in      if reachableChild 
                        then moveRobotChild robot childPath childList freePos dirtyList playpen robotList gen
                        else    if reachableDirty
                                then moveRobotDirty robot dirtyPath childList freePos dirtyList playpen robotList gen
                                else randomRobot robot childList freePos dirtyList playpen robotList gen n m

        | otherwise = 
                let
                
                (play, playVisited) = bfs [robot] [] n m freePos dirtyList playpen childList robotList "Playpen"
                playPath = getPathToMoveRobot play playVisited

                reachablePlay = if playPath == [] then False else True

                in      if robotInPlaypen
                        then    if playplenInPosUp
                                then moveRobotUp robot childList freePos dirtyList playpen robotList gen
                                else moveRobotLeftChild robot childList freePos dirtyList playpen robotList gen
                        
                        else    if reachablePlay
                                then moveRobotPlay robot playPath childList freePos dirtyList playpen robotList gen
                                else randomRobot robot childList freePos dirtyList playpen robotList gen n m

                

     where      state = getRobotState robot
                robotInPlaypen = elem (x,y) (getPosObjects playpen)
                playplenInPosUp = elem ((x+1),y) (getPosObjects playpen) && not (elem ((x+1),y) (getPosObjects childList)) 


-- el robot random se mueve aleatoriamente en el tablero, si no esta cargando un ninno y
-- encuentra una casilla sucia, la limpia.
randomRobot :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[Object],[(Int,Int)],StdGen)
randomRobot robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen n m
        | elem (x,y) (getPosObjects dirtyList) = 
                let newDirtyList = delete (Object "Dirty" (Location x y)) dirtyList
                in (childList, playpen, newDirtyList, robotList, freePos, gen)

        | otherwise =
                let 
                        adj = [((dirx!!i),(diry!!i)) | i <- [0..3], isValidPos (a i) (b i) n m, 
                                                        canRobotMove (a i) (b i) freePos dirtyList playpen childList robotList ""]
                in      if adj == []
                        then (childList, playpen, dirtyList, robotList, freePos, gen)
                        else let
                                (i,newGen) = randomR (0, ((length adj)-1)) gen
                                (dx,dy) = adj !! i

                                newx = x + dx
                                newy = y + dy

                                newChildList =  if (getRobotState robot) == "charging"
                                                then let child = getObj (x,y) childList
                                                     in moveChild child dx dy childList
                                                else childList

                                (newRobotList, newFreePos) = moveRobot robot newx newy newChildList freePos playpen robotList dirtyList
                                
                        in (newChildList, playpen, dirtyList, newRobotList, newFreePos, newGen) 

                where   a i = x + dirx!!i
                        b i = y + diry!!i


-- la prioridad del robot limpiador es mantener el corral limpio, si no tiene suciedad alcanzable para limpiar, recoge 
-- ninnos para ponerlos en el corral. Si en medio de este proceso aparece una suciedad deja al ninno y va a limpiarla.
cleanerRobot :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[Object],[(Int,Int)],StdGen)
cleanerRobot robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen n m
        | state == "free" = 
                if reachableDirty 
                then moveRobotDirty robot dirtyPath childList freePos dirtyList playpen robotList gen
                else    if reachableChild
                        then    if elem (x,y) (getPosObjects dirtyList)
                                then    let newDirtyList = delete (getObj (x,y) dirtyList) dirtyList
                                        in (childList, playpen, newDirtyList, robotList, freePos, gen)
                                else  moveRobotChild robot childPath childList freePos dirtyList playpen robotList gen
        
                        else randomRobot robot childList freePos dirtyList playpen robotList gen n m
        
        | otherwise = 
                if reachableDirty
                then moveRobotLeftChild robot childList freePos dirtyList playpen robotList gen
                else    if robotInPlaypen
                        then    if playplenInPosUp
                                then moveRobotUp robot childList freePos dirtyList playpen robotList gen
                                else moveRobotLeftChild robot childList freePos dirtyList playpen robotList gen

                        else    if reachablePlay
                                then moveRobotPlay robot playPath childList freePos dirtyList playpen robotList gen
                                else randomRobot robot childList freePos dirtyList playpen robotList gen n m

        where
                state = getRobotState robot
                robotInPlaypen = elem (x,y) (getPosObjects playpen)
                playplenInPosUp = elem ((x+1),y) (getPosObjects playpen) && not (elem ((x+1),y) (getPosObjects childList))

                (child, childVisited) = bfs [robot] [] n m freePos dirtyList playpen childList robotList "Child"
                (dirty, dirtyVisited) = bfs [robot] [] n m freePos dirtyList playpen childList robotList "Dirty"

                childPath = getPathToMoveRobot child childVisited
                dirtyPath = getPathToMoveRobot dirty dirtyVisited

                reachableChild = if childPath == [] then False else True
                reachableDirty = if dirtyPath == [] then False else True

                (play, playVisited) = bfs [robot] [] n m freePos dirtyList playpen childList robotList "Playpen"
                playPath = getPathToMoveRobot play playVisited

                reachablePlay = if playPath == [] then False else True


-- el robot equilibrado realiza sus acciones dependiendo del estado del ambiente.
-- si esta muy sucio se pone a limpiar para mantener el ambiente por debajo del 60%,
-- en caso contrario se pone a buscar ninnos para meterlos en el corral.
balancedRobot :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[Object],[(Int,Int)],StdGen)
balancedRobot robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen n m
        | (dirtPercent dirtyList n m) >= 50 = 
                cleanerRobot robot childList freePos dirtyList playpen robotList gen n m
        
        | otherwise = 
                nannyRobot robot childList freePos dirtyList playpen robotList gen n m