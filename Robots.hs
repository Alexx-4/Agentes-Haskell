module Robots 
(
        randomRobot,
        nannyRobot
)
where

import Objects
import Environment
import Functions
import UtilsRobots

import Data.List
import System.Random


-- simula el momento en el que un robot deja un ninno
moveRobotLeftChild :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> ([Object],[Object],[Object],[Object],[(Int,Int)],StdGen)
moveRobotLeftChild robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen =
        let newRobot = Object "Robot free" (Location x y)
            (newRobotList, newFreePos) = moveRobot newRobot x y childList freePos playpen robotList dirtyList

        in (childList, playpen, dirtyList, newRobotList, newFreePos, gen)
        


-- la prioridad del robot ninnera es meter a los ninnos en el corral, si no hay ninno alcanzable se pone a limpiar
-- si no puede hacer ninguna de las dos cosas entonces se mueve aleatoriamente por todo el tablero.
nannyRobot :: Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[Object],[(Int,Int)],StdGen)
nannyRobot robot@(Object name (Location x y)) childList freePos dirtyList playpen robotList gen n m
        | state == "free" = 
                let
                (child, childVisited) = bfs [robot] [] n m freePos dirtyList playpen childList "Child"
                (dirty, dirtyVisited) = bfs [robot] [] n m freePos dirtyList playpen childList "Dirty"

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
                
                (play, playVisited) = bfs [robot] [] n m freePos dirtyList playpen childList "Playpen"
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
                                                        canRobotMove (a i) (b i) freePos dirtyList playpen childList ""]
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