module Simulation
(
    createEnvironment
)
where

import System.Random
import Data.List

import Environment
import Functions
import Objects
import UtilsRobots
import Robots




-- crea el estado inicial del ambiente, ubicando en posiciones aleatorias los distintos objetos
createEnvironment :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO()
createEnvironment n m agNumber childNumber dirtyNumber obstNumber t robotType = do

        let freePos = createPos n m

        -- ubicando corral
        gen <- newStdGen
        let playpenPos = createPlaypen childNumber freePos
        let playpen = locateObjects "Playpen" childNumber playpenPos gen

        let aux = updatePos playpenPos freePos
        let freePos = aux 

        -- ubicando agentes
        gen <- newStdGen
        let robotList = locateObjects "Robot free" agNumber freePos gen
        
        let aux = updatePos (getPosObjects robotList) freePos
        let freePos = aux
        
        -- ubicando obstaculos
        gen <- newStdGen
        let obstList = locateObjects "Obstacle" obstNumber freePos gen
        
        let aux = updatePos (getPosObjects obstList) freePos
        let freePos = aux

        -- ubicando ninnos
        gen <- newStdGen
        let childList = locateObjects "Child" childNumber freePos gen
        
        let aux = updatePos (getPosObjects childList) freePos
        let freePos = aux

        -- ubicando suciedad
        gen <- newStdGen
        let dirtyList = locateObjects "Dirty" dirtyNumber freePos gen
        
        let aux = updatePos (getPosObjects dirtyList) freePos
        let freePos = aux

        let actionRobot = getRobotType

        gen <- newStdGen
        mainPrintEnvironment playpen robotList childList dirtyList obstList freePos n m
        simulation childList robotList dirtyList freePos obstList playpen t 1 0 gen n m actionRobot

        where
            getRobotType
                    | robotType == 1 = randomRobot
                    | robotType == 2 = nannyRobot
                    | robotType == 3 = cleanerRobot
                    | robotType == 4 = balancedRobot
                    | robotType == 5 = teamworkRobot


-- ejecuta la simulacion 
simulation :: [Object] -> [Object] -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> Int -> Int -> Int -> StdGen -> Int -> Int ->
    (Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[(Int,Int)],StdGen)) -> IO()
simulation childList robotList dirtyList freePos obstList playpen t iteration iRobot gen n m actionRobot
    | iteration > 10000 = do
                        putStrLn("Success")

    | (dirtPercent dirtyList freePos) < 60 && iteration > 10 =  do 
                                                        putStrLn("Fail")

    | all (\(x,y) -> elem (x,y) (getPosObjects playpen)) (getPosObjects childList) = do
                                                                                putStrLn ("Success")
    
    | iRobot == (length robotList) = 
            if (mod iteration t) == 0
            then let  (newChildList, newDirtyList, newObstList, newFreePos) =
                                changeEnvironment childList dirtyList obstList freePos robotList playpen n m 0 gen

                 in do
                     putStrLn ("\nEnvironment changing\n")
                     mainPrintEnvironment playpen robotList newChildList newDirtyList newObstList newFreePos n m
                     simulation newChildList robotList newDirtyList newFreePos newObstList playpen t (iteration + 1) 0 gen n m actionRobot

            else simulation childList robotList dirtyList freePos obstList playpen t (iteration + 1) 0 gen n m actionRobot       

    | otherwise = 
        let robot = robotList !! iRobot
            (newChildList, newDirtyList, newRobotList, newFreePos, newGen) = 
                            actionRobot robot childList freePos dirtyList playpen robotList gen n m
            newRobot = newRobotList !! iRobot

        in do
            putStrLn("\n\nMove: " ++ (show robot) ++ "\nTo:   " ++ (show newRobot))
            mainPrintEnvironment playpen newRobotList newChildList newDirtyList obstList newFreePos n m
            simulation newChildList newRobotList newDirtyList newFreePos obstList playpen t iteration (iRobot+1) newGen n m actionRobot