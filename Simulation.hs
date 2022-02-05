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
createEnvironment :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> String -> IO()
createEnvironment n m agNumber childNumber dirtyNumber obstNumber t robotType sim totalSim results allTurns debugMode
    | sim == totalSim = let averageResults = div ((sum results)*100) (length results)
                            averageTurn = div (sum allTurns) (length allTurns)
                            averageIter = div ((sum allTurns)*agNumber) (length allTurns)

                        in  do
                            putStrLn ("Victory Percent           = " ++ (show averageResults) ++ "%")
                            putStrLn ("Turns average             = " ++ (show averageTurn))
                            putStrLn ("Robots Iterations average = " ++ (show averageIter) ++ "\n")

    | otherwise = do
                let freePos = createPos n m

                -- ubicando corral
                gen <- newStdGen
                gen' <- newStdGen
                let playpenPos = createPlaypen childNumber n m gen gen'
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
                simulation childList robotList dirtyList freePos obstList playpen t 1 0 gen n m actionRobot robotType dirtyNumber sim totalSim results allTurns debugMode

                where
                    getRobotType
                            | robotType == 1 = randomRobot
                            | robotType == 2 = nannyRobot
                            | robotType == 3 = cleanerRobot
                            | robotType == 4 = balancedRobot
                            | robotType == 5 = teamworkRobot


-- ejecuta la simulacion 
simulation :: [Object] -> [Object] -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> Int -> Int -> Int -> StdGen -> Int -> Int ->
    (Object -> [Object] -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> StdGen -> Int -> Int -> ([Object],[Object],[Object],[(Int,Int)],StdGen)) -> Int -> Int -> Int -> Int -> [Int] -> [Int] -> String -> IO()
simulation childList robotList dirtyList freePos obstList playpen t turn iRobot gen n m actionRobot robotType dirtyNumber sim totalSim results allTurns debugMode
    -- derrota
    | (cleanPercent dirtyList freePos) < 60 && turn > 5 =  do 
                putStrLn("Fail")
                putStrLn("Robots couldnt keep environment clean")
                putStrLn("Turns: " ++ (show turn))
                putStrLn("Robots iterations: " ++ (show (turn * (length robotList))))
                printAt
                createEnvironment n m (length robotList) (length childList) dirtyNumber (length obstList) t robotType (sim+1) totalSim (results ++ [0]) (allTurns ++ [turn]) debugMode
    
    -- victoria
    | turn > 150 = do
                putStrLn("Success")
                putStrLn("Number of turns exceeded")
                printAt
                createEnvironment n m (length robotList) (length childList) dirtyNumber (length obstList) t robotType (sim+1) totalSim (results ++ [1]) (allTurns ++ [turn]) debugMode
    
    -- victoria
    | ( let inPlaypen = [child | child <- (getPosObjects childList), elem child (getPosObjects playpen)]
            inRobot = [child | child <- (getPosObjects childList), elem child (getPosObjects robotList)]
            inBoth = [child | child <- (getPosObjects childList), elem child (getPosObjects playpen),
                                                                     elem child (getPosObjects robotList)]

        in ( (length inPlaypen) + (length inRobot) - (length inBoth) ) == (length childList)) = do
                putStrLn ("Success")
                putStrLn ("All childs in playplen or in robots")
                putStrLn("Turns: " ++ (show turn))
                putStrLn("Robots iterations: " ++ (show (turn * (length robotList))))
                printAt
                createEnvironment n m (length robotList) (length childList) dirtyNumber (length obstList) t robotType (sim+1) totalSim (results ++ [1]) (allTurns ++ [turn]) debugMode

    -- cambio de turno (puede haber cambio de ambiente)
    | iRobot == (length robotList) = 
            if (mod turn t) == 0
            then let  (newChildList, newDirtyList, newObstList, newFreePos) =
                                changeEnvironment childList dirtyList obstList freePos robotList playpen n m 0 gen

                 in do
                     printNumber
                     putStrLn ("\nEnvironment changing\n")
                     mainPrintEnvironment playpen robotList newChildList newDirtyList newObstList newFreePos n m
                     printNumber
                     simulation newChildList robotList newDirtyList newFreePos newObstList playpen t (turn + 1) 0 gen n m actionRobot robotType dirtyNumber sim totalSim results allTurns debugMode

            else simulation childList robotList dirtyList freePos obstList playpen t (turn + 1) 0 gen n m actionRobot robotType dirtyNumber sim totalSim results allTurns debugMode

    -- ejecucion de una accion de agente
    | otherwise = 
        let robot = robotList !! iRobot
            (newChildList, newDirtyList, newRobotList, newFreePos, newGen) = 
                            actionRobot robot childList freePos dirtyList playpen robotList gen n m
            newRobot = newRobotList !! iRobot

        in do
            printMinus
            putStrLn("\n\nMove: " ++ (show robot) ++ "\nTo:   " ++ (show newRobot))
            mainPrintEnvironment playpen newRobotList newChildList newDirtyList obstList newFreePos n m
            printMinus
            callSim newChildList newRobotList newDirtyList newFreePos newGen
            


    where
        printAt =     putStrLn("\n@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
        printNumber = putStrLn("##################################################################################################")
        printMinus =  putStrLn("--------------------------------------------------------------------------------------------------")

        callSim newChildList newRobotList newDirtyList newFreePos newGen = 
            if debugMode == "y"
            then do
                putStrLn ("Press Enter to continue")
                a <- getLine
                simulation newChildList newRobotList newDirtyList newFreePos obstList playpen t turn (iRobot+1) newGen n m actionRobot robotType dirtyNumber sim totalSim results allTurns debugMode
            else 
                simulation newChildList newRobotList newDirtyList newFreePos obstList playpen t turn (iRobot+1) newGen n m actionRobot robotType dirtyNumber sim totalSim results allTurns debugMode