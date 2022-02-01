import Data.Char
import System.Random
import Control.Exception

import Environment
import Functions
import Objects
import Robots


{-- Presentacion
main = do
        putStrLn ("Presentación here")
        beginSimulation

-- llama a la funcion que crea el ambiente y comienza la simulacion
-- captando las excepciones de la entrada de usuario mediante la funcion handler
beginSimulation :: IO ()
beginSimulation = catch createEnvironment handler

-- capta las excepciones de los parametros de entrada y vuelve a iniciar la simulacion
handler :: SomeException -> IO ()
handler e = do 
        putStrLn "Whoops, had some trouble on input parameters! Try again."
        -- !!!!!!!!!! UNCOMMENT THIS !!!!!!!!!!!!!!!!!!!!!
        --beginSimulation


-- crea el estado inicial del ambiente, ubicando en posiciones aleatorias los distintos objetos,
-- recibe como entrada del usuario las dimesiones nxm del ambiente y la cantidad de objetos de
-- cada tipo que contendra. Recibe tambien cada que tiempo t cambia el ambiente.
-- Si los parametros de entrada no son consistentes se genera una excepcion.
createEnvironment :: IO ()
createEnvironment = -}

main =  do
        -- putStrLn ("Ingrese dimensiones separadas por espacio de la forma: filas columnas \n >>")
        -- dimentions <- getLine
        -- let dimentionsList = words dimentions
        -- let n = read (dimentionsList !! 0) :: Int
        -- let m = read (dimentionsList !! 1) :: Int
        let n = 6
        let m = 6

        let totalPos = n*m

        -- putStrLn ("Ingrese número de robots \n >>")
        -- agNumber <- getLine
        -- let ag = read agNumber :: Int
        let ag = 1

        -- putStrLn ("Ingrese número de niños \n >>")
        -- childNumber <- getLine
        -- let child = read childNumber :: Int
        let child = 3

        -- putStrLn ("Ingrese número de casillas sucias \n >>")
        -- dirtyNumber <- getLine
        -- let dirty = read dirtyNumber :: Int
        let dirty = 4

        -- putStrLn ("Ingrese número de obstáculos \n >>")
        -- obstNumber <- getLine
        -- let obst = read obstNumber :: Int
        let obst = 7

        -- putStrLn ("Ingrese cada qué tiempo cambia el ambiente \n >>")
        -- time <- getLine
        -- let t = read time :: Int
        let time = 0


        let freePos = createPos n m

        -- ubicando corral
        gen <- newStdGen
        let playpenPos = createPlaypen child freePos
        let playpen = locateObjects "Playpen" child playpenPos gen

        let aux = updatePos playpenPos freePos
        let freePos = aux 

        -- ubicando agentes
        gen <- newStdGen
        let agentList = locateObjects "Robot free" ag freePos gen
        
        let aux = updatePos (getPosObjects agentList) freePos
        let freePos = aux
        
        -- ubicando obstaculos
        gen <- newStdGen
        let obstList = locateObjects "Obstacle" obst freePos gen
        
        let aux = updatePos (getPosObjects obstList) freePos
        let freePos = aux )

        -- ubicando ninnos
        gen <- newStdGen
        let childList = locateObjects "Child" child freePos gen
        
        let aux = updatePos (getPosObjects childList) freePos
        let freePos = aux

        -- ubicando suciedad
        gen <- newStdGen
        let dirtyList = locateObjects "Dirty" dirty freePos gen
        
        let aux = updatePos (getPosObjects dirtyList) freePos
        let freePos = aux



        let freePos = [(0,4),(1,3),(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(3,2),(3,4),(3,5),(4,3),(4,5),(5,1),(5,2),(5,3),(5,4),(1,2),(1,0)]
        let dirtyList = [Object {name = "Dirty", location = Location {row = 1, column = 1}},Object {name = "Dirty", location = Location {row = 3, column = 0}},Object {name = "Dirty", location = Location {row = 4, column = 0}}]
        let childList = [Object {name = "Child", location = Location {row = 5, column = 0}}]
        let playpen = [Object {name = "Playpen", location = Location {row = 0, column = 0}},Object {name = "Playpen", location = Location {row = 0, column = 1}},Object {name = "Playpen", location = Location {row = 0, column = 2}}]
        let obstList = [Object {name = "Obstacle", location = Location {row = 4, column = 4}},Object {name = "Obstacle", location = Location {row = 3, column = 3}},Object {name ="Obstacle", location = Location {row = 4, column = 1}},Object {name = "Obstacle", location = Location {row = 0, column = 5}},Object {name = "Obstacle", location = Location {row = 3, column = 1}},Object {name = "Obstacle", location = Location {row = 4, column = 2}},Object {name = "Obstacle", location = Location{row = 1, column = 5}},Object {name = "Obstacle", location = Location {row = 5, column = 5}}]
        let agentList = [Object {name = "Robot free", location = Location {row = 1, column = 4}},Object {name = "Robot free", location = Location {row = 0, column = 3}}]



        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m
        

        let (newChildList, newPlaypen, newDirty, newRobot, newFreePos, newGen) =
                nannyRobot (agentList !! 0) childList freePos dirtyList playpen agentList (mkStdGen 5) n m
        
        let childList = newChildList 
        let playpen = newPlaypen
        let dirtyList = newDirty
        let agentList = newRobot
        let freePos = newFreePos


        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        


        gen <- newStdGen
        let (newChildList, newDirtyList, newObstList, newFreePos) =
                changeEnvironment childList dirtyList obstList freePos agentList playpen n m 0 gen
        
        let childList = newChildList
        let dirtyList = newDirtyList
        let obstList = newObstList
        let freePos = newFreePos

        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m
        

        let (newChildList, newPlaypen, newDirty, newRobot, newFreePos, newGen) =
                nannyRobot (agentList !! 0) childList freePos dirtyList playpen agentList (mkStdGen 5) n m
        
        let childList = newChildList 
        let playpen = newPlaypen
        let dirtyList = newDirty
        let agentList = newRobot
        let freePos = newFreePos


        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        


        gen <- newStdGen
        let (newChildList, newDirtyList, newObstList, newFreePos) =
                changeEnvironment childList dirtyList obstList freePos agentList playpen n m 0 gen
        
        let childList = newChildList
        let dirtyList = newDirtyList
        let obstList = newObstList
        let freePos = newFreePos

        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        let (newChildList, newPlaypen, newDirty, newRobot, newFreePos, newGen) =
                nannyRobot (agentList !! 0) childList freePos dirtyList playpen agentList (mkStdGen 5) n m
        
        let childList = newChildList 
        let playpen = newPlaypen
        let dirtyList = newDirty
        let agentList = newRobot
        let freePos = newFreePos


        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        


        gen <- newStdGen
        let (newChildList, newDirtyList, newObstList, newFreePos) =
                changeEnvironment childList dirtyList obstList freePos agentList playpen n m 0 gen
        
        let childList = newChildList
        let dirtyList = newDirtyList
        let obstList = newObstList
        let freePos = newFreePos

        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        let (newChildList, newPlaypen, newDirty, newRobot, newFreePos, newGen) =
                nannyRobot (agentList !! 0) childList freePos dirtyList playpen agentList (mkStdGen 5) n m
        
        let childList = newChildList 
        let playpen = newPlaypen
        let dirtyList = newDirty
        let agentList = newRobot
        let freePos = newFreePos


        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        


        gen <- newStdGen
        let (newChildList, newDirtyList, newObstList, newFreePos) =
                changeEnvironment childList dirtyList obstList freePos agentList playpen n m 0 gen
        
        let childList = newChildList
        let dirtyList = newDirtyList
        let obstList = newObstList
        let freePos = newFreePos

        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        let (newChildList, newPlaypen, newDirty, newRobot, newFreePos, newGen) =
                nannyRobot (agentList !! 0) childList freePos dirtyList playpen agentList (mkStdGen 5) n m
        
        let childList = newChildList 
        let playpen = newPlaypen
        let dirtyList = newDirty
        let agentList = newRobot
        let freePos = newFreePos


        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        


        gen <- newStdGen
        let (newChildList, newDirtyList, newObstList, newFreePos) =
                changeEnvironment childList dirtyList obstList freePos agentList playpen n m 0 gen
        
        let childList = newChildList
        let dirtyList = newDirtyList
        let obstList = newObstList
        let freePos = newFreePos

        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m



        



       




