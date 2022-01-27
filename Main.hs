import Data.Char
import System.Random
import Control.Exception

import Environment
import Functions
import Objects


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
        putStrLn ("Ingrese dimensiones separadas por espacio de la forma: filas columnas \n >>")
        dimentions <- getLine
        let dimentionsList = words dimentions
        let n = read (dimentionsList !! 0) :: Int
        let m = read (dimentionsList !! 1) :: Int

        let totalPos = n*m

        putStrLn ("Ingrese número de robots \n >>")
        agNumber <- getLine
        let ag = read agNumber :: Int

        putStrLn ("Ingrese número de niños \n >>")
        childNumber <- getLine
        let child = read childNumber :: Int

        putStrLn ("Ingrese número de casillas sucias \n >>")
        dirtyNumber <- getLine
        let dirty = read dirtyNumber :: Int

        putStrLn ("Ingrese número de obstáculos \n >>")
        obstNumber <- getLine
        let obst = read obstNumber :: Int

        putStrLn ("Ingrese cada qué tiempo cambia el ambiente \n >>")
        time <- getLine
        let t = read time :: Int


        let freePos = createPos n m

        -- ubicando corral
        gen <- newStdGen
        let playpenPos = createPlaypen child freePos
        let playpen = locateObjects "Playpen" child playpenPos gen

        let aux = updatePos playpenPos freePos
        let freePos = aux 

        -- ubicando agentes
        gen <- newStdGen
        let agentList = locateObjects "Robot" ag freePos gen
        
        let aux = updatePos (getPosObjects agentList) freePos
        let freePos = aux
        
        -- ubicando obstaculos
        gen <- newStdGen
        let obstList = locateObjects "Obstacle" obst freePos gen
        
        let aux = updatePos (getPosObjects obstList) freePos
        let freePos = aux

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


        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m
        
        

        gen <- newStdGen
        let (newChildList, newDirtyList, newObstList, newFreePos) =
                changeEnvironment childList dirtyList obstList freePos n m 0 gen
        
        let childList = newChildList
        let dirtyList = newDirtyList
        let obstList = newObstList
        let freePos = newFreePos

        mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m

        



       




