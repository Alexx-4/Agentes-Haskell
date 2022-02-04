import Data.Char
import System.Random
import Control.Exception

import Environment
import Functions
import Objects
import Robots
import Simulation


-- Presentacion
main = do
        putStrLn ("\n\nPROYECTO DE SIMULACION Y PROGRAMACION DECLARATIVA \n\n\t\tAlejandro Campos\n\t\t     C-411\n\n\t      Problema: Agentes\n\n    Facultad de Matemática y Computación\n\t Universidad de La Habana\n\t\t   2022\n\n")
        catchExceptions


-- llama a la funcion que recibe la entrada de usuario
-- captando las excepciones mediante la funcion handler
catchExceptions :: IO ()
catchExceptions = catch getInput handler

-- capta las excepciones de los parametros de entrada y vuelve a iniciar la simulacion
handler :: SomeException -> IO ()
handler e = do 
        putStrLn "Whoops, had some trouble on input parameters! Try again."
        getInput


-- recibe como entrada del usuario las dimensiones nxm del ambiente y la cantidad de objetos de
-- cada tipo que contendra. Recibe tambien cada que tiempo t cambia el ambiente, ademas del tipo de
-- agente con que se realizaran las n simulaciones, que tambien son entrada del usuario.
-- Si los parametros de entrada no son consistentes se genera una excepcion.
getInput :: IO ()
getInput =  do
        putStrLn ("Enter environment's rows and columns: <rows> <columns> \n >>")
        dimentions <- getLine
        let dimentionsList = words dimentions
        let n = read (dimentionsList !! 0) :: Int
        let m = read (dimentionsList !! 1) :: Int

        let totalPos = n*m

        putStrLn ("\nEnter number of robots \n >>")
        ag <- getLine
        let agNumber = read ag :: Int

        putStrLn ("\nEnter number of children \n >>")
        children <- getLine
        let childNumber = read children :: Int

        putStrLn ("\nEnter number of dirty cells \n >>")
        dirty <- getLine
        let dirtyNumber = read dirty :: Int

        putStrLn ("\nEnter number of obstacles \n >>")
        obst <- getLine
        let obstNumber = read obst :: Int

        putStrLn ("\nEnter environment change time \n >>")
        time <- getLine
        let t = read time :: Int

        putStrLn ("\nChoose a robot type. Enter corresponding number: \n  1- random Robot \n  2- nanny Robot \n  3- cleaner Robot \n  4- balanced Robot \n  5- teamwork Robot \n >>")
        type' <- getLine
        let robotType = read type' :: Int

        putStrLn ("\nFinally, enter number of simulations \n >>")
        sim <- getLine
        let simulations = read sim :: Int

        putStrLn ("\nDebug mode: y/n \n >> ")
        debugMode <- getLine

        createEnvironment n m agNumber childNumber dirtyNumber obstNumber t robotType 0 simulations [] [] debugMode