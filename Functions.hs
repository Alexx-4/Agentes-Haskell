module Functions
(
    mainPrintEnvironment,
    getFreePosAsObject,
    cleanPercent,
    indexOf
) 
where
    
import Objects

import System.Random
import Data.List


-- funcion que calcula el porcentaje de casillas que no estan sucias del ambiente
cleanPercent :: [Object] -> [(Int,Int)] -> Int
cleanPercent dirtyList freePos = div (free * 100) (dirty+free) 
                                where dirty = length dirtyList
                                      free = length freePos

-- dados un elemento y una lista devuelve su indice en la lista, en caso que no aparezca devuelve -1
indexOf :: (Eq a) => a -> [a] -> Int
indexOf obj list = indexOf' obj list 0
        where
            indexOf' _ [] _ = -1
            indexOf' obj (x:xs) i = if obj == x then i else indexOf' obj xs (i+1)


--                               >>Para pintar el ambiente<<
-- ###############################################################################################
--recibe listas y devuelve la concatenacion de todas
joinList :: [Object] -> [Object] -> [Object] -> [Object] -> [Object] -> [Object] -> [Object]
joinList a b c d e f = sort (a ++ b ++ c ++ d ++ e ++ f)

-- devuelve un ambiente vacio de dimensiones nxm
getEmptyEnvironment :: Int -> Int -> [[String]]
getEmptyEnvironment 0 m = []
getEmptyEnvironment n m = (take m $ repeat ""): getEmptyEnvironment (n-1) m

-- ubica los elementos de la lista de Objetos en el ambiente a partir de sus posiciones
getEnvironment :: [Object] -> [[String]] -> [[String]]
getEnvironment [] env = env
getEnvironment (obj@(Object name (Location x y)):ls) env = getEnvironment ls (insertInEnv name x y 0 env)

-- recibe como parametros un nombre de objeto, una posicion donde ubicarlo y un ambiente. Devuelve 
-- el objeto insertado en la posicion indicada. 
insertInEnv :: String -> Int -> Int -> Int -> [[String]] -> [[String]]
insertInEnv _ _ _ _ [] = []
insertInEnv name x y i env@(l:ls)
    | x == i = (getNewList name y 0 l): insertInEnv name x y (i+1) ls
    | otherwise = l: insertInEnv name x y (i+1) ls
    where
        getNewList _ _ _ [] = []
        getNewList name y j list@(x:xs)
                | j == y = (addSpaces $ add (name !! 0) (removeSpaces x)) : getNewList name y (j+1) xs
                | otherwise = (addSpaces (removeSpaces x)) : getNewList name y (j+1) xs
                where 
                    removeSpaces s = filter (\x -> x /= ' ') s 
                    addSpaces s 
                        | length s >= 3 = s
                        | length s == 2 = s ++ " "
                        | length s == 1 = " " ++ s ++ " "
                        | otherwise = "   "
                    add c s = s ++ [c]

-- pinta el ambiente en consola
printEnvironment :: [[String]] -> IO()
printEnvironment [] = return ()
printEnvironment (l:ls) = do
                        putStrLn (concat (intersperse "|" l))
                        putStrLn (concat $ take (length l) (repeat "---+"))
                        printEnvironment ls

-- devuelve una lista de posiciones libres como instacias de tipo Objeto
getFreePosAsObject :: [(Int,Int)] -> [Object]
getFreePosAsObject [] = []
getFreePosAsObject ((x,y):xs) = (Object " " (Location x y)) :  getFreePosAsObject xs


-- metodo principal para pintar el tablero
mainPrintEnvironment :: [Object] -> [Object] -> [Object] -> [Object] -> [Object] -> [(Int,Int)] -> Int -> Int -> IO ()
mainPrintEnvironment playpen agentList childList dirtyList obstList freePos n m = do
    let freePosObj = getFreePosAsObject freePos
    let empty = getEmptyEnvironment n m
    let orderList = joinList playpen agentList childList dirtyList obstList freePosObj
    let env = getEnvironment orderList empty
    let indexedEnv = envWithIndex env 0
    let columns = map fixIndex (map show [0..(m-1)])
    putStrLn("\n")
    printEnvironment (("   ":columns):indexedEnv)
    putStrLn("\n")

-- alinea los indices en el ambiente
fixIndex s = if length s == 1 then " " ++ s ++ " " else if length s == 2 then s ++ " " else s

-- annade indices al ambiente
envWithIndex [] _ = []
envWithIndex (l:ls) i = ([fixIndex (show i)] ++ l): envWithIndex ls (i+1)

-- #####################################################################################################