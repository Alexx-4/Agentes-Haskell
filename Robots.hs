import Objects
import Environment
import Functions

-- un robot puede moverse a casillas que esten libres, sucias o a una casilla 
-- del corral si este no tiene un ninno
canRobotMove :: Int -> Int -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> String -> Bool
canRobotMove x y freePos dirtyList playpen childList name_object
        | elem (x,y) freePos || elem (x,y) (getPosObjects dirtyList) = True
        | elem (x,y) (getPosObjects playpen) && not ( elem (x,y) (getPosObjects childList) ) = True
        | elem (x,y) (getPosObjects childList) && name_object == "Child" = True
        | otherwise = False


-- devuelve la menor distancia del objeto que se le pasa en queue (primer parametro) al resto de los
-- objetos, devolviendo en visited el arbol del bfs, retornando cuando encuentra el objeto especificado 
-- en el ultimo parametro. Si no lo encuentra retorna objeto null
bfs :: [Object] -> [(Object,Object)] -> Int -> Int -> [(Int,Int)] -> [Object] -> [Object] -> [Object] -> String -> (Object,[(Object,Object)])
bfs [] visited _ _ _ _ _ _ _ = (Object "null" (Location (-1) (-1)),[])
bfs queue@(obj@(Object name (Location x y)):xs) visited n m freePos dirtyList playpen childList name_object
    | name == name_object = (obj, visited)
    | otherwise =
            let adj = [((a i),(b i)) | i <- [0..3], isValidPos (a i) (b i) n m, 
                                                    canRobotMove (a i) (b i) freePos dirtyList playpen childList name_object, 
                                                    not (isVisited (a i) (b i) visited)]

                objsList = posToObject ((getFreePosAsObject freePos) ++ dirtyList ++ playpen ++ childList) adj
                newqueue = xs ++ objsList
                newVisited = visited ++ zip (repeat obj) objsList

            in bfs newqueue newVisited n m freePos dirtyList playpen childList name_object

            where   a i = x + dirx!!i
                    b i = y + diry!!i

                    -- devuelve si un objeto (dada su posicion) fue visitado
                    isVisited :: Int -> Int -> [(Object,Object)] -> Bool
                    isVisited x y [] = False
                    isVisited x y ((_ , (Object _ (Location lx ly))) : xs)
                            | x == lx && y == ly = True
                            | otherwise = isVisited x y xs



-- devuelve la mejor posicion para llegar al objeto que se pasa como parametro
getPosToMoveRobot :: Object -> [(Object,Object)] -> Object
getPosToMoveRobot obj visited
        | head(name parent) == 'R' = obj
        | otherwise = getPosToMoveRobot parent visited name_object

        where 
            parent = parentOf obj visited

            parentOf :: Object -> [(Object,Object)] -> Object
            parentOf _ [] = Object "null" (Location (-1) (-1))
            parentOf obj ((parent,child):xs)
                | obj == child = parent
                | otherwise = parentOf obj xs