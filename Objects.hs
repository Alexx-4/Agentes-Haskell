module Objects
( 
    Object(..),
    Location(..),
    createObject,
    moveObject,
    getPosObjects,
    createPlaypen,
    moveObstacles,
    moveChild,
    posToObject,
    updateObj,
    getObj,
    dirx,
    diry,
    isValidPos,
    rand

) 
where

import System.Random
import Data.List


-- direcciones posibles de casillas adyacentes
dirx :: [Int]
dirx = [1, 0, -1,  0]

diry :: [Int]
diry = [0, 1,  0, -1]


-- dada una posicion x,y del ambiente devuelve si esta dentro de los limites de este
isValidPos :: Int -> Int -> Int -> Int -> Bool
isValidPos x y n m = if x >= 0 && x < n && y >= 0 && y < m then True else False


-- devuelve una lista de n numeros aleatorios distintos en el
-- intervalo [min,max]
rand :: Int -> (Int, Int) -> StdGen -> [Int]
rand n (min, max) gen = if n > (max - min + 1) 
                        then error "full environment"
                        else take n $ nub $ randomRs (min, max) gen :: [Int]


-- cada objeto esta identificado por su nombre (ya sea robot, ninno, suciedad, etc),
-- ademas de la posicion donde se encuentra en el ambiente
data Object = Object { name :: String, 
                       location :: Location
                     } deriving (Show,Eq)


-- guarda un posicion x,y determinada
data Location = Location {row::Int, column::Int} deriving (Show,Eq)


instance Ord Object where
    (<) (Object _ (Location x1 y1)) (Object _ (Location x2 y2))
        |x1 < x2 = True
        |x1 == x2 = y1 < y2
        |otherwise = False

    (<=)(Object _ (Location x1 y1)) (Object _ (Location x2 y2))
        |x1 < x2 = True
        |x1 == x2 = y1 <= y2
        |otherwise = False

    (>) o1 o2 = not (o1 <= o2) 
    (>=) o1 o2 = not (o1 < o2)


-- crea un objeto dados su nombre y ubicacion
createObject :: String -> (Int,Int) -> Object
createObject name (x,y) = Object name $ Location x y


-- recibe un objeto y una direccion d = d1,d2
-- devuelve el objeto desplazado en la direccion d
moveObject :: Object -> Int -> Int -> Object
moveObject (Object name (Location x y)) d1 d2 = Object name $ Location (x + d1) (y + d2)


-- recibe una lista de objetos y deluelve otra lista con todas las posiciones que
-- estos ocupan
getPosObjects :: [Object] -> [(Int,Int)]
getPosObjects [] = []
getPosObjects ((Object name (Location x y)):xs) = (x,y):getPosObjects xs


-- devuelve una lista de objetos basado en una lista de posiciones
posToObject :: [Object] -> [(Int,Int)] -> [Object]
posToObject _ [] = []
posToObject objList ((x,y):ys) = getObject objList (x,y) ++ posToObject objList ys
                            where
                                getObject :: [Object] -> (Int,Int) -> [Object]
                                getObject [] _ = [] 
                                getObject (obj@(Object _ (Location ox oy)):xs) (x,y) =  if x == ox && y == oy 
                                                                                        then [obj] 
                                                                                        else getObject xs (x,y)

-- el corral se representa como una lista de casillas, esta funcion crea el corral en posiciones adyacentes 
-- aleatorias del ambiente, dado sus dimensiones y el numero de ninnos
createPlaypen :: Int -> Int -> Int -> StdGen -> StdGen -> [(Int,Int)]
createPlaypen child n m g1 g2 = 
                    let 
                        (x,_) = randomR (0, ((n-1)-2)) g1
                        (y,_) = randomR (0,  (m-1))    g2

                    in dfs [(x,y)] [] 0 child n m g1
                    where
                        -- devuelve una lista de casillas adyacentes seleccionadas de forma aleatoria
                        dfs :: [(Int,Int)] -> [(Int,Int)] -> Int -> Int -> Int -> Int -> StdGen -> [(Int,Int)]
                        dfs [] _ _ _ _ _ _ = []
                        dfs stack@((x,y):xs) visited c child n m gen
                            | c == child = visited
                            | otherwise =
                                let newVisited = visited ++ [(x,y)]
                                    adj = [((a i),(b i)) | i <- (rand 4 (0,3) gen), 
                                                                    isValidPos (a i) (b i) (n-2) m,
                                                                    not (elem ((a i),(b i)) visited)]
                                    (_,newGen) = randomR (0,3) gen :: (Int,StdGen)
                        
                                in  dfs (adj ++ xs) newVisited (c+1) child n m newGen
                                    
                        
                                where   a i = x + dirx!!i
                                        b i = y + diry!!i



-- dado un objeto que representa un ninno, una direccion, modifica la lista 
-- de ninnos, cambiando la posicion del objeto en la direccion indicada
moveChild :: Object -> Int -> Int -> [Object] -> [Object]
moveChild child dx dy [] = []
moveChild child@(Object _ (Location x y)) dx dy (currentChild@(Object _ (Location currentx currenty)):xs) =
                                if currentx == x && currenty == y
                                then (moveObject child dx dy): moveChild child dx dy xs
                                else currentChild: moveChild child dx dy xs


-- mueve los obstaculos consecutivos a partir de la posicion indicada en la direccion que
-- recibe como parametro.
moveObstacles :: Int -> Int -> Int -> Int -> [Object] -> [(Int,Int)] -> Int -> Int -> [Object]
moveObstacles x y currentx currenty obstList freePos dx dy
            | elem (x,y) freePos = updateObj (x,y) (currentx,currenty) obstList "Obstacle"
            | otherwise = moveObstacles (x+dx) (y+dy) currentx currenty obstList freePos dx dy
    

-- actualiza un objeto con la nueva posicion (x,y)
-- recibe como parametros la nueva posicion, la antigua posicion y la lista de objetos a actualizar
updateObj :: (Int,Int) -> (Int,Int) -> [Object] -> String -> [Object]
updateObj _ _ [] _ = []
updateObj (x,y) (currentx,currenty) (obj@(Object _ (Location lx ly)):xs) name = 
                                if lx == currentx && ly == currenty
                                then (Object name (Location x y)): updateObj (x,y) (currentx,currenty) xs name
                                else obj: updateObj (x,y) (currentx,currenty) xs name


-- devuelve el primer objeto que coincide con la posicion indicada
getObj :: (Int,Int) -> [Object] -> Object
getObj _ [] = Object "null" (Location (-1) (-1))
getObj (x,y) (obj@(Object _ (Location ox oy)):xs) =
    if ox == x && oy == y then obj else getObj (x,y) xs