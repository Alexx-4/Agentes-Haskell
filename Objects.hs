module Objects
( 
    Object(..),
    Location(..),
    createObject,
    moveObject,
    getPosObjects,
    createPlaypen,
    moveObstacles,
    moveChild
) 
where


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

-- el corral se representa como una lista de casillas,
-- esta funcion crea el corral dado un numero n y una lista de casillas vacias
createPlaypen :: Int -> [(Int,Int)] -> [(Int, Int)]
createPlaypen 0 _ = []
createPlaypen n (x:xs) = x : createPlaypen (n - 1) xs

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
            | elem (x,y) freePos = updateObst x y currentx currenty obstList
            | otherwise = moveObstacles (x+dx) (y+dy) currentx currenty obstList freePos dx dy
    

-- actualiza un obstaculo con la nueva posicion (x,y)
updateObst :: Int -> Int -> Int -> Int -> [Object] -> [Object]
updateObst _ _ _ _ [] = []
updateObst x y currentx currenty (obst@(Object name (Location lx ly)):xs) = 
                                if lx == currentx && ly == currenty
                                then (Object "Obstacle" (Location x y)): updateObst x y currentx currenty xs
                                else obst: updateObst x y currentx currenty xs


