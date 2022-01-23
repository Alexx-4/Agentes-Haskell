module Objects
( 
    Object(..),
    Location(..),
    createObject,
    moveObject,
    getPosObjects,
    createPlaypen
) 
where


-- cada objeto esta identificado por su nombre (ya sea robot, ninno, suciedad, etc),
-- ademas de la posicion donde se encuentra en el ambiente
data Object = Object { name :: String, 
                       location :: Location
                     } deriving Show

-- guarda un posicion x,y determinada
data Location = Location Int Int deriving Show

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