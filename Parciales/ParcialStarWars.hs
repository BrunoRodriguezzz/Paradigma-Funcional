data Nave = UnaNave{
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: [Nave -> Nave]
}

ejTIE :: Nave
ejTIE = UnaNave "TIE Fighter" 200 100 50 [turbo]
ejXWING :: Nave
ejXWING = UnaNave "X WING" 300 150 100 [reparacionEmergencia]

-- Funciones de las naves

turbo :: Nave -> Nave
turbo nave = nave {ataque = ataque nave + 25}

reparacionEmergencia :: Nave -> Nave 
reparacionEmergencia nave = nave{durabilidad = durabilidad nave + 50, ataque = ataque nave - 30}

superTurbo :: Nave -> Nave
superTurbo nave = foldl aplicarFunciones nave {durabilidad = durabilidad nave - 45} [turbo, turbo, turbo]

-- Funcion adicional

aplicarFunciones :: a -> (a -> a) -> a
aplicarFunciones a f = f a

incrementarEscudo :: Nave -> Nave
incrementarEscudo nave = nave{escudo = escudo nave + 100}

-- Nuevo poder del punto 1

choque :: Nave -> Nave
choque nave = incrementarEscudo nave{ataque = ataque nave +120, durabilidad = durabilidad nave - 200}

-- Punto 2

durabilidadTotal :: [Nave] -> Int
durabilidadTotal = sum . map durabilidad

-- Punto 3

postAtaque :: Nave -> Nave -> Nave
postAtaque naveAtacada naveAtacante = aplicarTodosLosPoderes naveAtacada

-- no llegue a terminar

-- Funcion adicional
aplicarTodosLosPoderes :: Nave -> Nave
aplicarTodosLosPoderes nave = foldl aplicarFunciones nave (poder nave)

checkNegativo :: Int -> Int
checkNegativo = max 0
