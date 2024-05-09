data Jugador = UnJugador
  { nombre :: String,
    padre :: String,
    habilidad :: Habilidad
  }
  deriving (Eq, Show)

data Habilidad = Habilidad
  { fuerzaJugador :: Int,
    precisionJugador :: Int
  }
  deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)

todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)

rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro
  { velocidad :: Int,
    precision :: Int,
    altura :: Int
  }
  deriving (Eq, Show)

type Puntos = Int

-- Tiro de ejemplo
tiroEjemplo1 :: Tiro
tiroEjemplo1 = UnTiro 10 95 0

tiroEjemplo2 :: Tiro
tiroEjemplo2 = UnTiro 100 95 0

-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)

mayorSegun :: (Ord a) => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

-- También necesitaremos modelar los palos de golf que pueden usarse y los obstáculos que deben enfrentar para ganar el juego.

-- 1) Sabemos que cada palo genera un efecto diferente, por lo tanto elegir el palo correcto puede ser la diferencia entre ganar o perder el
--    torneo.
--     Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y
--     altura.
--         El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
--         La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
--         Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n,
--         La precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.
--     Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.

paloGolfitoPutter :: Habilidad -> Tiro
paloGolfitoPutter habilidad = UnTiro {velocidad = 10, precision = 2 * precisionJugador habilidad, altura = 0}

paloGolfitoMadera :: Habilidad -> Tiro
paloGolfitoMadera habilidad = UnTiro {velocidad = 100, altura = 5, precision = div (precisionJugador habilidad) 2}

paloGolfitoHierro :: Habilidad -> Int -> Tiro
paloGolfitoHierro habilidad n = UnTiro {velocidad = fuerzaJugador habilidad * n, precision = div (precisionJugador habilidad) n, altura = checkNegativo (n - 3)}

palos :: [Habilidad -> Tiro]
palos = [paloGolfitoPutter, paloGolfitoMadera] ++ map (flip paloGolfitoHierro) [1 .. 10]

-- 2) Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.
-- Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0.

golpe :: Jugador -> (Habilidad -> Tiro) -> Tiro
golpe persona palo = palo (habilidad persona)

-- 3) Lo que nos interesa de los distintos obstáculos es si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado
-- dicho tiro por el obstáculo. En principio necesitamos representar los siguientes obstáculos:
--     Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad del tiro.
--     Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0.
--     Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de
--     superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por
--     el largo de la laguna.
--     Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al superar el
--     hoyo, el tiro se detiene, quedando con todos sus componentes en 0.
-- Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, se detiene,
-- quedando con todos sus componentes en 0.

tunelConRampita :: Tiro -> Tiro
tunelConRampita tiro
  | precision tiro > 90 && altura tiro == 0 = UnTiro {velocidad = 2 * velocidad tiro, precision = 100, altura = 0}
  | otherwise = UnTiro {altura = 0, velocidad = 0, precision = 0}

laguna :: Int -> Tiro -> Tiro
laguna largo tiro
  | velocidad tiro > 80 && between 1 5 (altura tiro) = UnTiro {velocidad = velocidad tiro, precision = precision tiro, altura = div (altura tiro) largo}
  | otherwise = UnTiro {altura = 0, velocidad = 0, precision = 0}

hoyo :: Tiro -> Tiro
hoyo tiro = tiro {altura = 0, velocidad = 0, precision = 0}

pasoHoyo :: Tiro -> Bool
pasoHoyo tiro = between 5 20 (velocidad tiro) && precision tiro > 95 && altura tiro == 0

-- 4) Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

palosUtiles :: Jugador -> (Tiro -> Tiro) -> [Habilidad -> Tiro]
palosUtiles persona obstaculo = filter (saberSiPaso persona obstaculo) palos

--    Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
--    Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos de un hoyo,
--    el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo.
--    BONUS: resolver este problema sin recursividad, teniendo en cuenta que existe una función
--    takeWhile :: (a -> Bool) -> [a] -> [a] que podría ser de utilidad.

cuantosPuedoSuperar :: [Tiro -> Tiro] -> Tiro -> Int
cuantosPuedoSuperar [] _ = 0
cuantosPuedoSuperar (x : xs) tiro
  | saberSiPasoTiro tiro x = 1 + cuantosPuedoSuperar xs (x tiro)
  | otherwise = 0

--    Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar más
--    obstáculos con un solo tiro.

paloMasUtil :: Jugador -> [Tiro -> Tiro] -> (Habilidad -> Tiro)
paloMasUtil persona listaObstaculos = maximoSegun (\p -> cuantosPuedoSuperar listaObstaculos (obtenerTiro persona p)) palos

-- 5) Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
-- se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó el 
-- torneo si tiene más puntos que los otros niños.

padresQuePierden :: [(Jugador, Puntos)] -> [String]
padresQuePierden lista = map (padre . fst) (filter (\(_, puntos) -> puntos < puntuacionMaxima lista) lista)

-- Extras

checkNegativo :: Int -> Int
checkNegativo = max 0

saberSiPaso :: Jugador -> (Tiro -> Tiro) -> (Habilidad -> Tiro) -> Bool
saberSiPaso persona obstaculo palo = ((obstaculo . palo . habilidad) persona /= UnTiro {altura = 0, velocidad = 0, precision = 0}) || pasoHoyo ((obstaculo . palo . habilidad) persona)

saberSiPasoTiro :: Tiro -> (Tiro -> Tiro) -> Bool
saberSiPasoTiro tiro obstaculo = (obstaculo tiro /= UnTiro {altura = 0, velocidad = 0, precision = 0}) || pasoHoyo (obstaculo tiro)

obtenerTiro :: Jugador -> (Habilidad -> Tiro) -> Tiro
obtenerTiro persona palo = (palo.habilidad) persona

-- obtenerListaTiro :: Jugador -> [Tiro]
-- obtenerListaTiro persona = map (obtenerTiro persona) palos

-- listaDeCuantosPuedoSuperar :: [Tiro] -> [Tiro -> Tiro] -> [Int]
-- listaDeCuantosPuedoSuperar listaTiros listaObstaculos = map (cuantosPuedoSuperar listaObstaculos) listaTiros

puntuacionMaxima :: [(Jugador, Puntos)] -> Puntos
puntuacionMaxima = maximum . map snd