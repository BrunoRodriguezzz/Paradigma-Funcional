-- Enunciado: Hola, buen día. Arrancamos, les comparto el enunciado del parcial https://docs.google.com/document/d/1vfmY4xOaGVMtDCixKtA6rDugwWM86ZJaL7K5vDKcksA/edit?usp=sharing

-- Punto 1

-- Los Fremen 

-- La tribu está compuesta por varios Fremen. Cada Fremen tiene un nombre, un nivel de tolerancia a la Especia,
--  una serie de títulos y una cantidad de reconocimientos.  

-- Ejemplo:
-- Stilgar tiene un nivel de tolerancia a la Especia de 150 y el título de "Guía". Fue reconocido 3 veces. 

data Fremen = UnFremen{
    nombre :: String,
    toleranciaEspecia :: Float,
    titulos :: [String],
    cantReconocimientos :: Int
} deriving (Eq, Show)

-- Se pide:
-- Averiguar cómo queda un Fremen al recibir un nuevo reconocimiento.

otorgarReconocimiento :: Fremen -> Fremen
otorgarReconocimiento fremen = fremen {cantReconocimientos = cantReconocimientos fremen + 1}

-- Saber si hay algún candidato a ser el elegido. Son candidatos quienes tengan el título de "Domador" y una 
-- tolerancia a la especia de más de 100.

condicionCandidato :: Fremen -> Bool
condicionCandidato fremen = esDomador fremen && toleranciaEspecia fremen > 100

esDomador :: Fremen -> Bool
esDomador = (elem "Domador").titulos

-- Hallar al Elegido: Es el Fremen de la tribu que más reconocimientos tenga entre los candidatos a serlo. 

elegido :: [Fremen] -> Fremen
elegido lista = encontrarMaxSegunParametro cantReconocimientos (filter condicionCandidato lista)

encontrarMaxSegunParametro :: Ord b => (a -> b) -> [a] -> a
-- encontrarMaxSegunParametro _ [] = 
encontrarMaxSegunParametro _ [x] = x
encontrarMaxSegunParametro parametro (x:y:ys)
    | parametro x > parametro y = encontrarMaxSegunParametro parametro (x:ys)
    | otherwise = encontrarMaxSegunParametro parametro (y:ys)

-- Punto 2

-- Gusanos de arena

-- Se conoce su longitud, su nivel de hidratación y una descripción. 
-- Los gusanos se reproducen. Dados dos gusanos, la cría nace de la siguiente manera:
-- Su longitud es el 10% de la máxima longitud de sus dos progenitores
-- Su nivel de hidratación es 0
-- La descripción es la concatenación de ambas descripciones de sus progenitores. 
-- Ejemplo:
-- Un gusano de longitud 10, hidratacion 5 y descripción “rojo con lunares”
-- Otro gusano de longitud 8, hidratación 1 y descripción “dientes puntiagudos”
-- La cría entre estos dos gusanos debería tener de longitud 1, hidratación 0 y descripción 
-- “rojo con lunares - dientes puntiagudos”

data Gusano = UnGusano{
    longitud :: Float,
    nivelHidratacion :: Int,
    descripcion :: String
} deriving Show

apareoGusanos :: Gusano -> Gusano -> Gusano
apareoGusanos gusano1 gusano2 = UnGusano {longitud = nuevaLongitud gusano1 gusano2, nivelHidratacion = 0, descripcion = nuevaDescripcion gusano1 gusano2}

nuevaLongitud :: Gusano -> Gusano -> Float
nuevaLongitud gusano1 gusano2 = 0.1*(max (longitud gusano1) (longitud gusano2))

nuevaDescripcion :: Gusano -> Gusano -> String
nuevaDescripcion gusano1 gusano2 = (descripcion gusano1) ++ " - " ++ (descripcion gusano2)

-- Se pide:
-- Obtener la lista de crías que surge de aparear dos listas de gusanos, uno a uno. En caso que un gusano no 
-- tenga con qué otro gusano aparearse, obviamente no hay cría. Por ejemplo, el primero de la primera lista con 
-- el primero de la segunda lista, los segundos entre sí y así sucesivamente.

listaCrias :: [Gusano] -> [Gusano] -> [Gusano]
listaCrias lista1 lista2 = apareoConFuncion apareoGusanos lista1 lista2

apareoConFuncion :: (a -> a -> a) -> [a] -> [a] -> [a]
apareoConFuncion _ [] [] = []
apareoConFuncion _ [] l2 = l2 
apareoConFuncion _ l1 [] = l1
apareoConFuncion funcion (x:xs) (y:ys) = (funcion x y: apareoConFuncion funcion xs ys) 

-- Punto 3

-- Los Fremen realizan misiones para sobrevivir en el desierto donde proliferan los gusanos. Las misiones son:

-- Domar gusano de arena: Un Fremen puede domar a un gusano de arena si su nivel de tolerancia a la Especia es 
-- al menos la mitad de la longitud del gusano. Al hacerlo, obtiene el título de "Domador" y su tolerancia a la 
-- especia aumenta en 100 unidades. Si no lo puede hacer su tolerancia a la Especia baja un 10%. 

-- Destruir gusano de arena: Un Fremen puede destruir a un gusano de arena si tiene el título de "Domador" y si 
-- su nivel de tolerancia a la Especia es menor la mitad de la longitud del gusano. Al hacerlo, recibe un 
-- reconocimiento y su tolerancia a la especia aumenta en 100 unidades. Si no lo logra, su especia baja un 20%. 

-- Inventada: Inventar otra misión que un Fremen pueda hacer con un gusano, que también se pueda realizar 
-- dependiendo de cómo sea el gusano en relación al Fremen y que provoque consecuencias diferentes sobre el Fremen 
-- si lo logra o no.

type Misiones = Fremen -> Gusano -> Fremen

domar :: Misiones
domar fremen gusano
    | condicion1  (>=) fremen gusano = (modificarTolerancia (+100).otorgarTitulo "Domador") fremen 
    | otherwise = modificarTolerancia (0.9*) fremen

destruirGusano :: Misiones
destruirGusano fremen gusano
    | condicion1 (<) fremen gusano = (modificarTolerancia (+100).otorgarReconocimiento) fremen 
    | otherwise = modificarTolerancia (0.8*) fremen 

engañar :: Misiones
engañar fermen gusano
    | elem "Bufon" (titulos fermen) && longitud gusano > 300 = (otorgarReconocimiento.otorgarReconocimiento) fermen
    | otherwise = modificarTolerancia(0.1*) fermen


condicion1 :: (Float -> Float -> Bool) -> Fremen -> Gusano -> Bool
condicion1 funcion fremen gusano = esDomador fremen && funcion (toleranciaEspecia fremen) (((/2).longitud) gusano)

otorgarTitulo :: String -> Fremen -> Fremen
otorgarTitulo titulo fremen 
    | elem "Domador" (titulos fremen) = fremen
    | otherwise = fremen {titulos = [titulo] ++ titulos fremen}

modificarTolerancia :: (Float -> Float) -> Fremen -> Fremen
modificarTolerancia funcion fremen = fremen {toleranciaEspecia = (funcion.toleranciaEspecia) fremen}

-- Se pide
--      Simular la realización colectiva de una misión: Dada una tribu, una misión cualquiera y un gusano de arena, 
-- hacer que cada uno de los Fremen de la tribu intenten llevar a cabo la misión con dicho gusano, obteniendo 
-- cómo queda la tribu en consecuencia.
--      Averiguar, para una tribu, una misión y un gusano, si el hecho de realizarla colectivamente haría que el 
-- elegido de la tribu fuera un Fremen diferente al que hubieran elegido previamente.

misionColectiva :: Gusano -> Misiones -> [Fremen] -> [Fremen]
misionColectiva gusano mision = map (\fremen -> mision fremen gusano)

cambiaElegido :: [Fremen] -> Gusano -> Misiones -> Bool    --Si no cambia True
cambiaElegido tribu gusano mision = elegido tribu ==  (elegido.(misionColectiva gusano mision)) tribu

-- Punto 4

-- Qué pasaría con una tribu de infinitos Fremen?
--      Al entrenarlos | entrenarlos = realizar un funcion colectivamente
--      Al querer saber si hay algún candidato a ser elegido
--      Al encontrar al elegido

-- Justificar conceptualmente
-- Mostrar un ejemplo de consulta y su resultado.

-- Entrenarlos
-- En este caso al tener una tribu infinita no se terminaria de ejecutar nunca map ya que, siempre estaria 
-- recorriendo la lista sin parar.
-- Ejemplo misionColectiva gusanoN1 domar tribuInfinita, muestra en el terminal el resultado de que cada Fremen
-- dome al gusanoN1 repitiendo sin fin.

-- Al querer saber si hay algun candidato a ser elegido
-- Si lo que se busca es una cantidad determinada de candidatos (por ejemplo 3 o 1), lograria devolver esos 3
-- candidatos (salvo que en esa lista infinita existieran 2 o menos) y eso se debe a la evauacion diferida,
-- que al encontrar los 3 que necesitaba deja de recorrer la lista. Pero si queremos todos los candidatos la
-- funcion no terminaria de ejecutarse nunca.
-- Ejemplo de 1 tribuInfinita !! length (takeWhile (not.condicionCandidato) tribuInfinita) devuelve a pepe

-- Al encontrar al elegido
-- Esta funcion no podria ejecutarse ya que el filter jamas terminaria de devolver la lista filtrada que es 
-- que es infinita.
-- Ejemplo elegido tribuInfinita no devuelve nada y hay que interrumpir el terminal porque se queda ahi.

-- Ejemplos

stilgar :: Fremen
stilgar = UnFremen {nombre = "Stilgar", toleranciaEspecia = 150, titulos = ["Guía"], cantReconocimientos = 3}
juan :: Fremen
juan = UnFremen {nombre = "Juan", toleranciaEspecia = 50, titulos = ["Domador", "Ingeniero"], cantReconocimientos = 2}
pepe :: Fremen
pepe = UnFremen {nombre = "Pepe", toleranciaEspecia = 110, titulos = ["Domador", "Guerro"], cantReconocimientos = 4}
lucas :: Fremen
lucas = UnFremen {nombre = "Lucas", toleranciaEspecia = 160, titulos = ["Domador"], cantReconocimientos = 5}
tribu1 :: [Fremen]
tribu1 = [stilgar, juan, pepe, lucas]
gusanoN1 :: Gusano
gusanoN1 = UnGusano {longitud = 200, nivelHidratacion = 4, descripcion = "lento"}
gusanoN2 :: Gusano
gusanoN2 = UnGusano {longitud = 170, nivelHidratacion = 3, descripcion = "pegajoso"}
gusanoN3 :: Gusano
gusanoN3 = UnGusano {longitud = 350, nivelHidratacion = 7, descripcion = "sangre venenosa"}
gusanoN4 :: Gusano
gusanoN4 = UnGusano {longitud = 210, nivelHidratacion = 11, descripcion = "timido"}
gusanoN5 :: Gusano
gusanoN5 = UnGusano {longitud = 190, nivelHidratacion = 4, descripcion = "pegajoso"}
grupoGusanos1 :: [Gusano]
grupoGusanos1 = [gusanoN1, gusanoN2, gusanoN3]
grupoGusanos2 :: [Gusano]
grupoGusanos2 = [gusanoN4, gusanoN5]

tribuInfinita :: [Fremen]
tribuInfinita = cycle tribu1