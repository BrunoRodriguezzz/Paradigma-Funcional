
-- Ejercicios listas:
-- 1) Definir una función que sume una lista de números.
--    Nota: Investigar sum

suma :: [Int] -> Int
suma = sum

-- 2) Durante un entrenamiento físico de una hora, cada 10 minutos de entrenamiento se tomóo la frecuencia cardíaca de
-- uno de los participantes obteniéndose un total de 7 muestras que son las siguientes:
-- frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]
-- Comienza con un frecuencia de 80 min 0.
-- A los 10 min la frecuencia alcanza los 100
-- A los 20 min la frecuencia es de 120,
-- A los 30 min la frecuencia es de 128
-- A los 40 min la frecuencia es de 130, …etc..
-- A los 60 min la frecuencia es de 125 frecuenciaCardiaca es un función constante.
-- Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca.
-- Main> promedioFrecuenciaCardiaca
-- 115.285714285714
-- Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la
-- frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60.
-- Main> frecuenciaCardiacaMomento 30
-- 128
-- Ayuda: Vale definir una función auxiliar para conocer el número de muestra.
-- Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m.
-- Main> frecuenciasHastaMomento 30
-- [80, 100, 120, 128]
-- Ayuda: Utilizar la función take y la función auxiliar definida en el punto anterior.

frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125]

promedioFrecuenciaCardiaca :: [Int] -> Float
promedioFrecuenciaCardiaca lista = fromIntegral (sum lista) / fromIntegral (length lista)

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto m = frecuenciaCardiaca !! div m 10

frecuenciasHastaMomento :: Int -> [Int]
frecuenciasHastaMomento cantFrec = take (div cantFrec 10 + 1) frecuenciaCardiaca

-- 3) Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación
-- de las sublistas es una lista capicua..Ej:
-- Main> esCapicua ["ne", "uqu", "en"]
-- True
-- Porque “neuquen” es capicua.
-- Ayuda: Utilizar concat/1, reverse/1.

esCapicua :: [String] -> Bool
esCapicua lista = concat lista == reverse (concat lista)

-- 4) Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un período determinado,
-- discriminadas en horario normal y horario reducido.
-- duracionLlamadas = (("horarioReducido",[20,10,25,15]),(“horarioNormal”,[10,5,8,2,9,10])).
-- Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, en el de tarifa normal o en el reducido.
-- Main> cuandoHabloMasMinutos
-- “horarioReducido”
-- Definir la función cuandoHizoMasLlamadas, devuelve en que franja horaria realizó más cantidad de llamadas, en el de tarifa normal o en el reducido.
-- Main> cuandoHizoMasLlamadas
-- “horarioNormal”
-- Nota: Utilizar composición en ambos casos

duracionLlamadas :: ((String, [Int]), (String, [Int]))
duracionLlamadas = (("horarioReducido", [20, 10, 25, 15]), ("horarioNormal", [10, 5, 8, 2, 9, 10]))

cuandoHabloMasMinutos :: ((String, [Int]), (String, [Int])) -> String
cuandoHabloMasMinutos ((horRed, lista1), (horNorm, lista2))
  | sum lista1 > sum lista2 = "Horario Reducido"
  | sum lista1 < sum lista2 = "Horario Normal"

cuandoHizoMasLlamadas :: ((String, [Int]), (String, [Int])) -> String
cuandoHizoMasLlamadas ((horRed, lista1), (horNorm, lista2))
  | length lista1 > length lista2 = "Horario Reducido"
  | length lista1 < length lista2 = "Horario Normal"

-- Orden Superior
-- 1) Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve True si existe algún
-- elemento de la tupla que haga verdadera la función.
-- Main> existsAny even (1,3,5)
-- False

-- Main> existsAny even (1,4,7)
-- True
-- porque even 4 da True

-- Main> existsAny (0>) (1,-3,7)
-- True
-- porque even -3 es negativo

existsAny :: (Int -> Bool) -> (Int, Int, Int) -> Bool
existsAny f (n1, n2, n3) = f n1 || f n2 || f n3

-- 2) Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto. P.ej.
-- Main> mejor cuadrado triple 1
-- 3
-- (pues triple 1 = 3 > 1 = cuadrado 1)

-- Main> mejor cuadrado triple 5
-- 25
-- (pues cuadrado 5 = 25 > 15 = triple 5)
-- Nota: No olvidar la función max.

mejorDeDosF :: (Int -> Int) -> (Int -> Int) -> Int -> Int
mejorDeDosF f g numero
  | f numero > g numero = f numero
  | otherwise = g numero

-- 3) Definir la función aplicarPar/2, que recibe una función y un par, 
-- y devuelve el par que resulta de aplicar la función a los elementos del par. P.ej. 
-- Main> aplicarPar doble (3,12) 
-- (6,24) 

-- Main> aplicarPar even (3,12) 
-- (False, True) 

-- Main> aplicarPar (even . doble) (3,12) 
-- (True, True) 
 
aplicarPar ::  (a -> b) -> (a, a) -> (b, b)
aplicarPar f (nro1, nro2) = (f nro1, f nro2)

-- 4) Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que 
-- es el resultado de aplicar las dos funciones al valor. P.ej. 
-- Main> parDeFns even doble 12 
-- (True, 24) 

parDeFns :: (a -> b) -> (a -> c) -> a -> (b, c) --se puede declarar funciones si determinar el tipo de variables
parDeFns f g nro = (f nro, g nro)

-- Orden superior o listas

-- 1) Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True 
-- si el número es múltiplo de alguno de los números de la lista. P.ej. 
-- Main> esMultiploDeAlguno 15 [2,3,4] 
-- True, 
-- porque 15 es múltiplo de 3 

-- Main> esMultiploDeAlguno 34 [3,4,5] 
-- False 
-- porque 34 no es múltiplo de ninguno de los 3 Nota: Utilizar la función any/2

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x==0

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno nro lista = any (esMultiploDe nro) lista

-- 2) Armar una función promedios/1, que dada una lista de listas me devuelve la 
-- lista de los promedios de cada lista-elemento. P.ej. 
-- Main> promedios [[8,6],[7,9,4],[6,2,4],[9,6]] 
-- [7,6.67,4,7.5] 
-- Nota: Implementar una solución utilizando map/2. 

promedios:: [[Float]] -> [Float]
promedios listaDeListas = map promedioDeLista listaDeListas

promedioDeLista :: [Float] -> Float
promedioDeLista lista = (sum lista) / fromIntegral(length lista)

-- 3) Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada 
-- lista-elemento, excluyendo los que sean menores a 4 que no se cuentan. P.ej. 
-- Main> promediosSinAplazos [[8,6],[6,2,6]] 
-- [7,6] 
-- Nota: Implementar una solución utilizando map/2. 

promediosSinAplazos :: [[Float]] -> [Float]
promediosSinAplazos listaDeListas = filter (4<) (promedios listaDeListas)

-- 4) Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno. P.ej. 
-- Main> mejoresNotas [[8,6,2,4],[7,9,4,5],[6,2,4,2],[9,6,7,10]] 
-- [8,9,6,10]. 
-- Ayuda: Utilizar la función predefinida maximum/1. 

mejoresNotas :: [[Float]] -> [Float] --no hace falta poner en este caso la variable que recibe
mejoresNotas = map maximum 

-- 5) Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True
--  si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más. P.ej. 
-- Main> aprobo [8,6,2,4] 
-- False 
-- Main> aprobo [7,9,6,8] 
-- True 
-- Ayuda: Utilizar la función predefinida minimum/1. 

aprobo :: [Int] -> Bool
aprobo = all (6>)

-- 6) Definir la función aprobaron/1, que dada la información de un curso devuelve 
-- la información de los alumnos que aprobaron. P.ej. 
-- Main> aprobaron [[8,6,2,4],[7,9,6,7],[6,2,4,2],[9,6,7,10]] 
-- [[7,9,6,7],[9,6,7,10]] 
-- Ayuda: usar la función aprobó/1. 

aprobaron :: [[Int]] -> [[Int]]
aprobaron = filter aprobo

-- 7) Definir la función divisores/1, que recibe un número y devuelve la lista de divisores. P.ej. 
-- Main> divisores 60 
-- [1,2,3,4,5,6,10,12,15,20,30,60] 
-- Ayuda: para calcular divisores n alcanza con revisar los números entre 1 y n. 

esDivisor :: Int -> Int -> Bool
esDivisor numero divisor = numero `mod` divisor == 0

divisores :: Int -> [Int]
divisores n = filter (esDivisor n) [1..n]

-- 8) Definir la función exists/2, que dadas una función booleana y una lista devuelve True 
-- si la función da True para algún elemento de la lista. P.ej. 
-- Main> exists even [1,3,5] 
-- False 
-- Main> exists even [1,4,7] 
-- True 
-- porque even 4 da True 

exists :: (a -> Bool) -> [a] -> Bool --NO ME LO PUEDO CREER MADRE MIA WILLY
exists = any 

-- 9) Definir la función hayAlgunNegativo/2, que dada una lista de números y 
-- un (…algo…) devuelve True si hay algún nro. negativo. 
-- Main> hayAlgunNegativo [2,-3,9] (…algo…) 
-- True 

hayAlgunNegativo :: [Int] -> a -> Bool
hayAlgunNegativo lista algo = any (0>) lista

-- 10) Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor 
-- cualquiera, devuelve la lista del resultado de aplicar las funciones al valor. P.ej. 
-- Main> aplicarFunciones[(*4),(+3),abs] (-8) 
-- [-32,-5,8] 
-- Si pongo:
-- Main> aplicarFunciones[(*4),even,abs] 8 
-- da error. ¿Por qué? 

aplicarFunciones :: [(a -> b)] -> a -> [b]
aplicarFunciones listaFunciones valor = map listaFunciones valor