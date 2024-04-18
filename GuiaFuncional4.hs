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
