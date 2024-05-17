-- Queremos armar un programa que nos permita simular unas fantásticas carreras de autos en 
-- las cuales cada vehículo avanza tan rápido como puede para consagrarse campeón, aprovechando 
-- del uso de algunos poderes especiales (o power ups) que encuentren a lo largo del trayecto 
-- para sacar ventaja por sobre los demás autos.

-- De cada auto conocemos su color (que nos servirá para identificarlo durante el desarrollo de la carrera), 
-- la velocidad a la que está yendo y la distancia que recorrió, ambos valores de tipo entero.

-- De la carrera sólo nos interesa el estado actual de los autos que están participando, lo cual nos 
-- permitirá analizar cómo viene cada uno, y posteriormente procesar aquellos eventos que se den en la 
-- carrera para determinar el resultado de la misma.

-- Teniendo en cuenta lo descrito anteriormente se pide resolver los siguientes puntos explicitando 
-- el tipo de cada función desarrollada y utilizando los conceptos aprendidos del Paradigma Funcional, 
-- poniendo especial énfasis en el uso de Composición, Aplicación Parcial y Orden Superior.

-- 1) Declarar los tipos Auto y Carrera como consideres convenientes para representar la información indicada y 
-- definir funciones para resolver los siguientes problemas:
--     a) Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y 
--     la distancia que hay entre ellos (en valor absoluto) es menor a 10.
--     b) Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y 
--     les va ganando a todos (por haber recorrido más distancia que los otros).
--     c) Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera 
--     que le van ganando.

data Auto = UnAuto{
    color :: String,
    velocidad :: Int,
    distaciaRecorrida :: Int
}