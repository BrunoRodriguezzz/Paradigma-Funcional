esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod y x==0

-- 5 Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no 
--es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2

esBisiesto :: Int -> Bool
esBisiesto x = esMultiploDe 400 x || (esMultiploDe 4 x && not (esMultiploDe 100 x))

--6 Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit

celsiusToFahr :: Float -> Float
celsiusToFahr x = x*9/5 + 32

--7 Definir la función fahrToCelsius/1, la inversa de la anterior.

fahrToCelsius :: Float -> Float
fahrToCelsius x = (x-32)*5/9

-- 8 Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit 
-- es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 

haceFrioF :: Float -> Bool
haceFrioF x = fahrToCelsius x < 8

-- 9 Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula. 
-- m.c.m.(a, b) = {a * b} / {m.c.d.(a, b)} 

--mcm :: Int -> Int -> Int
--mcm x y = (x * y) / (gcd x y)

--10 Dispersión
--Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos;
--cada medición es un entero que representa una cantidad de cm. 
--P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
--A partir de estos tres números, podemos obtener algunas conclusiones. 
--Definir estas funciones: 

--dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a 
--tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda 
--(las guardas están en max y min, que estamos usando). 

--diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, 
--que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos. Una dispersión se considera 
--chica si es de menos de 30 cm, y grande si es de más de un metro. 
--Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas. 

dispersion :: Int -> Int -> Int -> Int
dispersion x y z = max (max x y) z - min(min x y ) z

diasParejos :: Int -> Int -> Int -> Bool
diasParejos x y z = dispersion x y z < 30

diasLocos :: Int -> Int -> Int -> Bool
diasLocos x y z = dispersion x y z > 100

diasNormales :: Int -> Int -> Int -> Bool
diasNormales x y z = not (diasLocos x y z || diasParejos x y z)

--12 Este ejercicio alguna vez se planteó como un Desafío Café con Leche: Implementar la función esCuadradoPerfecto/1, 
--sin hacer operaciones con punto flotante. Ayuda: les va a venir bien una función auxiliar, tal vez de dos parámetros. 
--Pensar que el primer cuadrado perfecto es 0, para llegar al 2do (1) sumo 1, para llegar al 3ro (4) sumo 3, para llegar 
--al siguiente (9) sumo 5, después sumo 7, 9, 11 etc.. También algo de recursividad van a tener que usar. NO ANDA

esCuadradoPerfecto :: Int -> Bool
esCuadradoPerfecto x = cuadradosPerfectos x 0

cuadradosPerfectos :: Int -> Int -> Bool
cuadradosPerfectos x y | x == y*y = True
                       | x > y*y = cuadradosPerfectos x (y+1)
                       | otherwise = False
