# Paradigma-Funcional
En este repositorio estan mis ejercicios de paradigma funcional.

Apuntes: 

Cosas importantes basicas:
- Los tipos de datos como por ejemplo int, cuando aparecen en una declaracion de una funcion o de un tipo de data tienen que empezar en mayusculas.
- Misma regla para los tipos de datos que creemos y sus constructores

Cosas de listas:
- Se puede declarar listas de cierto valor a otro valor [1.. 4] = [1,2,3,4]
- Tambien se pueden hacer en orden inverso [6,5.. 1] = [6,5,4,3,2,1]
- O hacer la lista infinita [1..]
- tabla n = [n, 2*n..]
- Lo que es muy util es que se puede hacer [n1.. n2] siempre que n2>n1 y a su vez n1 y n2 pueden ser valores que reciba una funcion y arme la lista de n1 a n2

El : en una lista devuelve del lado izquierdo la cabeza de la lista y a derecha el resto de la lista. (cabeza:cola), funciona solo cuando la lista tiene almenos un elemento.
El !! sirve devolver necesita una lista y un valor, por ejemplo [1,2,3,4] !! 2 devuelve 3, ya que el indice comienza en 0

Funciones:

div es una funcion que sirve para dividir dos numeros, muy util para trabajar con enteros.

elem es una funcion que toma un elemento y una lista e indica si ese elemento esta en la lista.

map es una funcion que recibe una funcion y una lista y aplica esa funcion en cada elemento de la lista 
Ejemplo map (3*) [1,2,3,4,5] = [3,6,9,12,15]  multiplica cada elemento por 3

filter es una funcion que sirve para filtrar los elementos de una lista, recibe una funcion (que devuelve True) y una lista con los elementos que cumplen esa condicion
Ejemplo filter (>3) [1..6] = [4,5,6]

\ es el operdor lamnda que sirve para hacer funciones en el lugar sin tener que declararlas, sirve para funciones que hacen cosas simples y que no vale la pena declarar una funcion aparte. 
(\f -> f valor) para todo f que encuentre, hace f de valor

fold es aplicar sucesivamente una funcion foldl lo hace se hace a izquierda y foldr lo hace a derecha
Por ejemplo:  foldl (+) 0 [1..10] = 55, es la suma de todos los elementos

foldl1 lo aplica en funciones cerradas por ejemplo 
foldl1 max [1,2,4,3,6,5] = 6, en este caso lo que hace es el maximo entre 1 y 2, despues con ese resultado hace max resultado 4 y asi sucesivamente

En este caso no se podria aplicar fold max [1,2,4,3,6,5], porque max espera dos valores.

Data

data Persona = UnaPersona{
    cansancio :: Int,
    nombre :: String
}deriving Show

En este caso Persona e el tipo de data, mientras que UnaPersona es el contructor que se utiliza para "armar nuevas Prsonas" 
Si por ejemplo quiero modificar solo un dato de la persona que recibi lo que puedo hacer es:

trabajar :: Persona -> Int -> Persona
trabajar alguien masCansancio = alguien {cansancio = cansacio alguien + mas cansancio}
Probar fold trabajar Jose [10,20,30]
Con:
Jose :: Persona
Jose = 10 "Jose"

Recursividad

Por ejemplo el factorial de un numero seria
f 0 = 1
f n = n * f(n-1)

Lista infinta desde un valor
f :: Integer -> [Integer]
-- f 0 = [] con esto la lista no es infinita y termina en el 0 devolviendo hasta el 1
f n = n : f (n-1)

Evaluacion diferida

g :: Float -> Float
g 0 x = 0
g y 0 = y
g y x = x + y 

En este caso haskell empieza a ejecutar la funcion principal, es decir si le mando g 0 42343/34342342, ni va a hacer la division y va a devolver directamente 0, este es un ejemplo de evaluacion diferida