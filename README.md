# <p align="center">Paradigma-Funcional</p>
En este repositorio estan mis ejercicios de paradigma funcional, y mis apuntes. 

- [Paradigma-Funcional](#paradigma-funcional)
    - [Basico](#basico)
  - [Listas:](#listas)
  - [Tuplas](#tuplas)
  - [Funciones:](#funciones)
  - [Aplicacion parcial](#aplicacion-parcial)
  - [Composicion](#composicion)
  - [Data](#data)
  - [Recursividad](#recursividad)
  - [Evaluacion diferida](#evaluacion-diferida)

### Basico

---

- Expresividad (tiene que poder se entendido)
- Declaratividad (Concentrarme en que quiero y no tanto en como)
- No repetir logica

*Cosas importantes basicas:*
- Los tipos de datos como por ejemplo int, cuando aparecen en una declaracion de una funcion o de un tipo de data tienen que empezar en mayusculas.
- Misma regla para los tipos de datos que creemos y sus constructores

---
## Listas: 
Lo fundamental es que **todos los elementos son del mismo tipo**, se usa [ ]
- Se puede declarar listas de cierto valor a otro valor `[1.. 4] = [1,2,3,4]`
- Tambien se pueden hacer en orden inverso `[6,5.. 1] = [6,5,4,3,2,1]`
- O hacer la lista infinita `[1..]`
- Tabla n = `[n, 2*n..]`
- Lo que es muy util es que se puede hacer `[n1.. n2]` siempre que n2 > n1 y a su vez n1 y n2 pueden ser valores que reciba una funcion y arme la lista de n1 a n2
- El : en una lista devuelve del lado izquierdo la cabeza de la lista y a derecha el resto de la lista. `(cabeza:cola)`, funciona solo cuando la lista tiene almenos un elemento. Por lo general se usa `(x:xs)`
- El !! sirve devolver necesita una lista y un valor, por ejemplo `[1,2,3,4] !! 2` devuelve 3, ya que el indice comienza en 0
- `repeat` Toma un valor valor y arma una lista infinita con ese valor, por ejemplo `repeat 5 = [5,5,5,5...]`
- `cycle` Recibe una lista y la repite infinitamente, por ejemplo `cycle [1,2,3] = [1,2,3,1,2,3,1...]` o `cycle "hola" = ['h', 'o', 'l', 'a', 'h', 'o' ...]`
- `words` Es una funcion que recibe un String y devuelve una lista de String sin los espacios, por ejemplo `words "hola a todos" = ["hola", "a", "todos"]`
---
## Tuplas
Consiste en una **lista de elementos que pueden ser de distintos tipos**, se declaran con ( ). Ejemplo: (1,"hola",True)

La principal diferencia es que tienen un tamaño fijo, y las funciones que se le pueden aplicar son distintas.  

---

## Funciones:

`div` es una funcion que sirve para dividir dos numeros, muy util para trabajar con enteros.

`elem` es una funcion que toma un elemento y una lista e indica si ese elemento esta en la lista.

El uso de guardas permite establecer que hace una funcion en distintos casos, es importante no usar guardas de mas
Por ejemplo
```haskell
f x 
  | x == 1 = 3
  | x == 2 = 1
  | x == 3 = 2
  | otherwise = 0 
```
> Si no esperamos que reciba otro valor que no sea 1,2 o 3 no se pone el otherwise, es preferible el error antes que esconderlo con un valor.

En este caso la funcion nos devuelve 3 si mandamos 1, 1 si mandamos 2 o 2 si mandammos 3, pero si no cumple ninguno devuelve 0

`map` Es una funcion que recibe una funcion y una lista y aplica esa funcion en cada elemento de la lista.
Ejemplo `map (3*) [1,2,3,4,5] = [3,6,9,12,15]`  multiplica cada elemento por 3

`filter` Es una funcion que sirve para filtrar los elementos de una lista, recibe una funcion (que devuelve un Bool) y una lista con los elementos que cumplen esa condicion
`Ejemplo filter (>3) [1..6] = [4,5,6]`

`\` Es el operdor lambda que sirve para hacer funciones en el lugar sin tener que declararlas, sirve para funciones que hacen cosas simples y que no vale la pena declarar una funcion aparte. 
Ejemplo: `(\f -> f valor)` 
>Para todo f que encuentre, hace f de valor

`fold` Es aplicar sucesivamente una funcion foldl lo hace se hace a izquierda y foldr lo hace a derecha
Por ejemplo:  `foldl (+) 0 [1..10] = 55`, es la suma de todos los elementos

`foldl1` Lo aplica en funciones cerradas por ejemplo `foldl1 max [1,2,4,3,6,5] = 6`, en este caso lo que hace es el maximo entre 1 y 2, despues con ese resultado hace max resultado 4 y asi sucesivamente

>En este caso no se podria aplicar `fold max [1,2,4,3,6,5]`, porque max espera dos valores.

---

## Aplicacion parcial

Basicamente y resumido consiste en utilizar funciones sin hacer uso de todos sus parametros
Ejemplo: la funcion `(+)` recibe dos parametros para poder hacer la suma pero si solo le mando un parametro, lo que obtengo en vez de un valor (que seria lo que devuelve `+`) es una nueva funcion. Mi nueva funcion queda `(3+)` que cuando le pase un numero le suma 3. Otro ejemplo puede ser `(^2)` que cuando recibe un numero lo eleva al cuadrado.

---

## Composicion

El concepto es exactamente el mismo que para matematica f o g = f(g(x)), en este caso se usa el operdor `.`
Ejemplo: `(2*).(3*) 4 = 24 = 2*(3*4)`

---

## Data

```haskell
data Persona = UnaPersona{
    cansancio :: Int,
    nombre :: String
}deriving Show
```

En este caso `Persona` es el tipo de data, mientras que `UnaPersona` es el contructor que se utiliza para "armar nuevas Prsonas" 

Si por ejemplo quiero **modificar solo un dato de la persona que recibi** lo que puedo hacer es:

```haskell
trabajar :: Persona -> Int -> Persona
trabajar alguien masCansancio = alguien {cansancio = cansacio alguien + mas cansancio}
```
---

## Recursividad

Una funcion que se llama a si misma.

Por ejemplo el factorial de un numero seria

```haskell
f 0 = 1
f n = n * f(n-1)
```

Lista infinta desde un valor

```haskell
f :: Integer -> [Integer]
-- f 0 = [] con esto la lista no es infinita y termina en el 0 devolviendo hasta el 1
f n = n : f (n-1)
```
---

## Evaluacion diferida

```haskell
g :: Float -> Float
g 0 x = 0
g y 0 = y
g y x = x + y 
```

En este caso haskell empieza a ejecutar la funcion principal, es decir si le mando `g 0 42343/34342342`, ni va a hacer la division y va a devolver directamente 0, este es un ejemplo de evaluacion diferida