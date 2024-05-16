-- En el pueblo de South Park, hay varios personajes conocidos. De cada uno se conoce su nombre,
-- la cantidad de dinero que posee y su nivel de felicidad (que puede ser mayor o igual a cero, pero nunca negativo).

data Personaje = UnPersonaje
  { nombre :: String,
    dinero :: Int,
    felicidad :: Int
  }
  deriving (Show)

butters :: Personaje
butters = UnPersonaje "Butters" 100 100

-- 1) Los personajes pueden realizar diversas actividades que los afectan de la siguiente manera:
-- Ir a la escuela primaria de South Park: resta 20 de felicidad al personaje, a excepción de Butters,
-- quien aumenta su felicidad en la misma cantidad.
-- Comer una cierta cantidad de Cheesy Poofs: aumenta 10 de felicidad y resta 10$ por cada Cheesy Poof consumido.
-- Ir a trabajar: ganan una cantidad de dinero dependiendo del trabajo. Si es en el "Restaurante de City Wok",
-- ganan 23$ porque tiene 23 caracteres.
-- Hacer doble turno: implica ir a trabajar el doble de tiempo y restar tanta felicidad como caracteres tenga el trabajo.
-- Jugar World of Warcraft: por cada amigo con el que juega cada hora aumenta 10 de felicidad, y por cada hora pierde $10.
-- A partir de las 5 horas no aumenta más la felicidad, pero el dinero sigue disminuyendo.
-- Por ejemplo, si Stan juega con 3 amigos por 8 horas aumenta en 150 su felicidad y pierde $80.
-- Realizar una actividad inventada que modifique al personaje de alguna manera.

irEscuelaPrimaria :: Personaje -> Personaje
irEscuelaPrimaria personaje
  | nombre personaje == "Butters" = personaje
  | otherwise = UnPersonaje {felicidad = felicidad personaje - 20}

comerCheesyPoofs :: Int -> Personaje -> Personaje
comerCheesyPoofs cantCheesyPoofs personaje = UnPersonaje {felicidad = felicidad personaje + 10 * cantCheesyPoofs, dinero = dinero personaje - 10 * cantCheesyPoofs}

irATrabajar :: String -> Personaje -> Personaje
irATrabajar trabajo personaje = UnPersonaje {dinero = dinero personaje + length trabajo}

hacerDobleTurno :: String -> Personaje -> Personaje
hacerDobleTurno trabajo personaje = UnPersonaje {felicidad = max (felicidad personaje - length trabajo) 0, dinero = dinero personaje + ((2 *) . length) trabajo}
-- no puede bajar la felicidad de 0 por eso el max 

jugarWorldOfWarcraft :: Int -> Int -> Personaje -> Personaje
jugarWorldOfWarcraft amigos horas personaje
  | horas < 5 = UnPersonaje {felicidad = felicidad personaje + 10 * amigos * horas, dinero = dinero personaje - 10 * horas}
  | otherwise = UnPersonaje {felicidad = felicidad personaje + 50 * amigos, dinero = dinero personaje - 10 * horas}

-- se podria hacer un min entre horas y 5 para no hacer las guardas

irAlGimnasio :: Int -> Personaje -> Personaje
irAlGimnasio horas personaje = UnPersonaje {nombre = "Trembo " ++ nombre personaje, felicidad = felicidad personaje + 15 * horas, dinero = dinero personaje - 30} --El precio de la trembo