data Nomus = UnNomus
  { tieneAlas :: Bool,
    multiplesBrazos :: Bool,
    cantOjos :: Int,
    color :: String,
    vida :: Int,
    fuerza :: Int,
    poderes :: [Poder]  } -- deriving Show -- Permite mostrar el nomu en terminal de forma estandar

-- Parte 1
-- En haskell no existen variables porque los datos no varian durante la ejecucion 
-- Es decir que si por ejemplo hago una funcion que entrene a un nomus y devuelva un nomus con mas fuerza
-- la funcion va a devolver otro nomus con mas fuerza, pero no la modifico del que recibo.

puedeVer :: Nomus -> Bool
puedeVer (UnNomus _ _ ojos _ _ _ _) = ojos >= 1

determinarCategoria :: Nomus -> String
determinarCategoria (UnNomus _ _ _ _ _ fuerza _)
  | fuerza >= 10000 = "high-end"
  | fuerza >= 3000 = "fuerte"
  | fuerza >= 1000 = "comun"
  | otherwise = "pichi"

-- Parte 2

data Poder = UnPoder
  { probabilidadCritico :: Float,
    curacionUso :: Int,
    cantDaño :: Int,
    rango :: Int
  }

probCriticoUltPoder :: Nomus -> Float
probCriticoUltPoder (UnNomus _ _ _ _ _ _ poderes) = (probabilidadCriticoPoder.last) poderes
--probCriticoUltPoder (UnNomus _ _ _ _ _ _ poderes) = (probabilidadCriticoPoder.extraerUltPoder) poderes

probabilidadCriticoPoder :: Poder -> Float
probabilidadCriticoPoder (UnPoder probCritico _ _ _) = probCritico

-- extraerUltPoder :: [Poder] -> Poder
-- extraerUltPoder [x] = last [x]