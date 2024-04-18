data Nomus = UnNomus
  { tieneAlas :: Bool,
    multiplesBrazos :: Bool,
    cantOjos :: Int,
    color :: String,
    vida :: Int,
    fuerza :: Int,
    poderes :: [Poder]  }

-- Parte 1

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