data Nomus = UnNomus{
    tieneAlas:: Bool,
    multiplesBrazos:: Bool,
    cantOjos:: Int,
    color:: String,
    vida:: Int,
    fuerza:: Int,
    poderes:: [unPoder]
}

--Parte 1

puedeVer:: Nomus -> Bool
puedeVer (unNomus alas brazos ojos color vida fuerza [Poder]) = ojos >= 1 

determinarCategoria:: Nomus -> String
determinarCategoria (unNomus alas brazos ojos color vida fuerza [Poder]) | fuerza >= 10000 = "high-end"
                                                                 | fuerza >= 3000 = "fuerte"
                                                                 | fuerza >= 1000 = "comun"
                                                                 | otherwise = "pichi"

--Parte 2

data Poder = UnPoder{
    probabilidadCritico:: Float
    curacionUso:: Int,
    cantDaño:: Int,
    rango:: Int,
}

probCriticoUltPoder:: Nomus -> Float
probabilidadCritico (unNomus a, b, o, c, v, f, [Poder]) = probabilidadCriticoPoder . extraerUltPoder

probabilidadCriticoPoder:: Poder -> Float
probabilidadCriticoPoder (unPoder probCritico _ _ _) = probCritico

extraerUltPoder:: [Poder] -> Poder
extraerUltPoder [x] = last [x] 
