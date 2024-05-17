-- CraftMine

data Personaje = UnPersonaje{
    nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
} deriving Show

type Material = String

data Receta = UnaReceta{
    ingredientes :: [Material],
    tiempo :: Int,
    resultadoMaterial :: Material
}

-- Punto 1

craftear :: Personaje -> Receta -> Personaje
craftear pj receta | tieneTodosLosIngredientes pj receta = pj {inventario = ((resultadoMaterial receta):(quitarMateriales pj receta)), puntaje = (puntaje pj)-100+(10*(tiempo receta))}  
    | otherwise = pj

tieneTodosLosIngredientes :: Personaje -> Receta -> Bool
tieneTodosLosIngredientes pj receta = all (\objInventario -> elem objInventario (ingredientes receta)) (inventario pj)

-- quitarMaterailes :: Personaje -> Receta -> [Material]
-- quitarMaterailes pj receta = filter (\material -> not (elem material (ingredientes receta))) (inventario pj)

quitarMateriales :: Personaje -> Receta -> [Material]
quitarMateriales pj receta = foldl quitarUnMaterial (inventario pj) (ingredientes receta)

quitarUnMaterial :: [Material] -> Material -> [Material]
quitarUnMaterial inventario matReceta = takeWhile (matReceta/=) inventario ++ drop(length(takeWhile (matReceta/=) inventario) +1) inventario

-- Punto 2

queRecetasPuedeCraftearYDuplicanPuntaje :: Personaje -> [Receta] -> [Receta]
queRecetasPuedeCraftearYDuplicanPuntaje pj listaRecetas = filter (\r -> cumpleTodosLosRequisitos pj r) listaRecetas

cumpleTodosLosRequisitos :: Personaje -> Receta -> Bool
cumpleTodosLosRequisitos pj receta = ((2*).puntaje) pj <= puntaje(craftear pj receta) && tieneTodosLosIngredientes pj receta

queRecetasPuedeHacer :: Personaje -> [Receta] -> [Receta]
queRecetasPuedeHacer pj = filter (tieneTodosLosIngredientes pj)

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente pj listReceta = foldl craftear pj listReceta

mayorQueOrdenInverso :: Personaje -> [Receta] -> Bool
mayorQueOrdenInverso pj lista = puntaje (craftearSucesivamente pj lista) > puntaje (craftearSucesivamente pj (reverse lista))

-- Mine