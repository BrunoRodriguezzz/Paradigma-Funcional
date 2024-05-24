-- CraftMine

data Personaje = UnPersonaje
  { nombre :: String,
    puntaje :: Int,
    inventario :: [Material]
  }
  deriving (Show)

type Material = String

data Receta = UnaReceta
  { ingredientes :: [Material],
    tiempo :: Int,
    resultadoMaterial :: Material
  }

-- Punto 1

craftear :: Personaje -> Receta -> Personaje
craftear pj receta
  | tieneTodosLosIngredientes pj receta = pj {inventario = ((resultadoMaterial receta) : (quitarMateriales pj receta)), puntaje = (puntaje pj) - 100 + (10 * (tiempo receta))}
  | otherwise = pj

tieneTodosLosIngredientes :: Personaje -> Receta -> Bool
tieneTodosLosIngredientes pj receta = all (\objInventario -> elem objInventario (ingredientes receta)) (inventario pj)

-- quitarMaterailes :: Personaje -> Receta -> [Material]
-- quitarMaterailes pj receta = filter (\material -> not (elem material (ingredientes receta))) (inventario pj)

quitarMateriales :: Personaje -> Receta -> [Material]
quitarMateriales pj receta = foldl quitarUnMaterial (inventario pj) (ingredientes receta)

quitarUnMaterial :: [Material] -> Material -> [Material]
quitarUnMaterial inventario matReceta = takeWhile (matReceta /=) inventario ++ drop (length (takeWhile (matReceta /=) inventario) + 1) inventario

-- Punto 2

queRecetasPuedeCraftearYDuplicanPuntaje :: Personaje -> [Receta] -> [Receta]
queRecetasPuedeCraftearYDuplicanPuntaje pj listaRecetas = filter (\r -> cumpleTodosLosRequisitos pj r) listaRecetas

cumpleTodosLosRequisitos :: Personaje -> Receta -> Bool
cumpleTodosLosRequisitos pj receta = ((2 *) . puntaje) pj <= puntaje (craftear pj receta) && tieneTodosLosIngredientes pj receta

queRecetasPuedeHacer :: Personaje -> [Receta] -> [Receta]
queRecetasPuedeHacer pj = filter (tieneTodosLosIngredientes pj)

craftearSucesivamente :: Personaje -> [Receta] -> Personaje
craftearSucesivamente pj listReceta = foldl craftear pj listReceta

mayorQueOrdenInverso :: Personaje -> [Receta] -> Bool
mayorQueOrdenInverso pj lista = puntaje (craftearSucesivamente pj lista) > puntaje (craftearSucesivamente pj (reverse lista))

-- Mine

data Bioma = UnBioma
  { elemNecesario :: Material,
    recursos :: [Material]
  }

hacha :: [Material] -> Material
hacha = last

espada :: [Material] -> Material
espada = head

pico :: Int -> [Material] -> Material
pico = flip (!!)

minar :: ([Material] -> Material) -> Personaje -> Bioma -> Personaje
minar herramienta pj bioma
  | elem (elemNecesario bioma) (inventario pj) = pj {inventario = herramienta (recursos bioma) : inventario pj, puntaje = puntaje pj + 50}
  | otherwise = pj

martillo :: [Material] -> Material --Elemento en la mitad de la lista
martillo lista = (!! div (length lista) 2) lista

taladro :: [Material] -> Material --Elementos mas largo nombre
taladro [] = []
taladro [x] = x
taladro (x:y:ys)
    | length x > length y = taladro (x:ys)
    | otherwise = taladro (y:ys)

materialCorrupto :: ([Material] -> Material) -> [Material] -> Material --Devuelve el material invertido que consiga una herramienta
materialCorrupto herramienta lista = herramienta (map (\material -> reverse material) lista)

-- En caso de contar con infinitos materiales, se podria minar algunos biomas dependiendo de la herramienta que se utilice y del personaje, 
-- por ejemplo el pico o la espada se podrian utilizar ya que gracias a la evalucion diferida se podria obtener el n elemento de la lista 
-- de recursos, sin embargo al tratar de utilizar el hacha se produciria un error ya que se intentaria obtener el ultimo elemento de la lista de 
-- recursos, lo cual no es posible. En cuanto el personaje se aplica lo mismo que antes siempre y cuando tenga el Material necesario 
-- en su invetario. En caso de no tenerlo no se evaluaria directamente la funcion y devolveria el mismo personaje.

-- Considerando las funciones que realice, no se puede aoplicar ninguna 
