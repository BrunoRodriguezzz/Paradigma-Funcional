-- Punto 1: Valor de una ciudad
-- Valor de una ciudad 
-- Definir el valor de una ciudad, un número que se obtiene de la siguiente manera:
-- si fue fundada antes de 1800, su valor es 5 veces la diferencia entre 1800 y el año de fundación
-- si no tiene atracciones, su valor es el doble del costo de vida
-- de lo contrario, será 3 veces el costo de vida de la ciudad

castelar :: Ciudad

castelar = UnaCiudad{
    nombre = "Castelar",
    anioFundacion = 1900,
    atracciones = [],
    costoDeVida = 2
}

buenosAires::Ciudad
buenosAires = UnaCiudad {
    nombre = "Buenos Aires",
    anioFundacion = 1700,
    atracciones = ["Obelisco","Abasto","Estatua de la Libertad"],
    costoDeVida = 3
}

data Ciudad = UnaCiudad {
    nombre :: String,
    anioFundacion :: Int,
    atracciones :: [String],
    costoDeVida :: Int
} deriving (Show)

valorCiudad :: Ciudad -> Int
valorCiudad (UnaCiudad _ anioFundacion atracciones costoDeVida) 
    | anioFundacion < 1800 = 5*(1800 - anioFundacion)
    | length atracciones == 0 = 2 * costoDeVida
    | otherwise = 3 * costoDeVida

--Punto 2: Características de las ciudades
--Alguna atracción copada
--Queremos saber si una ciudad tiene alguna atracción copada, esto es que la atracción comience con una vocal. 
--Por ejemplo: "Acrópolis" es una atracción copada y "Golden Gate" no es copada.

atracciones4 = ["Hola", "Pepe", "Guatemala", "España"]
atracciones2 = ["Hola", "Pepe"]

atraccionCopada :: Ciudad -> Bool
atraccionCopada (UnaCiudad _ _ atracciones _) = algunaEmpiezaConVocal atracciones

algunaEmpiezaConVocal :: [String] -> Bool
algunaEmpiezaConVocal listaAtracciones =  any empiezaConVocal listaAtracciones

empiezaConVocal :: String -> Bool
empiezaConVocal palabra = elem (head palabra) "AEIOUÁÉÍÓÚ"

-- Ciudad sobria
-- Queremos saber si una ciudad es sobria, esto se da si todas las atracciones tienen más de x letras.
-- El valor x tiene que poder configurarse

esCiudadSobria :: Ciudad -> Int -> Bool
esCiudadSobria (UnaCiudad _ _ atracciones _ ) letras = all (mayorA letras) atracciones 

mayorA :: Int -> String -> Bool
mayorA letras palabra = length palabra > letras

-- Ciudad con nombre raro
-- Queremos saber si una ciudad tiene un nombre raro, esto implica que tiene menos de 5 letras en su nombre.
