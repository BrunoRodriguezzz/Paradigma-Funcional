import Data.Char (isLower)
type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

-- Saber la calificacion de un canción, que equivale a la cantidad de letras minúsculas (sin espacios, números ni caracteres especiales) 
-- de la canción, más 10.

calificacionCancion :: Cancion -> Int
calificacionCancion cancion = length (filter (/= ' ') (filter isLower cancion)) + 10

-- Averiguar si es exitoso un artista, lo que sucede cuando la suma de las calificaciones buenas de las canciones de un artista es mayor 
-- a 50 (son buenas las que tienen calificacion mayor a 20)

artistaExitoso :: Artista -> Bool
-- artistaExitoso artista = sum (filter (>20) (map calificacionCancion (canciones artista))) > 50
artistaExitoso = (>50) . sum . filter (>20) . map calificacionCancion . canciones

-- 3) Obtener todos los artistas exitosos, a partir de un conjunto de artistas

artistasExitosos :: [Artista] -> [Artista]
artistasExitosos = filter artistaExitoso

-- 4) Hacer todo lo anterior en una función definida en una sola línea, sin definir funciones auxiliares

todoEnUn1 :: [Artista] -> [Artista]
todoEnUn1 = filter (\artista -> (>50) . sum . filter (>20) . map (\cancion -> length (filter (/= ' ') (filter isLower cancion)) + 10) . canciones $ artista)
