sueldo :: String -> Float -> Float -> Float
sueldo x y z = basicoPorCargo x * antiguedad y * fromIntegral (cantidadHoras z)

basicoPorCargo :: String -> Float
basicoPorCargo x
  | x == "titular" = 149000
  | x == "adjunto" = 116000
  | x == "ayudante" = 66000
  | otherwise = 0

antiguedad :: Float -> Float
antiguedad x
  | x >= 24 = 2.2
  | x >= 10 = 1.5
  | x >= 5 = 1.3
  | x >= 3 = 1.2
  | otherwise = 1

cantidadHoras :: Float -> Int
cantidadHoras x
  | x < 50 && x > 0 = round (x / 10)
  | otherwise = 0