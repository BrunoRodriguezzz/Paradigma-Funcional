sueldo :: String -> Float -> Float -> Float
sueldo x y z = basicoCargo x * porcentajeIncremento y * fromIntegral (cantHoras z)

porcentajeIncremento :: Float -> Float
porcentajeIncremento x | x >= 24 = 2.2
                       | x >= 10 = 1.5
                       | x >= 5 = 1.3
                       | x >= 3 = 1.2
                       | otherwise = 1

cantHoras :: Float -> Int
cantHoras x | x <= 50 && x > 0 = round (x/10)
            | otherwise = 0
    --      | otherwise = error: "horas invalidas" 
            --en esta caso la funcion error interrumpe la ejecucion del programa mostrando el mensaje.
            --en esta materia no vamos a considerar que haya errores en el ingreso, la validacion se realiza UNA SOLA VEZ
            --y se realiza en otro parte del programa o en otro programa.

basicoCargo :: String -> Float
basicoCargo x | x == "titular" = 149000
              | x == "adjunto" = 116000
              | x == "ayudante" = 66000
              | otherwise = 0
              -- en este caso no deberia haber otherwise ya que es preferible el error a tratar de esconderlo
              -- las variables tendrian que ser mas descriptivas 
