pesoPino :: Float -> Float
pesoPino x | x <= 3 = x*100*3
           | otherwise = 900 + (x-3)*100*2

esPesoUtil :: Float -> Bool
esPesoUtil x = 400 <= x && x <= 1000

esPinoUtil :: Float -> Bool
esPinoUtil = esPesoUtil . pesoPino 
