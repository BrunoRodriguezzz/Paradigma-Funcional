--Misma cantidad de empleados 
--Si el nombre es Acme = son 10 empleados
--Si la primer letra es mayor que la ultima = la cantidad de las letras en el medio 

cantEmpleados:: String -> Int
cantEmpleados x | x == "Acme" = 10
                | last x < head x = length x - 2
                | x == reverse x = (length x - 2)*2
                | mod (length x) 3 == 0 || mod (length x) 7 == 0 = 3
                | otherwise = 0