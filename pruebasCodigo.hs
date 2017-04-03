esMultiploDe numero numero2 = mod numero numero2 == 0
esBisiesto año = (esMultiploDe año 400) || ((esMultiploDe año 4) && not (esMultiploDe año 100))
