module Library where
import PdePreludat

--Punto 1
data Pais = Pais {
    ingresoPerCapita :: Number,
    sectorPrivado :: Number,
    sectorPublico :: Number,
    recursosNaturales :: [String],
    deudaFMI :: Number
} deriving (Show)

namibia :: Pais
namibia = Pais {
    ingresoPerCapita = 4140,
    sectorPrivado = 650000,
    sectorPublico = 400000,
    recursosNaturales = ["mineria", "ecoturismo"],
    deudaFMI = 50000000
}

--Punto 2
type Estrategia = Pais -> Pais

modificarDeuda :: Number -> Pais -> Pais
modificarDeuda deudaNueva pais = pais{deudaFMI = deudaFMI pais + deudaNueva}
--
prestarPlata :: Number -> Estrategia
prestarPlata nuevaDeuda = modificarDeuda (nuevaDeuda * 1.5)
--
reduccionIPC :: Number -> Number -> Number
reduccionIPC puestos ipc = if puestos > 100 then ipc * 0.8 else ipc * 0.85

reducirSectorPublico :: Number -> Estrategia
reducirSectorPublico puestos pais = pais {sectorPublico = sectorPublico pais - puestos,
                                          ingresoPerCapita = reduccionIPC puestos (ingresoPerCapita pais)
}
--
existeRecurso :: String -> [String] -> [String]
existeRecurso recursoNatural = filter (/=recursoNatural)

autorizoExplotacion :: String -> Estrategia
autorizoExplotacion recursoNatural pais = (modificarDeuda (-2000000) pais) {recursosNaturales = existeRecurso recursoNatural (recursosNaturales pais)}
--
calculoPBI :: Pais -> Number
calculoPBI pais = (sectorPublico pais + sectorPrivado pais) * ingresoPerCapita pais

blindaje :: Estrategia
blindaje pais = reducirSectorPublico 500 (modificarDeuda (calculoPBI pais) pais)

--Punto 3
type Receta = [Estrategia]
recetaA :: Receta
recetaA = [prestarPlata 200000000, autorizoExplotacion "mineria"]

aplicarReceta :: Receta -> Pais -> Pais
aplicarReceta receta pais = foldl (flip ($)) pais receta

--Punto 4
puedenZafar :: [Pais] -> [Pais]
puedenZafar = filter (("petroleo" `elem`) . recursosNaturales)

deudaAfavorFMI :: [Pais] -> Number
deudaAfavorFMI = foldr ((+) . deudaFMI) 0 

--Punto 5

recetaB :: Receta
recetaB = [reducirSectorPublico 400000, blindaje]

recetaC :: Receta
recetaC = [blindaje]

recetaOrdenada :: [Receta]
recetaOrdenada = [recetaB, recetaC, recetaA]

recetaSinOrdenar :: [Receta]
recetaSinOrdenar = [recetaA, recetaB, recetaC]

recetaUnElemento :: [Receta]
recetaUnElemento = [recetaA]

recetaVacia :: [Receta]
recetaVacia = []

recetasOrdenadas :: Pais -> [Receta] -> Bool
recetasOrdenadas _ [] = True --si ejecuto recetasOrdenadas namibia recetaVacia, da error
recetasOrdenadas _ [receta] = True
recetasOrdenadas pais (receta1:receta2:recetas) 
 |(calculoPBI . aplicarReceta receta1) pais < (calculoPBI . aplicarReceta receta2) pais = recetasOrdenadas pais (receta2:recetas) && True
 |otherwise                                                                             = False

--Punto 6
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos

{-a. Al no haber ninguna condicion que haga cortar la evaluacion continua de la lista de recursos naturales infinitos,
la funcion no diverge en un resultado y se quedará iterando infinitamente
b. La funcion deudaAfavorFMI, al no estar evaluando los recursos naturales infinitos que posee el pais,
va a iterar dos veces sumando la deuda de namibia y de Pais. Se lo puede relacionar con el concepto de 
Lazy Evaluation que posee Haskell, donde las funciones solo toman la informacion que necesitan.-}