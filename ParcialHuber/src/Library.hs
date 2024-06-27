module Library where
import PdePreludat

data Chofer = Chofer{
    nombreChofer :: String,
    kilometrajeAuto :: Number,
    viajesTomados :: [Viaje],
    condicion :: Condicion
}
data Viaje = Viaje{
    fecha :: (Number, Number, Number),
    cliente :: Cliente,
    costo :: Number
}

data Cliente = Cliente{
    nombreCliente :: String,
    localidad :: String
}

--Punto 2


viaje1 :: Viaje
viaje1 = Viaje {
    fecha = (19, 06, 2024),
    cliente = juan,
    costo = 400
}

juan :: Cliente
juan = Cliente {
    nombreCliente = "Juan",
    localidad = "Catan"
}

type Condicion = Viaje -> Bool

cualquierViaje :: Condicion
cualquierViaje viaje = True

mayorA200 :: Condicion
mayorA200  = (>200) . costo

nombreMasDeNletras :: Number -> Condicion
nombreMasDeNletras letrasMinimas = (>letrasMinimas) . length . nombreCliente . cliente

zonaNoViaja :: String -> Condicion
zonaNoViaja zona = (/=zona) . localidad . cliente

--Punto 3
lucas :: Cliente
lucas = Cliente {
    nombreCliente = "Lucas",
    localidad = "Victoria"
}

daniel :: Chofer
daniel = Chofer {
    nombreChofer = "Daniel",
    kilometrajeAuto = 23500,
    viajesTomados = [viaje2],
    condicion = zonaNoViaja "Olivos"
}

viaje2 :: Viaje
viaje2 = Viaje {
    fecha = (20, 04, 2017),
    cliente = lucas,
    costo = 150
}

alejandra :: Chofer
alejandra = Chofer {
    nombreChofer = "Alejandra",
    kilometrajeAuto = 180000,
    viajesTomados = [],
    condicion = cualquierViaje
}

--Punto 4

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = (condicion chofer)viaje

--Punto 5

liquidacionChofer :: Chofer -> Number
liquidacionChofer chofer = foldr ((+) . costo) 0 (viajesTomados chofer)

--Punto 6

choferesQueTomanViaje :: Viaje -> [Chofer] -> [Chofer]
choferesQueTomanViaje viaje = filter (puedeTomarViaje viaje) 

cantidadViajes :: Chofer -> Number
cantidadViajes = length . viajesTomados

choferConMenosViajes' :: Chofer -> Chofer -> Chofer
choferConMenosViajes' chofer1 chofer2
 |cantidadViajes chofer1 > cantidadViajes chofer2 = chofer2
 |otherwise = chofer1

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes [chofer] = chofer
choferConMenosViajes (chofer1:chofer2:choferes) = choferConMenosViajes (choferConMenosViajes' chofer1 chofer2:choferes)

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje chofer = chofer{viajesTomados = viajesTomados chofer ++ [viaje]}

--Punto 7
viajeInfinito :: Viaje
viajeInfinito = Viaje {
    fecha = (11, 03, 2017),
    cliente = lucas,
    costo = 50
}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

nitoinfy :: Chofer
nitoinfy = Chofer{
    nombreChofer = "Nito Infy",
    kilometrajeAuto = 70000,
    viajesTomados = repetirViaje viajeInfinito,
    condicion = nombreMasDeNletras 3 
}

{-b. No puedo calcular la liquidacion de Nito ya que no estoy aplicando ninguna funcion
haga que el resultado pueda converger
c. Si, tranquilamente se puede hacer ya que no estaria involucrando
la lista infinita de viajes que modelé

8. gongNeng :: Ord c => c -> (c -> Bool) -> (a -> c) -> [a] -> c
   gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3   
-}

