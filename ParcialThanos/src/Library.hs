module Library where
import PdePreludat

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
} deriving (Show, Eq)

data Personaje = Personaje {
    edad :: Number,
    energia :: Number,
    nombre :: String,
    planeta :: String,
    habilidades :: [String]
} deriving (Show, Eq)

guantelete1 = Guantelete{
    material = "uru",
    gemas = [mente 10, alma "hola", espacio "hola", poder, tiempo]
}

guantelete2 = Guantelete{
    material = "uru",
    gemas = [mente 10, alma "hola", espacio "hola", tiempo, poder]
}

guantelete3 = Guantelete{
    material = "uru",
    gemas = [poder, mente 10, alma "hola", espacio "hola", tiempo]
}

guantelete5 = Guantelete{
    material = "uru",
    gemas = [mente 20, mente 10, mente 50]
}

ironMan :: Personaje
ironMan = Personaje{
    edad = 50,
    energia = 1000,
    nombre = "IronMan",
    planeta = "Tierra",
    habilidades = ["volar", "fuerza"]
}

drStrange :: Personaje
drStrange = Personaje{
    edad = 38,
    energia = 2000,
    nombre = "DrStrange",
    planeta = "Tierra",
    habilidades = ["controlar tiempo"]
}

groot :: Personaje
groot = Personaje{
    edad = 4000,
    energia = 500,
    nombre = "Groot",
    planeta = "SuperArbol",
    habilidades = ["regenerarse", "decir soy Groot", "ser un arbol"]

}

wolverine :: Personaje
wolverine = Personaje{
    edad = 54,
    energia = 800,
    nombre = "Wolverine",
    planeta = "Neptuno",
    habilidades = ["garras"]
}

blackWidow :: Personaje
blackWidow = Personaje{
    edad = 25,
    energia = 400,
    nombre = "BlackWidow",
    planeta = "Tierra",
    habilidades = ["tiro"]
}

type Universo = [Personaje]
universo1 :: [Personaje]
universo1 = [ironMan, drStrange, groot, wolverine]
universo2 :: [Personaje]
universo2 = [ironMan, drStrange, groot, wolverine, blackWidow]
universo3 :: [Personaje]
universo3 = [wolverine, groot]


--se puede crear una func auxiliar para q quede mas lindo
chasquido :: Universo -> Guantelete -> Universo
chasquido universo guante
 |((==6) . length .gemas) guante && ((=="uru") . material) guante = take (length universo `div` 2) universo
 |otherwise = universo

--Punto2

universoPendex :: Universo -> Bool
universoPendex = any ((<45) . edad)

masDeUnaHabilidad :: Universo -> Universo
masDeUnaHabilidad = filter ((>1) . length . habilidades)

energiaTotal :: Universo -> Number
energiaTotal universo = foldr ((+) . energia) 0 (masDeUnaHabilidad universo)

--Punto 3

type Gema = Personaje -> Personaje

modificarEnergia :: Number -> Gema
modificarEnergia nuevaEnergia personaje = personaje {energia = energia personaje - nuevaEnergia}

mente :: Number -> Gema
mente = modificarEnergia

existePoder :: String -> String -> Bool
existePoder poderAeliminar poder = poderAeliminar == poder

alma :: String -> Gema
alma poderaeliminar personaje = (modificarEnergia 10 personaje) {habilidades = filter (existePoder poderaeliminar) (habilidades personaje)}

espacio :: String -> Gema
espacio planetaAtransportar personaje = (modificarEnergia 20 personaje) {planeta = planetaAtransportar}

maximoDosHabilidades :: Personaje -> Bool
maximoDosHabilidades = (<=2) . length . habilidades 

poder :: Gema
poder personaje = (modificarEnergia (energia personaje) personaje) {habilidades = if maximoDosHabilidades personaje then [] else habilidades personaje}

tiempo :: Gema
tiempo personaje = (modificarEnergia 50 personaje) {edad = (edad personaje `div` 2) `max` 18 }

loca :: Gema -> Personaje -> Personaje --o Gema -> Gema
loca gema = gema . gema

--Punto 4

thor :: Personaje
thor = Personaje {
    edad = 30,
    energia = 2500,
    nombre = "Thor",
    planeta ="Tierra",
    habilidades = ["usar Mjolnir", "baile", "programacion en Haskell"]
}

guantelete4 :: Guantelete
guantelete4 = Guantelete {
    material = "goma",
    gemas = [tiempo, alma "usar Mjolnir", loca (alma "programacion en Haskell")]
}

--Punto 5

utilizar :: Personaje -> [Gema]-> Personaje
utilizar = foldr ($)

--Punto 6

gemass :: [Gema]
gemass = [tiempo, poder, alma "hola"]

perdidaDeEnergia :: Gema -> Personaje -> Number
perdidaDeEnergia gemaAplicante personaje = energia personaje - (energia . gemaAplicante) personaje 

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete = gemaMasPoderosa' (gemas guantelete)

gemaMasPoderosa' :: [Gema] -> Personaje -> Gema
gemaMasPoderosa' [gema] _ = gema
gemaMasPoderosa' (gema1:gemas) personaje
    | perdidaDeEnergia gema1 personaje >= perdidaDeEnergia (gemaMasPoderosa' gemas personaje) personaje = gema1
    | otherwise                                                                                         = gemaMasPoderosa' gemas personaje

--Otra forma mas linda y amena
gemaMasPoderosaP :: Personaje -> Guantelete -> Gema
gemaMasPoderosaP personaje guantelte = gemaMasPoderosaDe personaje $ gemas guantelte

gemaMasPoderosaDeP :: Personaje -> [Gema] -> Gema
gemaMasPoderosaDeP _ [gema] = gema
gemaMasPoderosaDeP personaje (gema1:gema2:gemas) 
    | (energia.gema1) personaje < (energia.gema2) personaje = gemaMasPoderosaDeP personaje (gema1:gemas)
    | otherwise = gemaMasPoderosaDeP personaje (gema2:gemas)


{- Punto 7 evaluación diferida -}
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" $ infinitasGemas tiempo

punisher:: Personaje 
punisher = Personaje "The Punisher" ["Disparar con de todo","golpear"] "Tierra" 38 350.0

usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (usar . take 3. gemas) guantelete

-- gemaMasPoderosa punisher guanteleteDeLocos
-- usoLasTresPrimerasGemas guanteleteDeLocos punisher