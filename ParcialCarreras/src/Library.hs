module Library where
import PdePreludat

data Auto = Auto {
    color :: String,
    velocidad :: Number,
    distancia :: Number
} deriving (Show, Eq)

hilux :: Auto
hilux = Auto {
    color = "Gris",
    velocidad = 110,
    distancia = 20
}

cronos :: Auto
cronos = Auto {
    color = "Gris",
    velocidad = 60,
    distancia = 18
}

partner :: Auto
partner = Auto {
    color = "Rojo",
    velocidad = 80,
    distancia = 40
}

camaro :: Auto
camaro = Auto {
    color = "Azul",
    velocidad = 200,
    distancia = 50
}

escort :: Auto
escort = Auto {
    color = "Violeta",
    velocidad = 20,
    distancia = 25
}

type Carrera = [Auto]

distanciaEntre :: Auto -> Auto -> Number
distanciaEntre auto1 auto2 = abs (distancia auto1 - distancia auto2)

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 = (auto1 /= auto2) && distanciaEntre auto1 auto2 < 10

vaGanando :: Auto -> Auto -> Bool
vaGanando auto1 auto2 = distancia auto1 > distancia auto2

masDistanciaQueTodos :: Auto -> Carrera -> Bool
masDistanciaQueTodos auto1 = all (vaGanando auto1)

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo auto autos = all (estaCerca auto) autos && masDistanciaQueTodos auto autos

puestoActual :: Auto -> Carrera -> Number
puestoActual auto autos = length (filter (vaGanando auto) autos) + 1

--Punto 2
type CambioVelocidad = Number -> Number

correr :: Auto -> Number -> Auto
correr auto tiempo = auto {distancia = (distancia auto * tiempo) + distancia auto}

alteroVelocidad :: Auto -> CambioVelocidad
alteroVelocidad auto velocidadNueva = velocidad auto + velocidadNueva

bajoVelocidad :: Auto -> CambioVelocidad
bajoVelocidad auto velocidadNueva = 0 `max` alteroVelocidad auto (-velocidadNueva)

-- Punto 3
autos = [cronos, escort, camaro, partner]

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

terremoto :: Auto -> Carrera -> Carrera
terremoto auto = afectarALosQueCumplen (estaCerca auto) (\auto -> auto{velocidad = 50})

miguelitos :: Auto -> Number -> Carrera -> Carrera
miguelitos auto velocidad autos = afectarALosQueCumplen (not.vaGanando auto) (bajoVelocidad flip velocidad) autos

