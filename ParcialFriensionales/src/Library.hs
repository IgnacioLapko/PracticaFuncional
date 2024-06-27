module Library where
import PdePreludat

data Persona = Persona {
    nombre :: String,
    edad :: Number,
    nivelDeAlegria :: Number,
    nivelDeAnsiedad :: Number,
    tareas :: [Tarea]
} deriving (Show)

juan :: Persona
juan = Persona {
    nombre = "Juan",
    edad = 33,
    nivelDeAlegria = 70,
    nivelDeAnsiedad = 100,
    tareas = [codearUnProyectoNuevo, hacerTramitesEnAfip 3, andarEnBici 6, escucharMusica, codearUnProyectoNuevo, hacerTramitesEnAfip 2]
}

cecilia :: Persona
cecilia = Persona {
    nombre = "Cecilia",
    edad = 50,
    nivelDeAlegria = 200,
    nivelDeAnsiedad = 40,
    tareas = [codearUnProyectoNuevo]    
}

richard :: Persona
richard = Persona {
    nombre = "Richard",
    edad = 55,
    nivelDeAlegria = 12,
    nivelDeAnsiedad = 10,
    tareas = [codearUnProyectoNuevo]    
}

messi :: Persona
messi = Persona {
    nombre = "Messi",
    edad = 37,
    nivelDeAlegria = 250,
    nivelDeAnsiedad = 30,
    tareas = [codearUnProyectoNuevo]    
}

nivelEstres :: Persona -> Number
nivelEstres persona 
 |((>5) . length . tareas) persona = nivelDeAnsiedad persona * 1.5
 |otherwise                        = nivelDeAnsiedad persona

nivelDeEnergia :: Persona -> Number
nivelDeEnergia persona
 |nivelDeAlegria persona > nivelDeAnsiedad persona = (nivelDeAlegria persona * 2) `min` 340
 |nivelDeAlegria persona < nivelDeAnsiedad persona && edad persona < 40 = 300 - nivelEstres persona
 |otherwise = nivelDeAlegria persona + 10

--Punto 2

grupo :: [Persona]
grupo = [juan, cecilia, richard, messi]

jovato :: Persona -> Bool
jovato persona = edad persona >= 40

cuantoDueleVerLasBuenas :: [Persona] -> [Persona]
cuantoDueleVerLasBuenas grupo = filter ((>40) . nivelDeEnergia) (filter jovato grupo)

nivelTotalDeAnsiedad :: [Persona] -> Number
nivelTotalDeAnsiedad grupo = foldr ((+).nivelDeAnsiedad) 0 (filter jovato grupo)

nivelDeAnsiedadMayorA :: [Persona] -> [Persona]
nivelDeAnsiedadMayorA  = filter ((>30) . nivelDeAnsiedad)

nivelDeEnergiaPar :: [Persona] -> [Persona]
nivelDeEnergiaPar = filter (even . nivelDeEnergia)

losMasCriticados :: [Persona] -> [Persona]
losMasCriticados = take 2

{-
>losMasCriticados (nivelDeAnsiedadMayorA grupo)
>losMasCriticados (nivelDeEnergiaPar grupo)
-}

--Punto 3 (no repetir ideas)

type Tarea = Persona -> Persona

modificarAlegria :: Number -> Number -> Number
modificarAlegria parametro valor = parametro + valor

modificarAnsiedad :: Number -> Number -> Number
modificarAnsiedad parametro valor = modificarAlegria parametro valor - 10

codearUnProyectoNuevo :: Tarea
codearUnProyectoNuevo persona = persona {nivelDeAlegria = modificarAlegria (nivelDeAlegria persona) 110,
                                         nivelDeAnsiedad = modificarAnsiedad (nivelDeAlegria persona) 50}

hacerTramitesEnAfip :: Number -> Tarea
hacerTramitesEnAfip cantidadTramites persona = persona {nivelDeAnsiedad = (nivelDeAnsiedad persona * cantidadTramites) - 10 `max` 300}

andarEnBici :: Number -> Tarea
andarEnBici kilometros persona = persona {nivelDeAlegria = modificarAlegria (nivelDeAlegria persona) (kilometros * 50),
                                          nivelDeAnsiedad = (modificarAnsiedad (nivelDeAnsiedad persona) (-nivelDeAnsiedad persona) -10) `max` 0}

escucharMusica :: Tarea
escucharMusica persona = persona {nivelDeAnsiedad = nivelDeAnsiedad persona - 10}

--Punto 4
actividades :: [Tarea]
actividades = [codearUnProyectoNuevo, hacerTramitesEnAfip 3, andarEnBici 6, escucharMusica]

energiaResultante :: Persona -> Number
energiaResultante persona  = nivelDeEnergia (foldr ($) persona (tareas persona))

--Punto 5

hiceLoQuePude :: Persona -> [Tarea] -> Bool
hiceLoQuePude _ [] = True
hiceLoQuePude persona (tarea1:tareas) 
 |((>100) . nivelDeEnergia . tarea1) persona = hiceLoQuePude persona tareas 
 |otherwise = False

