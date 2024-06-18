module Library where
import PdePreludat

data Animal = Animal{
    nombre :: String,
    tipo :: String,
    peso :: Number,
    edad :: Number,
    enfermo :: Bool,
    visitasMedicas :: [VisitaMedica]
} deriving(Show, Eq)

type DiasDeRecuperacion = Number
type Costo = Number
type VisitaMedica = (DiasDeRecuperacion, Costo)

dorothy :: Animal 
dorothy = Animal{
    nombre = "Dorothy",
    tipo = "Vaca",
    peso = 690,
    edad = 10,
    enfermo = False,
    visitasMedicas = [(50, 100), (20, 30)]
}

gachi :: Animal 
gachi = Animal{
    nombre = "Gachi",
    tipo = "Vaca",
    peso = 690,
    edad = 10,
    enfermo = False,
    visitasMedicas = [(10, 15), (20, 30)]
}

pachi :: Animal 
pachi = Animal{
    nombre = "Pachi",
    tipo = "Vaca",
    peso = 690,
    edad = 10,
    enfermo = False,
    visitasMedicas = [(50, 100), (20, 30)]
}

juan :: Animal 
juan = Animal{
    nombre = "Juan",
    tipo = "Vaca",
    peso = 690,
    edad = 10,
    enfermo = False,
    visitasMedicas = [(50, 100), (20, 30)]
}

messi :: Animal 
messi = Animal{
    nombre = "Messi",
    tipo = "Vaca",
    peso = 690,
    edad = 10,
    enfermo = False,
    visitasMedicas = [(50, 100), (20, 30)]
}

fideini :: Animal 
fideini = Animal{
    nombre = "Fideini",
    tipo = "Vaca",
    peso = 690,
    edad = 10,
    enfermo = False,
    visitasMedicas = [(50, 100), (20, 30)]
}

animales :: [Animal]
animales = [dorothy, gachi, pachi, juan, messi, fideini]

type KilosDeAlimento = Number

type PesoMinimo = Number

type Actividad = Animal -> Animal
type Proceso = [Actividad]

procesoRechazo :: Proceso
procesoRechazo = [chequeoDePeso 800, festejoCumple]

procesoRechazo2 :: Proceso
procesoRechazo2 = [engorde 20, chequeoDePeso 900]

procesoRechazo3 :: Proceso
procesoRechazo3 = [engorde 4, chequeoDePeso 900, festejoCumple]

procesoRechazo4 :: Proceso
procesoRechazo4 = [engorde 10, chequeoDePeso 900, festejoCumple]

procesoAcepta :: Proceso
procesoAcepta = [chequeoDePeso 900, engorde 4]

pasoMal :: Animal -> Bool
pasoMal animal = foldr ((||) . (>30) . fst) False (visitasMedicas animal)

nombreFalopa :: Animal -> Bool
nombreFalopa = (=='i') . last . nombre

engorde :: KilosDeAlimento -> Animal -> Animal
engorde kilosDeAlimento animal 
    |kilosDeAlimento<=5 = animal {peso = ((+(kilosDeAlimento/2)) . peso) animal}
    |otherwise         = animal {peso = ((+5) . peso) animal}

revisacion :: Animal -> VisitaMedica -> Animal
revisacion animal datosVisita 
 |enfermo animal = animal {peso = (peso . engorde 2) animal, 
                           visitasMedicas = visitasMedicas animal ++ [datosVisita]}

festejoCumple :: Animal -> Animal
festejoCumple animal = animal {peso = peso animal - 1,
                               edad = ((+1) . edad) animal}

chequeoDePeso :: PesoMinimo -> Animal -> Animal
chequeoDePeso pesoMinimo animal
    |((<pesoMinimo).peso) animal = animal {enfermo = True}
    |otherwise                   = animal {enfermo = False}

--(engorde 4 . festejoCumple . chequeoDePeso 800)dorothy

mejoraPeso :: Animal -> Proceso -> Bool
mejoraPeso animal [actividad] = peso animal <= (peso . actividad) animal && peso animal >= (peso . actividad) animal - 3
mejoraPeso animal (actividad1:actividades)
    |peso animal <= (peso . actividad1) animal && peso animal >= (peso . actividad1) animal - 3 = mejoraPeso animal actividades
    |otherwise                                                                                  = False

animalesConNombreFalopa :: [Animal] -> [Animal]
animalesConNombreFalopa animales = take 3 (filter nombreFalopa animales)
--hola
{-Si le pasaramos una cantidad infinita de animales, Haskell podria ejecutar la funcion correctamente,
ya que por Lazy Evaluation, una vez tomados los primero 3 animales que cumplen la condicion, la funcion 
devolveria dichos 3 animales sin la necesidad de continuar avanzando con la lista.-}

