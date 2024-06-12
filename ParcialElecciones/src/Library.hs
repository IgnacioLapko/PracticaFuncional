module Library where
import PdePreludat

data Persona = Persona {
    nombre :: String,
    deuda :: Number,
    porcentajeFelicidad :: Number,
    tieneEsperanza :: Bool,
    estrategia :: CriterioEleccion
} deriving (Show, Eq)

type CriterioEleccion = Persona -> Bool

conformista :: CriterioEleccion
conformista persona = True

esperanzado :: Bool -> CriterioEleccion
esperanzado estadoEsperanza persona = porcentajeFelicidad persona > 50 || estadoEsperanza == tieneEsperanza persona

economico :: Number -> CriterioEleccion
economico valorMaximo persona = deuda persona < valorMaximo

combinado :: CriterioEleccion
combinado persona = esperanzado True persona || economico 500 persona

silvia :: Persona
silvia = Persona{
    nombre = "Silvia",
    deuda = 100,
    porcentajeFelicidad = 50,
    tieneEsperanza = False,
    estrategia = conformista
}

lara :: Persona
lara = Persona{
    nombre = "Lara",
    deuda = 800,
    porcentajeFelicidad = 80,
    tieneEsperanza = True,
    estrategia = conformista
}

manuel :: Persona
manuel = Persona{
    nombre = "Manuel",
    deuda = 51,
    porcentajeFelicidad = 30,
    tieneEsperanza = False,
    estrategia = conformista
}

victor :: Persona
victor = Persona{
    nombre = "Victor",
    deuda = 5,
    porcentajeFelicidad = 80,
    tieneEsperanza = True,
    estrategia = combinado
}

type Pais = [Persona]

pais :: Pais
pais = [silvia, lara, manuel, victor]

--Punto2

isVowel :: Char -> Bool
isVowel character = character `elem` "aeiou"

(\nombre -> foldr ((+).isVowel) True nombre)

masDeDosVocales :: Pais -> Bool
masDeDosVocales pais = foldr ((\nombre -> foldr ((+).isVowel) True nombre)) False pais

sumaDeudasPares :: Pais -> Number
sumaDeudasPares pais = foldr ((+).deuda) 0 (filter (even . deuda) pais)

