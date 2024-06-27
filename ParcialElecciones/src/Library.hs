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
    estrategia = esperanzado True
}

victor :: Persona
victor = Persona{
    nombre = "Victor",
    deuda = 150,
    porcentajeFelicidad = 80,
    tieneEsperanza = True,
    estrategia = economico 100
}

manuel :: Persona
manuel = Persona{
    nombre = "Manuel",
    deuda = 500,
    porcentajeFelicidad = 25,
    tieneEsperanza = False,
    estrategia = combinado
}

type Pais = [Persona]

pais :: Pais
pais = [silvia, lara, manuel, victor]

paisSinVocal :: Pais
paisSinVocal = [lara, victor]

--Punto2

isVowel :: Char -> Bool
isVowel character = character `elem` "aeiou"

masDeDosVocalesLista :: Pais -> Bool
masDeDosVocalesLista = any ((>2) . masDeDosVocalesPalabra . nombre)

masDeDosVocalesPalabra :: String -> Number
masDeDosVocalesPalabra palabra = length (filter isVowel palabra)

sumaDeudasPares :: Pais -> Number
sumaDeudasPares pais = foldr ((+).deuda) 0 (filter (even . deuda) pais)

--Punto 3
type Candidato = Persona -> Persona

yrigoyen :: Candidato
yrigoyen persona = persona {deuda = deuda persona `div` 2}

alende :: Candidato
alende persona = persona {tieneEsperanza = True,
                          porcentajeFelicidad = porcentajeFelicidad persona + 10}

alsogaray :: Candidato
alsogaray persona = persona {tieneEsperanza = False,
                             deuda = deuda persona + 100}

mRaymonda :: Candidato
mRaymonda = alende . yrigoyen

candidatos :: [Candidato]
candidatos = [yrigoyen, alende, alsogaray, mRaymonda]

--Punto 4

cumpleCandidato :: Persona -> [Candidato] -> [Candidato]
cumpleCandidato _ [] = []
cumpleCandidato persona (candidato:candidatos) 
 |estrategia persona (candidato persona) = candidato:cumpleCandidato persona candidatos 
 |otherwise                              = []

--aca, primero aplico los cambios que hace el candidato en una persona y luego comparo si dicha persona modificada cumple
--con las condiciones de su estrategia, si las cumple, agrega el candidato a la lista que va a devolver y sigue iterando
--la lista, hasta llegar a uno que no satisfaga su estrategia, y corta la lista y devuelve los candidatos que si

--Punto 5

aplicarCandidatos :: Persona -> [Candidato] -> Persona
aplicarCandidatos = foldr ($)

--aplicarCandidatos persona (cumpleCandidato persona candidatos)
{-Si le pasamos una lista infinita de candidatos, se aplicaran todos aquellos candidatos
que la persona acepte hasta llegar a uno que no, donde convergerá y mostrará el estado
final de la persona. Esto es posible gracias a la caracteristica de Lazy Evaluation, donde
solo evaluará aquello que necesita (en este caso unicamente los elementos aplicables
a la persona) y no aquellos que no necesita (la lista completa de candidatos infinitos).-}