module Library where
import PdePreludat

--Punto 1
data Turista = Turista {
  cansancio :: Number,
  estres :: Number,
  viajaSolo :: Bool,
  idiomas :: [String]
}

ana :: Turista
ana = Turista {
  cansancio = 0,
  estres = 21,
  viajaSolo = False,
  idiomas = ["español"]
}

beto :: Turista
beto = Turista {
  cansancio = 15,
  estres = 15,
  viajaSolo = True,
  idiomas = ["aleman"]
}

cathi :: Turista
cathi = Turista {
  cansancio = 15,
  estres = 15,
  viajaSolo = True,
  idiomas = ["aleman", "catalan"]
}

--Punto2 a.

type Excursion = Turista -> Turista

modificoCansancio :: Number -> Excursion
modificoCansancio cansancioAdicional turista = turista {cansancio = cansancio turista + cansancioAdicional}

modificoEstres :: Number -> Excursion
modificoEstres estresAdicional turista = turista {estres = estres turista + estresAdicional}

reduccionEstres :: Excursion
reduccionEstres turista = turista {estres = estres turista * 0.9}

--

irAlaPlaya :: Excursion
irAlaPlaya turista
 |viajaSolo turista = reduccionEstres (modificoCansancio (-5) turista)
 |otherwise         = reduccionEstres (modificoEstres (-1 ) turista) 

apreciarPaisaje :: String -> Excursion
apreciarPaisaje elemento turista = reduccionEstres (modificoEstres (-length elemento) turista)

hablarIdioma :: String -> Excursion
hablarIdioma idioma turista = reduccionEstres turista {idiomas = idiomas turista ++ [idioma],
                                                       viajaSolo = False}

intensidad :: Number -> Number
intensidad minutos = minutos `div` 4

caminar ::  Number -> Excursion
caminar minutos turista = reduccionEstres (modificoEstres (-intensidad minutos) (modificoCansancio (intensidad minutos) turista))

mareaFuerte :: Excursion
mareaFuerte turista = reduccionEstres (modificoEstres 6 (modificoCansancio 10 turista))

mareaModerada :: Excursion
mareaModerada  = reduccionEstres 

mareaTranquila :: Excursion
mareaTranquila turista = reduccionEstres ((hablarIdioma "aleman" . apreciarPaisaje "mar" . caminar 10) turista)

data Marea = Fuerte | Moderada | Tranquila

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte turista    = modificoEstres 6 turista{cansancio = cansancio turista + 10}
paseoEnBarco Moderada turista  = turista
paseoEnBarco Tranquila turista = (hablarIdioma "aleman" . apreciarPaisaje "mar" . caminar 10) turista

--b.

deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2

deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (excursion turista) turista 

--c.

esEducativa :: Turista -> Excursion -> Bool
esEducativa turista excursion = (length . idiomas) turista < (length . idiomas . excursion)turista

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = (estres . excursion)turista + 3 <= estres turista

sonDesestresantes :: Turista -> Tour -> [Excursion]
sonDesestresantes _ [] = []
sonDesestresantes turista (excursion:excursiones)
 |esDesestresante turista excursion = excursion:sonDesestresantes turista excursiones
 |otherwise                         = sonDesestresantes turista excursiones

--Punto 3

type Tour = [Excursion]

tourCompleto :: Tour
tourCompleto = [caminar 20, irAlaPlaya, apreciarPaisaje "cascada", caminar 40, hablarIdioma "melmacquiano"]

ladoB :: Excursion -> Tour
ladoB excursion = [paseoEnBarco Tranquila, excursion, caminar 120]

islaVecina :: Marea -> Tour
islaVecina Fuerte    = [paseoEnBarco Fuerte, apreciarPaisaje "lago", paseoEnBarco Fuerte]
islaVecina otraMarea = [paseoEnBarco otraMarea, irAlaPlaya, paseoEnBarco otraMarea]

--a.

hacerTour :: Turista -> Tour -> Turista
hacerTour turista tour = foldl (flip($)) (modificoEstres (length tour) turista) tour

--b.
dejaAcompañado :: Turista -> Excursion -> Bool
dejaAcompañado turista excursion = (not . viajaSolo . excursion)turista

tourConvincente :: Turista -> Tour -> Bool
tourConvincente turista tour = any (dejaAcompañado turista) (sonDesestresantes turista tour)

--c.
perdidaEstres :: Turista -> Tour -> Number
perdidaEstres turista tour = estres turista - estres (hacerTour turista tour)

perdidaCansancio :: Turista -> Tour -> Number
perdidaCansancio turista tour = cansancio turista - cansancio (hacerTour turista tour)

espiritualidadTurista :: Turista -> Tour -> Number
espiritualidadTurista turista tour
 |tourConvincente turista tour = perdidaCansancio turista tour + perdidaEstres turista tour

efectividadTourGeneral :: [Turista] -> Tour -> Number
efectividadTourGeneral turistas tour = sumOf (flip espiritualidadTurista tour) turistas
