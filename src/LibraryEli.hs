module LibraryEli where
import PdePreludat
import Data.List (find)

-- ------------------------- Dominio ---------------------------
data Serie = UnaSerie {
    nombreSerie :: Nombre,
    actores :: [Actor],
    presupuestoAnual :: Presupuesto, 
    cantTemps :: Number,
    rating :: Rating,
    cancelada :: Bool
} deriving (Show, Eq)

data Actor = UnActor {
    nombreActor :: Nombre,
    sueldo :: Sueldo,
    restricciones :: [Restriccion]
} deriving (Show, Eq)

-- ------------------------- Definición de Tipos ---------------------------
type Nombre = String
type Presupuesto = Number
type Rating = Number
type Importancia = Number
type Sueldo = Number
type Restriccion = String
type Bienestar = Number

type Productor = Serie -> Serie

-- ------------------------- Ejemplos ---------------------------

paulRudd :: Actor
paulRudd = UnActor "Paul Rudd" 41000000 ["No actuar en bata", "Comer ensalada de rucula todos los dias"]

johnnyDepp :: Actor
johnnyDepp = UnActor "Johnny Depp" 20000000 []

helenaBonham :: Actor
helenaBonham = UnActor "Helena Bonham" 15000000 []

timBurton :: Productor
timBurton = conFavoritismos [johnnyDepp, helenaBonham]

-- ------------------------- Funciones ---------------------------
-- --------------- Punto 1
-- Funcion A
estaEnRojo :: Serie -> Bool
estaEnRojo serie = presupuestoAnual serie < sumaSueldoTotal serie

sumaSueldoTotal :: Serie -> Number
sumaSueldoTotal = sum . map sueldo . actores 

-- conseguirActores :: Serie -> [Actor]
-- conseguirActores serie = map fst $ actores serie

-- Funcion B
esProblematica :: Serie -> Bool
esProblematica serie = actoresConRestric serie && menosDeXActores 3 serie
    
actoresConRestric :: Serie -> Bool
actoresConRestric serie = any (actorTieneRestric) (actores serie)

actorTieneRestric :: Actor -> Bool
actorTieneRestric = not . null . restricciones

menosDeXActores :: Number -> Serie -> Bool
menosDeXActores delta = (>delta) . length . actores 

-- --------------- Punto 2
-- Funcion A
conFavoritismos :: [Actor] -> Productor
conFavoritismos actoresFav serie = serie{ 
    actores = actoresFav ++ drop 2 (actores serie)
}

-- Funcion B (Arriba)

-- Funcion C
gatopardeitor :: Productor
gatopardeitor = id

-- Funcion D
estireitor :: Productor
estireitor serie = serie{
    cantTemps = cantTemps serie * 2 
}

-- Funcion E
desespereitor :: [Productor] -> Productor
desespereitor ideasProductor serie 
    | ((>=2) . length) ideasProductor = foldl aplicarProd serie ideasProductor
    | otherwise = serie

aplicarProd :: Serie -> Productor -> Serie
aplicarProd serie productor = productor serie

-- Funcion F

canceleitor :: Number -> Productor
canceleitor delta serie 
    | estaEnRojo serie || rating serie < delta = cancelarSerie serie
    | otherwise = serie

cancelarSerie :: Serie -> Serie
cancelarSerie serie = serie{cancelada = True}

-- --------------- Punto 3
bienestarSerie :: Serie -> Bienestar
bienestarSerie serie 
    | cancelada serie = 0 
    | otherwise = mas4Temps serie + menos10Actores serie 

mas4Temps :: Serie -> Number
mas4Temps serie
    | cantTemps serie > 4 = 5
    | otherwise = (10 - cantTemps serie) * 2

menos10Actores :: Serie -> Number
menos10Actores serie 
    | menosDeXActores 10 serie = 3
    | otherwise = 10 - minResta serie

minResta :: Serie -> Number
minResta serie = max 2 (cantActoresRestric serie)

cantActoresRestric :: Serie -> Number
cantActoresRestric = length . filter actorTieneRestric . actores

-- --------------- Punto 4
seriesEfectivas :: [Serie] -> [Productor] -> [Serie]
seriesEfectivas [] _ = []
seriesEfectivas (serie:series) productores = 
    aplicarProdEfectivo productores serie: seriesEfectivas series productores

aplicarProdEfectivo :: [Productor] -> Productor
aplicarProdEfectivo productores serie = 
    find ((== maxListaBienestar productores serie) . bienestarSerie . aplicarProd serie) productores


serieSegunProd :: Serie -> [Productor] -> [Serie]
serieSegunProd serie =  map (aplicarProd serie) 

listaBienestar :: [Serie] -> [Number]
listaBienestar = map bienestarSerie

maxListaBienestar :: [Productor] -> Serie -> Number
maxListaBienestar productores serie = maximum . listaBienestar . serieSegunProd serie productores
