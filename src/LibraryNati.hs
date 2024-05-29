module Library where
import PdePreludat


type Nombre = String
type Actores = [Actor]
type Presupuesto = Number
type Temporadas = Number
type Rating = Number
type Cancelada = Bool
type Sueldo = Number
type Bienestar = Number
type Restricciones = [String]
type Productor =  Serie -> Serie

data Serie = UnaSerie {
    nombreSerie :: Nombre, 
    actores :: Actores, 
    presupuesto :: Presupuesto,
    temporadasEstimadas :: Temporadas,
    ratingProm :: Rating,
    cancelada :: Cancelada,
    bienestar :: Bienestar
}

data Actor = UnActor {
    nombreActor :: Nombre,
    sueldoPretendido :: Sueldo,
    restricciones :: Restricciones
}

paul :: Actor
paul = UnActor {
    nombreActor = "Paul Rudd",
    sueldoPretendido = 41000000,
    restricciones = ["No actuar en bata", "Comer ensalada de rúcula todos los días"]
}

johnnyDepp :: Actor
johnnyDepp = UnActor {
    nombreActor = "Johnny Depp",
    sueldoPretendido = 20000000,
    restricciones = []
}

helenaBonham :: Actor
helenaBonham = UnActor{
    nombreActor = "Helena Bonham Carter",
    sueldoPretendido = 15000000,
    restricciones = []
}

-- Funcion 1

sueldoPretendidoActores :: Serie -> [Sueldo]
sueldoPretendidoActores = map sueldoPretendido . actores

sumaSueldoTotalActores :: Serie -> Number
sumaSueldoTotalActores  = sum . sueldoPretendidoActores 

estaEnRojo :: Serie ->  Bool
estaEnRojo serie = presupuesto serie < sumaSueldoTotalActores serie

cantRestricciones :: Actor -> Number
cantRestricciones  = length . restricciones

restriccionesMayorAUno :: Actor -> Bool
restriccionesMayorAUno  = (> 1) . cantRestricciones 


actoresConMasDeUnaRestriccion :: Serie -> [Actor]
actoresConMasDeUnaRestriccion serie = filter restriccionesMayorAUno (actores serie)

cantActoresConRestricciones :: Serie -> Number
cantActoresConRestricciones = length . actoresConMasDeUnaRestriccion

esProblematica :: Serie -> Bool
esProblematica = (> 3) . cantActoresConRestricciones 

-- Funcion 2

modificarSerie :: (Serie -> a) -> a -> Serie -> Serie
modificarSerie campo valorModificado serie = serie {campo = valorModificado}

sacarActores :: Productor
sacarActores = modificarSerie actores (take 2 (actores serie)) 


productorConFavoritismo :: [Actor] -> Productor
productorConFavoritismo  actoresFavoritos = modificarSerie actores ((sacarActores serie) ++ actoresFavoritos)


timBurton :: Productor
timBurton serie = productorConFavoritismo [johnnyDepp, helenaBonham] serie


gatopardeitor :: Productor
gatopardeitor serie = serie

estireitor :: Productor
estireitor serie =  modificarSerie temporadasEstimadas 
(temporadasEstimadas serie )*2 serie

desespereitor ::  Productor -> Productor -> Serie  -> Serie
desespereitor productor1 productor2 =  productor2 . productor1

ratingMenorA :: Number -> Serie -> Bool
ratingMenorA  x = (< x) . ratingProm

canceleitor ::  Number -> Serie  -> Bool
canceleitor x serie = (estaEnRojo serie) && (ratingMenorA x serie)

-- Funcion 3

type Bienestar = Serie -> Serie
calcularBienestar :: Bienestar
calcularBienestar serie
    |cancelada serie = modificarSerie bienestar 0 serie
    |mayorACuatroTemporadas serie = modificarSerie bienestar 5
    | not . mayorACuatroTemporadas serie = modificarSerie bienestar (bienestarPorTemporadas serie) serie
    |mayorADiezActores serie = modificarSerie bienestar 3 serie
    |not . mayorADiezTemporadas serie = modificarSerie bienestar (bienestarPorActores serie) serie

mayorACuatroTemporadas :: Serie -> Bool 
mayorACuatroTemporadas serie = (> 4) .temporadasEstimadas

mayorADiezActores :: Serie -> Bool
mayorADiezActores serie = (<10) . cantActores

bienestarPorTemporadas :: Bienestar
bienestarPorTemporadas serie = 10 - (temporardasEstimadas serie)*2

cantActores :: Serie -> Serie
cantActores = length . actores

conRestricciones :: Actor-> Bool
conRestricciones actor  = not . null . restricciones 

actoresConRestricciones :: Serie -> Serie
actoresConRestricciones = filter (conRestricciones) actor

cantActoresConRestricciones :: Serie -> Number
cantActoresConRestricciones = length . actoresConRestricciones 

bienestarPorActores :: Serie -> Serie
bienestarPorActores serie = max 2 (10 - cantActoresConRestricciones serie)

-- Funcion 4

aplicarProductor :: Productor -> Serie -> Serie
aplicarProductor productor serie = productor serie 

compararBienestarSerie:: [Productor] -> Serie -> Productor
compararBienestarSerie [] _ = []
compararBienestarSerie [p]_ = p
compararBienestarSerie (p : ps) serie 
    |bienestar (aplicarProductor p serie) > bienestar (aplicarProductor ps serie) = p 
    |otherwise = compararBienestarSerie ps serie

aplicarProductorMaxBienestar :: Serie -> [Productor]  -> Productor
aplicarProductorMaxBienestar serie productores = aplicarProductor (compararBienestarSerie productores serie) serie

serieMasEfectiva :: [Serie] -> [Productor]-> [Serie]
serieMasEfectiva series productores = foldl (flip aplicarProductorMaxBienestar) series productores
















