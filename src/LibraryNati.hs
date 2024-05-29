module LibraryNati where
import PdePreludat


type Nombre = String
type Actores = [(Number, Actor)]
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
}deriving (Show, Eq)

data Actor = UnActor {
    nombreActor :: Nombre,
    sueldoPretendido :: Sueldo,
    restricciones :: Restricciones
}deriving (Show, Eq)

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

obtenerSueldo :: (Number, Actor) -> Sueldo
obtenerSueldo (_,actor) = sueldoPretendido actor

sueldoPretendidoActores :: Serie -> [Sueldo]
sueldoPretendidoActores serie = map (obtenerSueldo) (actores serie)

sumaSueldoTotalActores :: Serie -> Number
sumaSueldoTotalActores  = sum . sueldoPretendidoActores 

mayorAPorCampo :: Ord a => Number -> (a -> Number) -> a  -> Bool
mayorAPorCampo x y = (> x) . y

estaEnRojo :: Serie ->  Bool
estaEnRojo serie = not (mayorAPorCampo (presupuesto serie) sumaSueldoTotalActores serie)

cantRestricciones :: Actor -> Number
cantRestricciones  = length . restricciones

listaActores :: Serie -> [Actor]
listaActores serie = map snd (actores serie)

actoresConMasDeUnaRestriccion :: [Actor] -> [Actor]
actoresConMasDeUnaRestriccion actores = filter (mayorAPorCampo 1 cantRestricciones) actores

cantActoresMasDeUnaRestriccion :: Serie -> Number
cantActoresMasDeUnaRestriccion = length . actoresConMasDeUnaRestriccion . listaActores

esProblematica :: Serie -> Bool
esProblematica = mayorAPorCampo 3 cantActoresMasDeUnaRestriccion 


-- Funcion 2
{-
modificarSerie :: (Serie -> a) -> a -> Serie -> Serie
modificarSerie campo valorModificado serie = serie {campo = valorModificado}
-}

actoresOrdenados :: Serie->Serie
actoresOrdenados serie = serie {actores = ordenarActores (actores serie)}

ordenarActores :: [(Number, Actor)] -> [(Number, Actor)]
ordenarActores [] = []
ordenarActores (actor : actores) = ordenarTuplaPorNumero actor (ordenarActores actores)

ordenarTuplaPorNumero ::  (Number, Actor) -> [(Number, Actor)] -> [(Number, Actor)]
ordenarTuplaPorNumero x [] = [x]
ordenarTuplaPorNumero (x1, actor1) ((x2, actor2):actores)
    |x1 <= x2 = (x1, actor1) : (x2, actor2):actores
    |otherwise = (x2, actor2) : ordenarTuplaPorNumero (x1, actor1) actores

sacarActores :: Serie -> [(Number, Actor)]
sacarActores serie = drop 2 (actores (actoresOrdenados serie))

productorConFavoritismo :: [(Number, Actor)] -> Productor
productorConFavoritismo  actoresFavoritos serie = serie {actores = (sacarActores serie) ++ actoresFavoritos}

timBurton :: Productor
timBurton serie = productorConFavoritismo [(1,johnnyDepp), (2,helenaBonham)] serie


gatopardeitor :: Productor
gatopardeitor serie = serie

duplicarTemporadas :: Serie  -> Number
duplicarTemporadas serie = (temporadasEstimadas serie)*2

estireitor :: Productor
estireitor serie = serie {temporadasEstimadas = duplicarTemporadas serie } 

--Corregir
desespereitor ::  Productor -> Productor -> Serie  -> Serie
desespereitor productor1 productor2 =  productor2 . productor1

cancelarSerie ::  Number -> Serie  -> Bool
cancelarSerie x serie = (estaEnRojo serie) && (mayorAPorCampo x ratingProm serie)

canceleitor ::  Number -> Productor
canceleitor x serie
    |cancelarSerie x serie = serie {cancelada = True}
    |otherwise = serie 

-- Funcion 3

type BienestarSerie = Serie -> Serie

-- Cambiar (sumatoria de todo)
calcularBienestar :: BienestarSerie
calcularBienestar serie
    |cancelada serie = serie{bienestar = 0}
    |mayorAPorCampo 4 temporadasEstimadas serie = serie{bienestar = 0}
    | not (mayorAPorCampo 4 temporadasEstimadas serie) = serie {bienestar = (bienestarPorTemporadas serie)}
    |mayorAPorCampo 10 cantActores serie = serie {bienestar = 3} 
    |not (mayorAPorCampo 10 cantActores serie) = serie { bienestar = bienestarPorActores serie}


bienestarPorTemporadas :: Serie -> Number
bienestarPorTemporadas serie = 10 - (duplicarTemporadas serie)

cantActores :: Serie -> Number
cantActores = length . actores

conRestricciones :: Actor-> Bool
conRestricciones  = not . null . restricciones 

actoresConRestricciones :: Serie -> [Actor]
actoresConRestricciones serie = filter (conRestricciones)(map snd (actores serie))

cantActoresConRestricciones :: Serie -> Number
cantActoresConRestricciones = length . actoresConRestricciones 

bienestarPorActores :: Serie -> Number
bienestarPorActores serie = max 2 (10 - cantActoresConRestricciones serie)

-- Funcion 4

aplicarProductor :: Productor -> Serie -> Serie
aplicarProductor productor = productor 

bienestarSerie :: Productor -> Serie -> Bienestar
bienestarSerie productor = bienestar . aplicarProductor productor 

mayorBienestarSerie :: Productor -> [Productor] -> Serie -> Bool
mayorBienestarSerie productor productores serie = bienestarSerie productor serie >  bienestarSerie (head productores) serie

compararBienestarSerie:: [Productor] -> Serie -> Productor
compararBienestarSerie [p]_ = p
compararBienestarSerie (p : ps) serie 
    |mayorBienestarSerie p ps serie = compararBienestarSerie (p:(tail ps)) serie
    |otherwise = compararBienestarSerie ps serie

aplicarProductorMaxBienestar :: Serie -> [Productor] -> Serie
aplicarProductorMaxBienestar serie productores = aplicarProductor (compararBienestarSerie productores serie) serie

serieMasEfectiva :: [Serie] -> [Productor]-> [Serie]
serieMasEfectiva series productores = map (flip aplicarProductorMaxBienestar productores) series 

{-
--Ej 5
a)
Se mostrará un resultado ya que dicha funcion ya que dicha funcion devuelve la serie sin modificar, por lo que continuará mostrando indefinidamente hasta que genere un stack overflow.

b) Considerando que primero se debe ordenar, nunca se podrá quitar los dos primeros elementos.
-}

serieConvertida  :: Serie -> Serie
serieConvertida serie = serie{actores = tail (actores serie)}

esControvertida :: Serie -> Bool
esControvertida serie 
    | null (actores serie) = False
    |null (tail (actores serie)) = True
    | obtenerSueldo (head (actores serie)) > obtenerSueldo (head (tail (actores serie))) = esControvertida (serieConvertida (serie))
    |otherwise = False 


{-
7. Explicar la inferencia del tipo de la siguiente función:
funcionLoca x y = filter (even.x) . map (length.y)

funcionLoca:: (Number -> Number) -> (a -> [b]) -> [a] -> [Number]

-}

