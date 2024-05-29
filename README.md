Nos contactaron para hacer un sistema que ayude a tomar decisiones sobre series de TV a
producir en un nuevo servicio llamado PdePrime Video.
En el sistema vamos a trabajar con series, de las cuales queremos saber cual es el nombre
de la misma, quienes actúan en ella (y el orden de importancia), su presupuesto anual,
cantidad de temporadas estimadas, el rating promedio que tiene y si está cancelada o no.
También, de cada actor o actriz conocemos el nombre, cuál es su sueldo pretendido (anual)
y qué restricciones tiene a la hora de trabajar. Por ejemplo, sabemos que el sueldo
pretendido de Paul Rudd es de 41 millones al año y que sus restricciones son no actuar en
bata y comer ensalada de rúcula todos los días.
Resolver los siguientes requerimientos, maximizando el uso de composición, aplicación
parcial y orden superior.
1.
a. Saber si la serie está en rojo, esto es si el presupuesto no alcanza a cubrir lo
que quieren cobrar todos los actores.
b. Saber si una serie es problemática, esto ocurre si tienen más de 3 actores o
actrices con más de 1 restricción

2. Queremos modelar diferentes tipos de productores, que evalúan qué se hace con las
series:
a. con favoritismos: elimina a los dos primeros actores de la serie y los
reemplaza por sus actores favoritos.
b. tim burton: es un caso particular de un productor con favoritismos, siempre
reemplaza a los primeros dos actores por johnny depp y helena bonham
carter, cuyos sueldos pretendidos anuales son $20.000.000 y $15.000.000
respectivamente, y no tienen ninguna restricción.
c. gatopardeitor: no cambia nada de la serie.
d. estireitor: duplica la cantidad de temporadas estimada de la serie.
e. desespereitor: hace un combo de alguna de las anteriores ideas, mínimo 2.

1 de 2

f. canceleitor: si la serie está en rojo o el rating baja de una cierta cifra, la serie
se cancela.

3. Calcular el bienestar de una serie, en base a la sumatoria de estos conceptos:
- Si la serie tiene estimadas más de 4 temporadas, su bienestar es 5, de lo contrario
es (10 - cantidad de temporadas estimadas) * 2
- Si la serie tiene menos de 10 actores, su bienestar es 3, de lo contrario es (10 -
cantidad de actores que tienen restricciones), con un mínimo de 2
Aparte de lo mencionado arriba, si la serie está cancelada, su bienestar es 0 más
allá de cómo diesen el bienestar por longitud y por reparto.
4. Dada una lista de series y una lista de productores, aplicar para cada serie el
productor que la haga más efectiva: es decir, el que le deja más bienestar.
5. Responder, justificando en cada caso:
a. ¿Se puede aplicar el productor gatopardeitor cuando tenemos una lista
infinita de actores?
b. ¿Y a uno con favoritismos? ¿De qué depende?
6. Saber si una serie es controvertida, que es cuando no se cumple que cada actor de
la lista cobra más que el siguiente.

7. Explicar la inferencia del tipo de la siguiente función:
funcionLoca x y = filter (even.x) . map (length.y)

