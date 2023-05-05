module Library where
import PdePreludat

--Punto 1

type MatrizCeldas = [[Celda]]

type Vector = (Number, Number)

type Cabezal = Vector

type Posicion = Vector

type Celda = [Bolita]

data Bolita = Roja | Azul | Verde | Negra deriving (Show, Eq)

data Direccion = Arriba | Abajo | Izquierda | Derecha deriving Show

data Tablero = UnTablero {
    celdas :: MatrizCeldas,
    cabezal :: Cabezal
} deriving Show

--Punto 2
inicializarTablero :: Number -> Number -> Tablero
inicializarTablero x y = UnTablero {celdas = matrizDeTamanio x y, cabezal = cabezalInicial }

matrizDeTamanio :: Number -> Number -> MatrizCeldas
matrizDeTamanio x y = replicate y (replicate x [])

cabezalInicial :: Cabezal
cabezalInicial = (1,1)

celdaEnPosicion :: Posicion -> Tablero -> Celda
celdaEnPosicion (x,y) = (!! (x-1) ).(!! (y-1) ).celdas --Indice 1

--Punto 3

type Sentencia = Tablero -> Tablero

--mover
mover :: Direccion -> Sentencia
mover direccion tablero = tablero {cabezal = moverCabezal direccion (cabezal tablero)}

moverCabezal :: Direccion -> Cabezal -> Cabezal
moverCabezal direccion = sumaVectores (direccionComoVector direccion)

sumaVectores :: Vector -> Vector -> Vector
sumaVectores (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

direccionComoVector :: Direccion -> Vector
direccionComoVector Arriba = (0,1)
direccionComoVector Abajo = (0,-1)
direccionComoVector Izquierda = (-1,0)
direccionComoVector Derecha = (1,0)

--Sacar y poner
sacar :: Bolita -> Sentencia
sacar bolita = cambiarCeldaActualSegun (sacarBolita bolita)

poner :: Bolita -> Sentencia
poner bolita = cambiarCeldaActualSegun (agregarBolita bolita)

agregarBolita :: Bolita -> Celda -> Celda
agregarBolita bolita celda = bolita : celda

sacarBolita :: Bolita -> Celda -> Celda
sacarBolita bolita celda = bolitasDeOtroColor bolita celda ++ drop 1 (bolitasDeColor bolita celda)

bolitasDeOtroColor :: Bolita -> Celda -> [Bolita]
bolitasDeOtroColor bolita = filter (/= bolita)

bolitasDeColor :: Bolita -> Celda -> [Bolita]
bolitasDeColor bolita = filter (== bolita)

cambiarCeldaActualSegun :: (Celda -> Celda) -> Tablero -> Tablero
cambiarCeldaActualSegun modificacion tablero = tablero{ celdas = modificarCelda (celdas tablero) (posicionCabezal tablero) modificacion }

posicionCabezal :: Tablero -> Posicion
posicionCabezal = sumaVectores (-1,-1) . cabezal --El cabezal es indice 1

modificarCelda :: MatrizCeldas -> Posicion -> (Celda -> Celda) -> MatrizCeldas
modificarCelda celdas (x,y) modificacion = reemplazarEnIndice y (modificarCeldaEnFila fila x modificacion ) celdas
    where fila = celdas !! y

modificarCeldaEnFila :: [Celda] -> Number -> (Celda -> Celda) ->  [Celda]
modificarCeldaEnFila fila indice modificacion = reemplazarEnIndice indice celdaModificada fila
    where celdaModificada = modificacion (fila !! indice)

reemplazarEnIndice :: Number -> a -> [a] -> [a]
reemplazarEnIndice indice x xs = take indice xs ++ [x] ++ drop (indice + 1) xs

--Punto 4

--Repetir
repetir :: Number -> [Sentencia] -> Sentencia
repetir veces sentencias = last . take (veces + 1) . iterate (aplicarSentencias sentencias)  --El primer elemento del iterate es el tablero sin modificar, asi que veces + 1

type Condicion = Tablero -> Bool

--alternativa
alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Sentencia
alternativa condicion sentenciasParaVerdad sentenciasParaFalso = siNo condicion sentenciasParaFalso . si condicion sentenciasParaVerdad

aplicarSentencias :: [Sentencia] -> Tablero -> Tablero
aplicarSentencias sentencias tablero = foldr ($) tablero (reverse sentencias) --Reverse para que se ejecute izquierda a derecha

--si
si :: Condicion -> [Sentencia] -> Sentencia
si condicion sentencias tablero
    | condicion tablero = aplicarSentencias sentencias tablero
    | otherwise = tablero

--si no
siNo :: Condicion -> [Sentencia] -> Sentencia
siNo condicion sentencias tablero
    | not (condicion tablero) = aplicarSentencias sentencias tablero
    | otherwise = tablero

--mientras
mientras :: Condicion -> [Sentencia] -> Sentencia
mientras condicion sentencias tablero
    | condicion tablero = mientras condicion sentencias (aplicarSentencias sentencias tablero)
    | otherwise = tablero

--ir al borde
irAlBorde :: Direccion -> Sentencia
irAlBorde direccion = mientras (puedeMoverse direccion) [mover direccion]

--Punto 5

--puede moverse
puedeMoverse :: Direccion -> Tablero -> Bool
puedeMoverse direccion tablero = not . estaFueraDelTablero (tamanioTablero tablero) . moverCabezal direccion . cabezal $ tablero

estaFueraDelTablero :: Vector -> Posicion -> Bool
estaFueraDelTablero (limiteX, limiteY) (x,y) = x < 1 || x > limiteX || y < 1 || y > limiteY

tamanioTablero :: Tablero -> Vector
tamanioTablero tablero = (length fila, length (celdas tablero))
    where fila = head (celdas tablero)

--hay bolita

hayBolita :: Bolita -> Tablero -> Bool
hayBolita bolita tablero = elem bolita (celdaActual tablero)

celdaActual :: Tablero -> Celda
celdaActual tablero = celdaEnPosicion (cabezal tablero) tablero

-- cantidad bolitas

cantidadBolitas :: Bolita -> Tablero -> Number
cantidadBolitas bolita = length . bolitasDeColor bolita . celdaActual 

-- punto 6

programa :: [Sentencia] -> Tablero -> Tablero
programa = aplicarSentencias 

--Punto 7

tableroInicialEjemplo :: Tablero 
tableroInicialEjemplo = inicializarTablero 3 3

sentenciasEjemplo :: [Sentencia]
sentenciasEjemplo = [
    mover Arriba,
    poner Negra,
    poner Negra,
    poner Azul,
    mover Arriba,
    repetir 15 [
        poner Roja,
        poner Azul
    ],
    alternativa (hayBolita Verde) [
            mover Derecha, 
            poner Negra
        ] [
            mover Abajo,
            mover Derecha, 
            poner Azul
        ],
   
    mover Derecha,
    mientras ( (<= 9) . cantidadBolitas Verde) [
        poner Verde
    ],
    
    poner Azul
    ]

tableroFinalEjemplo :: Tablero
tableroFinalEjemplo = programa sentenciasEjemplo tableroInicialEjemplo 