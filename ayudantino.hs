import Text.Show.Functions
import Data.List 

type Concepto = (String,Int)
nombreConcepto = fst
nivel = snd

type Juez = Ayudante -> Int

data Ayudante = UnAyudante { 
                nombre :: String,
				conceptos :: [Concepto] 
				} deriving (Show)

guille :: Ayudante 
guille = UnAyudante { 
                nombre = "guille",
				conceptos =[("orden superior",6),("expresiones lambda",7),("fold",9)]
				}
 
elChacal :: Ayudante 
elChacal = UnAyudante { 
                nombre = "el chacal",
				conceptos =[("aplicacion parcial",9),("fold",6),("sinonimos de tipo",7)]
				}
				
vicky :: Ayudante 
vicky = UnAyudante { 
                nombre = "vicky",
				conceptos =[("clases de tipo",5),("aplicacion parcial",6),("tuplas",9),("orden superior",8)]
				}
	
ayudantes :: [Ayudante]
ayudantes = [guille,elChacal,vicky]

--cuantosTienenElMismoNivel ayudantes = filter ((==6).nivel) (conceptos ayudantes)

-- 3

fueAprendiendo :: Ayudante -> Bool
fueAprendiendo = (sonCrecientes.obtenerNiveles)

sonCrecientes :: [Int] -> Bool
sonCrecientes niveles = (sort niveles) == niveles

obtenerNiveles :: Ayudante -> [Int]
obtenerNiveles ayudante = map nivel (conceptos ayudante)

--B
--preguntar como seria con genericLength
gise :: Juez
gise = promedio sumarNiveles cantNiveles

sumarNiveles :: Ayudante -> Int
sumarNiveles = sum.obtenerNiveles

cantNiveles :: Ayudante -> Int
cantNiveles = length.obtenerNiveles

promedio :: (Ayudante -> Int) -> (Ayudante -> Int) -> Ayudante -> Int 
promedio func1 func2 ayudante = div (func1 ayudante) (func2 ayudante) 

marche :: Juez
marche ayudante | sabeOrdenSuperior ayudante = 9
                | otherwise = 5

sabeOrdenSuperior :: Ayudante -> Bool
sabeOrdenSuperior ayudante = elem "orden superior" (map nombreConcepto (conceptos ayudante))

hernan :: Bool -> Juez
hernan humor ayudante | tienBuenDia humor = (cantNiveles ayudante) + 2
                      | otherwise = cantNiveles ayudante

tienBuenDia :: Bool -> Bool					  
tienBuenDia humor = humor == True

jueces :: [Juez]
jueces = [gise,marche, (hernan True)]

--se podia hacer con fold??
promedioTotal ayudante jueces = map (aplicarFuncion ayudante) jueces

aplicarFuncion algo otroAlgo = otroAlgo algo

esBuenAyudante ayudante jueces = all funcion (promedioTotal ayudante jueces)

funcion nota = nota > 7
{--
quienGana :: Carrera -> Auto
quienGana = (foldl1 compararDosAutos.participantes.correrCarrera)

compararDosAutos :: Auto -> Auto -> Auto
compararDosAutos unAuto otroAuto | velocidad unAuto > velocidad otroAuto = unAuto
                                 | otherwise = otroAuto
								 --}

--C

--maximoAyudanteSegun criterios

tresMejores = promedioMayorA7

promedioMayorA7 ayudante = filter fun ayudantes

fun ayudante = gise ayudante >= 7

juezSeleccionado ayudantes = (puntajeMaximo.obtenerPuntaje ayudantes)

obtenerPuntaje ayudantes juez = map juez ayudantes

puntajeMaximo notas = maximum notas