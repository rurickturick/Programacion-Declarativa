-- **************************************************************************
--   Ángel Luis Ortiz Folgado                                               *
--    DNI 50774922 F                                                        *
-- **************************************************************************
-- **********************Funcion principal *********************
-- Uso:
-- casillas "estadoInicial"  donde estadoInicial es un tablero  desde el que queremos partir para obtener la solución

-- ***************************************************************
--casillas-->muestra ,partiendo del estado inicial, los estados intermedios hasta llegar a la solución .
casillas::Estado->IO ()
casillas estadoIni = putStr (imprimeSolucion(resolver estadoIni)) -- uso la  funcion IO putStr para mostrar la solución de manera más "elegante"
	
---------------------------------------------------

-- Algunas inplementaciones de estados 
puzzle2,puzzle3,puzzle32,puzzle3dificil,puzzle4,raiz::Estado

puzzle2= [(2,2),(2,1),(1,2),(1,1)] --puzzle de  2x2 se necesitan 4 movimientos para llegar a la solución
puzzle3 = [(3,2),(1,1),(2,1),(3,1),(1,2),(2,2),(3,3),(1,3),(2,3)] -- puzzle  de 3x3 , necesita un movimiento para terminar
puzzle32 = [(2,2),(1,1),(2,1),(3,1),(1,3),(1,2),(3,2),(2,3),(3,3)] --puzzle de 3x3 necesita 4 movimientos
raiz = [] --Nos ayuda para identificar al padre del estado inicial
puzzle3dificil = [(3,1),(1,1),(2,2),(2,1),(1,3),(1,2),(3,2),(2,3),(3,3)] --puzzle de 3x3 necesita 7 movimientos para solición
puzzle4 = [(2,1),(1,2),(1,1),(3,2),(3,1),(1,3),(2,4),(2,2),(4,1),(2,3),(3,3),(4,4),(4,2),(1,4),(3,4),(4,3)] --Puzzle de 4x4 con  18 movimientos para solucion



-- ****************************************************************************
--                        Declaración de tipos                                *
-- ****************************************************************************

type Casilla = (Int,Int) --Representa la posicion de la casilla en el tablero. El primer numero corresponde a la columna y el segundo a la fila.
type Estado = [Casilla] 
-- *******************************************************************************

--Un estado es una lista de casillas, en la primera posición se guardan las coordenadas del hueco y las en siguientes, que corresponden al número de su posición,
-- las coordenadas de cada una.
-- ejemplo para un puzzle de 3x3:
--[hueco,casilla1,casilla2,casilla3,casilla4,casilla5,casilla6,casilla7,casilla8] casillaX contiene las coordenadas de donde se encuentra esa casilla


-- *********************************************************************************
--              Funciones auxiliares para realizar la búsqueda de la solución      *
-- *********************************************************************************

--creaestadoFinal crea el estado final a partir de un número, n, que es el tamaño del tablero(nxn)
-- hay que eliminar la posición (n,n) que es la del hueco y tiene que ir al principio

creaEstadoFinal::Int->Estado
creaEstadoFinal n =  ([(n,n)]++ reverse (drop 1 (reverse([(x,y) | y<-[1..n], x<-[1..n]])) ))

--tamTablero  devuelve el tamaño del tablero, a partir del estado inicial que le pasamos
tamTablero::Estado->Int
tamTablero estado = head [x | x<-[1..], x^2==length estado] -- se podría hacer con una raiz cuadrada pero me daba error de tipos

--esSolucion comprueba si el estado pasado es una solucion

esSolucion::Estado->Bool
esSolucion estado = estado==creaEstadoFinal(tamTablero estado)

--distManhattan  calcula la distancia entre dos fichas
distManhattan::Casilla->Casilla->Int
distManhattan (a,b) (c,d) = abs(a-c) + abs(b-d)

--esMovValido  comprueba   si se puede hacer un movimiento entre dos casillas dadas
-- si la suma de  las distancias  entre  las dos casillas (en valor absoluto) es  1 entonces son adyacentes y se pueden mover
esMovValido::Casilla->Casilla->Bool
esMovValido (a,b) (c,d) 
						| distManhattan (a,b) (c,d) == 1 = True
						| otherwise = False


--sucesores   calcula la lista de los posibles estados siguientes,  moviendo el hueco en las direcciones que se pueda, a partir de un estado dado.						
sucesores :: Estado -> [Estado]
sucesores (hueco:fichas) = calculaEstados [hueco] fichas 

--calculaEstados función auxiliar que  devuelve la lista de estados posibles que se obtienen de analizar  las casillas dos a dos
calculaEstados :: [Casilla] -> [Casilla] -> [Estado]
calculaEstados (hueco:fichas) [] = []
calculaEstados (hueco:fichas1) (fichaMov:fichas2)
		| esMovValido hueco fichaMov = mov : calculaEstados (hueco:fichas1++[fichaMov]) fichas2  --Si el movimiento de la ficha y el hueco es valido  añade un estado con ese movimiento a la lista de estados,y sigue calculando los estados siguientes
		| otherwise = calculaEstados (hueco:fichas1++[fichaMov]) fichas2 --Si no se puede mover busca recursivamente en  la lista de casillas, sin añadir ese estado al conjunto de sucesores
		  where mov = fichaMov:fichas1 ++ hueco:fichas2 --Definicion del nuevo estado, pone en el hueco la posicion de la ficha y donde el hueco el de la ficha 
					
--heuristicaManhattan  heurística que calcula la suma de las distancias de cada ficha a su posicion final
heuristicaManhattan::Estado->Int
heuristicaManhattan estado = sum(zipWith distManhattan estado estadoObjetivo) --calculo la distancia entre cada posicion de un estado y su posición corecta
						where estadoObjetivo = creaEstadoFinal (tamTablero estado) 


-- ************************************************************************************
--                                                                                    *
--          Búsqueda de la solución en el espacio de estados                          *
-- ************************************************************************************ 

--resolver  Funcion principal que dado un estado inicial nos da el conjunto de movimientos que habra que realizar para llegar al estado final
resolver :: Estado -> [Estado]
resolver s = busqueda [(s,raiz)] []
				
--busqueda hace  una busqueda voraz de la solucion usando como heuristica la distancia Manhattan. Va expandiendo aquellos nodos con menor valor heurístico
--se usan  dos listas, una de nodos abiertos (abier) y otra de nodos  expandidos (expan)
--Ademas para poder encontrar un camino hasta la solución debemos tener conocimiento del "padre" del estado generado (actual) para poder hallar un camino inverso hasta el nodo inicial, con padre []
busqueda::[(Estado,Estado)]->[(Estado,Estado)]->[Estado]						
busqueda [] expan = []
busqueda ((actual,padre):abier) expan
		| esSolucion actual = caminoHasta actual ((actual,padre):expan) --Si es solucion el estado actual, buscamos el camino desde el actual hasta el inicial
		| otherwise = busqueda (ordenar(creaEstados abier expan actual)) ((actual,padre):expan) --Si no es solucion se añade el nodo actual a los ya expandidos, y se sigue buscando la solucion en los estados generados ordenados por el valor de la distancia Manhattan
			

--ordenar   ordena el conjunto de estados según el valor de la heurística que le hayamos dado al nodo actual (hace el quicksort)
ordenar::[(Estado,Estado)]->[(Estado,Estado)]
ordenar [] = []
ordenar ((x,p):xs) = ordenar(menores) ++ [(x,p)] ++ ordenar(mayores)
					where
						menores = [(a,b) | (a,b) <-xs, heuristicaManhattan a < heuristicaManhattan x]
						mayores = [(c,x) | (c,x) <-xs, heuristicaManhattan c >= heuristicaManhattan x]
						
--creaEstados crea los estados a partir del actual, comprobando que no esten  en la lista de abiertos o en la de expandidos			
creaEstados:: [(Estado, Estado)]-> [(Estado, Estado)] -> Estado -> [(Estado, Estado)]
creaEstados abier expan actual = crearPadre (actual) (visitados (visitados ((sucesores (actual))) (expan)) (abier))

--crearPadre  dado un estado, lo asigna como padre a la lista de estados del segundo argumento y devuelve la lista de pares de estados
crearPadre::Estado->[Estado]->[(Estado,Estado)]
crearPadre p [] = []
crearPadre p (x:xs) = (x,p) : crearPadre p xs
				
--visitados comprueba si un estado  ha sido visitado y devuelve la lista de los que no  lo han sido  para evitar ciclos				
visitados::[Estado]->[(Estado,Estado)]->[Estado] 
visitados [] y = []
visitados (x:xs) y 
						| noEsta x y = x: visitados xs y
						| otherwise = visitados xs y

--noEsta  devuelve True si un estado no esta en la lista de estados ( y False si esta) 						
noEsta::Estado->[(Estado,Estado)]->Bool
noEsta x [] = True
noEsta x ((actual,padre):ys)
							| x == actual || x == padre = False
							| otherwise = noEsta x ys
							


--caminoHasta  devuelve el camino que se ha seguido para llegar hasta un estado dado, para lograrlo  utiliza   que cada nodo esta emparejado con su estado padre
caminoHasta::Estado -> [(Estado, Estado)] -> [Estado]
caminoHasta e xs
					| p == raiz = [e]
					| otherwise = caminoHasta p xs ++ [e] 
						where p = estadoPadre e xs
					

--estadoPadre  devuelve el estado padre de un nodo dado, para ello vamos comparando en la lista de pares de estado y devolvemos aquel que corresponda el hijo con el padre					
estadoPadre :: Estado -> [(Estado, Estado)] -> Estado
estadoPadre e ((x,p):xs)
				| e == x = p
				|otherwise = estadoPadre e xs

				


-- *************************************************************************************************
--                       Imprimir resultados                                                       *
-- *************************************************************************************************
						
--numCasilla   devuelve un Int que representa el número que contiene la casilla
numCasilla::Casilla->Estado->Int
numCasilla pos (x:xs)
						| pos==x = 0
						| otherwise = 1 + numCasilla pos xs
						
--mostrarCasilla  muestra la casilla en la pantalla. El hueco se representa con H
mostrarCasilla::Int->String
mostrarCasilla n 
				| n==0 = "H"			
				| otherwise = show n 
				
--mostrarFila  muestra por pantalla la fila n del estado e, se usa la funcion mostrarFilaAux para permitir tableros de distintos tamaños
mostrarFila::Int->Estado->String
mostrarFila n e = mostrarFilaAux n e 0	

--mostrarFilaAux  es una función auxiliar que  imprime cada fila del tablero, sin importar  el tamaño de este
mostrarFilaAux::Int->Estado->Int->String
mostrarFilaAux n e t 
					| t == tamTablero(e) = "\n" 
					| otherwise = c t ++ "  " ++ (mostrarFilaAux n e (t+1)) 
					where c t = mostrarCasilla(numCasilla (t+1,n) e)

--imprimeEstado muestra  un estado  por pantalla				
imprimeEstado::Estado->String
imprimeEstado est = imprimeEstadoAux (est) (tamTablero(est))

--imprimeEstadoAux  es una  funcion auxiliar que muestra por pantalla un estado sin importar su tamaño 
imprimeEstadoAux::Estado->Int->String
imprimeEstadoAux est t 
						| t == 0 = "\n"
						| otherwise = (imprimeEstadoAux est (t-1)) ++ mostrarFila t est++"\n"

--imprimeSolucion  imprime por pantalla la solución del puzzle						
imprimeSolucion::[Estado]->String
imprimeSolucion [] = ""
imprimeSolucion (x:xs) = imprimeEstado x ++ imprimeSolucion xs


		

-- ******************************************************************************************************


-- ***********************************************************
--              Notas sobre la práctica                      *
-- ***********************************************************

-- Para realizar esta práctica he usado como ayuda el libro "Programación Funcional con Haskell" de Blas C. Ruiz et al; donde viene un problema
-- del 8puzzle y de ahi me inspiré a la hora de crear la representación de los puzzles, además, también me basé en una práctica que hice este año para
-- la asignatura de Inteligencia  Artificial dónde aprendí las heurísticas y las formas de recorrer espacios de estados para encontrar soluciones.
--  El método para que sirva para puzzles de nxn lo he implementado yo, asi como la parte de la heurística (aunque me he ayudado de lo visto en Inteligencia Artificial.
-- También he usado las transparencias de clase así como internet para solucionar problemas que me han surgido con la implementación de funciones y 
-- problemas con Haskell. 
-- los ejemplos de estados iniciales implementados al principio de la práctica para probarla están sacados de internet.








