-- ***************************************
-- * �NGEL LUIS ORTIZ FOLGADO            *
-- * DNI: 50774922-F                     *
-- ***************************************

-- *********************************
-- *  DECLARACI�N DE TIPOS         *
-- *********************************
data Ins = String := Exp| Cond ExpB Prog Prog | While ExpB Prog

data Exp = V String | I Integer | Exp :+ Exp | Exp :- Exp |Exp :* Exp  
      deriving (Eq,Show) 
	  
data ExpB = Exp :== Exp | Exp :> Exp | Exp :>= Exp |Exp :< Exp | Exp :<= Exp
   
type Prog = [Ins]
type  Estado = [(String,Integer)] 

-- **************************************************
-- *  FUNCI�N PRINCIPAL QUE EJECUTA EL PROGRAMA     *
-- **************************************************

ejecuta :: Prog -> Estado -> Integer
ejecuta p estado = resuelve p estado  

--    ***********************************************
--    * FUNCIONES FACTORIAL Y ESTADO INICIAL        *
--    ***********************************************
factorial = ["Y" := V "X", "R" := I 1, While (I 0 :< V "Y") ["R" := (V "R" :* V "Y"), "Y" := (V "Y" :- I 1)]]
s0=  [("X",3)]

-- ****************************************************************************
-- *   FUNCIONES AUXILIARES PARA PODER LLEVAR A CABO LA EJECUCION DEL PROGRAMA*
-- ****************************************************************************

-- FUNCI�N AUXILIAR PARA EVALUAR EXPRESIONES ARITM�TICAS
-- Para evaluar expresiones estas deben ir entre par�ntesis
evalua :: Exp ->Estado -> Integer
evalua (I n) es = n 	
evalua (V v) ((v',x):es) 
    |  (v == v') = x
	|  otherwise = evalua (V v) es 
evalua (ex :+ ex') es = evalua ex es + evalua ex' es
evalua (ex :- ex') es = evalua ex es - evalua ex' es
evalua (ex :* ex') es = evalua ex es * evalua ex' es


-- FUNCI�N AUXILIAR PARA EVALUAR EXPRESIONES BOOLEANAS
evaluaexpb :: ExpB -> Estado -> Bool
evaluaexpb (ex :== ex') es = evalua ex es  == evalua ex' es
evaluaexpb (ex :> ex') es = evalua ex es  > evalua ex' es
evaluaexpb (ex :>= ex') es = evalua ex es  >= evalua ex' es
evaluaexpb (ex :< ex') es = evalua ex es  < evalua ex' es
evaluaexpb (ex :<= ex') es = evalua ex es  <= evalua ex' es



-- FUNCI�N QUE PERMITE CALCULAR CADA INSTRUCCI�N DEL PROGRAMA
resuelve :: Prog -> Estado -> Integer
resuelve [] (("R",r):es) = r
resuelve [] (e:es) = resuelve [] es
resuelve (s := exp:ps) es = resuelve' (s := exp:ps) es  es 
resuelve (Cond exb p p':ps) es 
     | evaluaexpb exb es == True = resuelve (p++ps) es
     | otherwise = resuelve (p'++ps) es	 
resuelve ( While exb p :ps) es
     | evaluaexpb exb es == True = resuelve (p ++  While exb p :ps) es
     | otherwise  = resuelve ps es	  

-- FUNCI�N AUXILIAR QUE PERMITE  RESOLVER LAS ASIGNACIONES RECURSIVAMENTE
-- El segundo par�metro estado (es') permite saber si la lista ha sido recorrida completamente, para as�,
-- conocer si la variable ya estaba en la lista de estados.
resuelve' :: Prog -> Estado -> Estado -> Integer
resuelve' (s := exp:ps) ((s',a):es)(e':e's)
     | s == s' = resuelve ps ((s',evalua exp ((s',a):es)):es) 
     | e's == []  = resuelve ps (((s',a):es) ++ ((s,evalua exp ((s',a):es)):[]))	 
	 | s /= s' = resuelve' (s := exp:ps)  (es ++ (s',a):[]) e's	 


-- **********************************************************
-- *          NOTA SOBRE LA PR�CTICA                       *
-- **********************************************************
-- Para realizar esta pr�ctica me he ayudado del ejemplo de las cifras explicado en clase para realizar la evaluaci�n de las expresiones 
-- tanto aritm�ticas como booleanas ya que las ecuaciones de ambas son similares. Tambi�n he usado la representaci�n de los programas que 
-- viene puesta como ejemplo en el enunciado de la pr�ctica.


