-- 1. Un número perfecto es aquel que es igual a la suma de sus divisores menores que él. Ejemplo 6:3,2,1
-- Utilizando lista por comprensión escribir la función "perfectos n" que dé como resultado la lista de números perfectos comprendidos en el intervalo 11,n.


sumaDivisores d = sum [ x	x<- [1..d], d 'mod' x , 0, x 1= d ]
perfectos p = [ x	x<-[1..p], sumaDivisores x == x ]

-- funcion que recibe como argumento dos listas ordenadas y devuelve una lista ordenada fusion de las listas
juntar :: (Ord a) => [a] -> [a] -> [a]
juntar a [] =a juntar [] b = b juntar (x:xs) (y:ys) = if x < y then (x : juntar xs (y:ys)) else (y: juntar ys (x:xs))

-- funcion qsort sin usar listas por comprension

particion ::  (Ord a) => a -> [a] -> ([a],[a])
particion p [] = ([],[])
particion p (x:xs) = if x <= p then (x:l1,l2) else (l1,x:l2) 
                    where (l1,l2) = particion p xs

qsort	(Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort y ++ [x] ++ qsort ys
                where (y,ys) = particion x xs


--funcion mizip

miZip :: [a] -> [b] -> [(a,b)]
miZip _ [] = [] 
miZip [] _ = [] 
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys


-- usando miZip y listas pro comprension realizar funcion escalar de dos listas, las suma de los productos uno a uno, componente a componente de cada lista.

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar (x:xs) (y:ys) = sum [ x*y | (x,y) <- miZip xs ys ]


-- usando miZip realizar indexado

indexado :: [a] -> [(a,Int)]
indexado lista = miZip lista [1..]


-- funcion inserta que inserta elementos en una lista de manera ordenada de menor a mayor, cada operacion en el head sobre la lista devuelve el elemento mas chico almacenado en ella
inserta :: (Ord a) => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) = if a < x then (a:x:xs) else (x: inserta a xs)

—Consideremos La siguiente función
---spLit:: (Ord a)= a->jal->([a],[a])
---spLit x L = ( L'y Ly c- L, y <=xj	[yly <- L, y > x])
---Defina una versión de esta función que trabaje en exactamente una sola pasada a La Lista "U'

split	[]	=	([]›[])
split	[x]	=	([x],[])
split	(x:xs:t) = (x:m1,xs:m2)
                    where (m1,m2) = split t


--Defina un tipo de dato árbol binario de búsqueda ( ArbolBin) Escriba el método addTree e InOrderTree.
--addTree	(Ord a) .> a -> ArbolBin a -> ArbolBin a
--Inserta un elemento del tipo a en un arbol binario. inOrderTree	(Ord a).) ArbolBin a -> [a] 
--Produce un listado "En Orden" del árbol binario.
--El listado en orden del árbol se define de la siguiente manera, primero se lista en orden el árbol izquierdo, luego la raíz y finalmente se lista en orden el árbol derecho.


data ArbolBin a = VacioAB | NodoAB a (ArbolBin a) (ArholBin a) deriving Show

mkNewTree   :: (Ord a) => ArbolBin a
addTree     :: (Ord a) => a -> ArbolBin a -> ArbolBin a
surfTree    :: (Ord a) => a -> ArbolBin a -› Bool
inOrderTree :: (Ord a) => ArbolBin a -› [a]

mkNewTree = VacioAB

addTree a VacioAB = NodoAB a VacioAB VacioAB
addTree a (NodoAB n izq der) | a == n = NodoAB n izq der
                             | a < n  = NodoAB n (addTree a izq) der 	
                             | a > n =  NodoAB n izq (addTree a der)

surfTree a VacioAB = False
surfTree a (NodoAB n izq der) | a == n = True
                              | a < n  = surfTree a izq 
                              | a > n  = surfTree a der

inOrderTree VacioAB = []
inOrderTree (NodoAB n izq der) = inOrderTree izq ++ [n] ++ inOrderTree der



---Una cola de prioridad es una estructura de datos que almacena elementos "clasificables". Con la particularidad que, cuando se saca uno de ella siempre se extrae el elemento con menor clave, de ahí su nombre pues clasifica los elementos en función de su prioridad. La prioridad más baja primero.
---Las funciones que manipulan a la cola de prioridad, son
---mkqpr: Instancia una nueva cola de prioridad vacia.
---addqpr: Agrega un nuevo elemento a la cola de prioridad
---nextqpr: Devuelve el elemento con clave más baja de la Cola de prioridad.
---popqpr: devuelve una cola de prioridad donde se ha quitado el nextqpr.
---Defina el TAD Cola Prioridad, e implemente el mismo utilizando un árbol binario de búsqueda como estructura de almacenamiento. Escribir todas las funciones necesarias para la manipulacion de la estructura subyascente, es decir para manipular el arbol. Sugerencia: Recordar como extraer el elemento con clave más pequeña de un árbol.


newtype ColaPrio a = CP[a] deriving Snou

mkqpr	ColaPrio a
mkqpr = CP []

addqpr :: a -> ColaPrio a -> ColaPrio a
addqpr a (CP p) = CP (p ++ [a])

nextqpr	ColaPrio a -› a
nextqpr (CP []) = error "Cola vacia"
nextqpr (CP (x:xs)) = x

popqpr :: ColaPrio a -> ColaPrio a
popqpr (CP []) = error "Cola vacia"
popqpr (CP (x:xs)) = CP xs





-- defina el tipo de dato e implemente los metodos del nuevo tipo de dato utilizando listas no ordenadas
-- y sin duplicados, el metodo unionset se escribira haciendo uso de los metodos ya definidos es decir no se operara directamente la lista 
--sino se operara al SET


module Set(Set, emptySet, setEmpty, inSet, addSet, delSet) where 
newtype Set a = Set [a] deriving Show 

emptySet :: Set a 
setEmpty :: Set a -> Bool 
inSet    :: (Eq a) => a -> Set a -> Bool 
addSet   :: (Eq a) => a -> Set a -> Set a 
delSet   :: (Eq a) => a -> Set a -> Set a
unionSet :: (Eq a) => Set a -> Set a -> Set a 


emptySet = Set [] 

setEmpty (Set []) = True 
setEmpty (Set [x]) = False 


inSet x (Set []) = False 
inSet x (Set (y:ys)) = x == y || inSet x (Set (ys)) 


addSet x (Set xs) = if inSet x (Set (xs)) then Set (xs) else addSet (Set x:xs) 


delSet x (Set []) = Set [] 
delSet x (Set (y:ys)) = if x /= y then addSet y (delSet x (Set (ys))) else delSet x (Set (ys)) 

unionSet (Set []) (Set (x)) = Set (x) 
unionSet (Set (x:xs)) (Set (y)) = if inSet x (Set y) then unionSet (Set (xs)) (Set (y)) else unionSet (Set (xs)) (Set (x:y))


--cola
newtype Queue a = Q [a] deriving Show

emptyQueue = Q[]

enQueue x (Q s) = Q (s ++ [x])

deQueue (Q []) = error "ColaVacia"
dequeue (Q(x:t)) = Q t

front (Q []) = error "ColaVacia"
front (Q(x:t)) = x

queueIsEmpty (Q []) = True
queueIsEmpty (Q _ ) = False


--pila

newtype Stack a = Stk [a] deriving Show

emptyStack = Stk []

push x (Stk xs) = Stk (x:xs)

pop (Stk []) = error "Pila Vacia"
pop (Stk (_:xs)) = Stk xs

top (Stk []) = error "Pila Vacia"
top (Stk (x:_)) = x

stackIsEmpty (Stk []) = True
stackIsEmpty (Stk _ ) = False