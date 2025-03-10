-- Estructura del árbol
data Abb a = Vacio | Nodo a (Abb a) (Abb a) deriving (Show)

-- Insertar un nuevo nodo a un árbol definido
-- insertarNodo (Valor a insertar) (Árbol)
insertarNodo :: (Ord a) => a -> Abb a -> Abb a
insertarNodo nuevo Vacio = Nodo nuevo Vacio Vacio
insertarNodo nuevo (Nodo a izq der)
 | nuevo <= a = Nodo a (insertarNodo nuevo izq) der
 | nuevo > a = Nodo a izq (insertarNodo nuevo der)

-- Devuelve True o False si el nodo se halla en el árbol
-- BuscarNodo (Valor a buscar) (Árbol)
buscarNodo :: (Ord a) => a -> Abb a -> Bool
buscarNodo buscado Vacio = False
buscarNodo buscado (Nodo a izq der)
 | buscado == a = True
 | buscado < a = buscarNodo buscado izq
 | otherwise = buscarNodo buscado der

-- Crea un nuevo árbol a partir de una lista
-- crearDeLista (Lista de números)
crearDeLista :: (Ord a) => [a] -> Abb a
crearDeLista [] = Vacio
crearDeLista (raiz:sub) = Nodo raiz (crearDeLista (filter (<= raiz) sub)) (crearDeLista (filter (> raiz) sub))

-- Devuelve el número de elementos en el árbol
-- numeroNodos (Árbol)
numeroNodos :: (Num a) => Abb t -> a
numeroNodos Vacio = 0
numeroNodos (Nodo _ izq der) = 1 + numeroNodos izq + numeroNodos der

-- Devuelve el número de hojas (nodos sin hijos) del árbol
-- numeroHojas (Árbol)
numeroHojas :: (Ord a) => Abb a -> Int
numeroHojas Vacio = 0
numeroHojas (Nodo actual Vacio Vacio) = 1
numeroHojas (Nodo actual izq der) = numeroHojas izq + numeroHojas der

-- Devuelve la altura del árbol (número de niveles desde la raíz a la hoja más baja)
-- alturaArbol (Árbol)
alturaArbol :: (Ord a) => Abb a -> Int
alturaArbol Vacio = 0
alturaArbol (Nodo _ izq der) = 1 + max (alturaArbol izq) (alturaArbol der)

-- Devuelve una lista de los nodos en recorrido preorden (raíz, izquierda, derecha)
-- preorden (Árbol)
preorden :: (Ord a) => Abb a -> [a]
preorden Vacio = []
preorden (Nodo actual izq der) = [actual] ++ preorden izq ++ preorden der

-- Devuelve una lista de los nodos en recorrido inorden (izquierda, raíz, derecha)
-- inorden (Árbol)
inorden :: (Ord a) => Abb a -> [a]
inorden Vacio = []
inorden (Nodo actual izq der) = inorden izq ++ [actual] ++ inorden der

-- Devuelve una lista de los nodos en recorrido postorden (izquierda, derecha, raíz)
-- postorden (Árbol)
postorden :: (Ord a) => Abb a -> [a]
postorden Vacio = []
postorden (Nodo actual izq der) = postorden izq ++ postorden der ++ [actual]