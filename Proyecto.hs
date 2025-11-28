
--Tipo de dato para los arboles de n ramas
data Arbol a = Void | Node a [Arbol a]
           deriving (Show, Eq)

-- Definición del tipo Prop
data Prop = 
    Var String |
    Op String | --Se agregan los operadores para poder imprimirlos
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)
    

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Operadores lógicos
(∨), (∧), (→), (↔), (¬):: Prop
(∨) = Op "∨"
(∧) = Op "∧"
(→) = Op "→"
(↔) = Op "↔"
(¬) = Op "¬"


-- Imprimir el tipo de dato Operadores y Variables
instance Show Prop where
  show (Op s) = s
  show (Var p) = p
  show (Cons True) = "Verdadero"
  show (Cons False) = "Falso"
  show (Not p) = "¬" ++ show p
  show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
  show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
  show (Impl p q) = "(" ++ show p ++ "→" ++ show q ++ ")"
  show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Sinónimo para los estados
type Estado = [String]


--Arboles de ejemplo para probar las funciones
testArbol :: Arbol Prop
testArbol = Node (Op "→") [
                Node (Op "∧") [Node p [], Node q []],
                Node (Op "∨") [
                    Node (Op "¬") [Node r []],
                    Node (Op "↔") [Node p [], Node s []]
                ]
            ]
--Estado donde es verdadera para probar "evaluacionForm"  : ["p"]  ["p", "q", "r"]
--Estado donde es falsa para probar "evaluacionForm"  : ["p", "q", "r"]

testArbol2 :: Arbol Prop
testArbol2 =
    Node (Op "↔")
        [ Node (Op "∧")
            [ Node p []
            , Node (Op "∨")
                [ Node q []
                , Node (Op "¬") [Node r []]
                ]
            ]
        , Node (Op "→")
            [ Node (Op "∧")
                [ Node s []
                , Node t []
                ]
            , Node (Op "∨")
                [ Node u []
                , Node (Op "↔") [Node p [], Node r []]
                ]
            ]
        ]

--Estado donde es verdadera para probar "evaluacionForm"  : ["p", "q", "s", "t", "u"]
--Estado donde es falsa para probar "evaluacionForm"  : ["p","r"]


--FUNCIONES 1/2:

--1. Una función que reciba una fórmula de la lógica proposicional y regrese como resultado 
--el árbol de sintaxis abstracta de la fórmula ingresada.
newArbol :: Prop -> Arbol Prop 
newArbol (Var p) = Node(Var p) []
newArbol (Cons b)  = Node  (Cons b) []
newArbol (Not p) = Node (¬) [newArbol(p)]
newArbol ((And p q)) = Node (∧) [newArbol(p),newArbol(q)]
newArbol ((Or p q)) = Node (∨) [newArbol(p),newArbol(q)]
newArbol ((Impl p q)) = Node (→) [newArbol(p),newArbol(q)]
newArbol ((Syss p q)) = Node (↔) [newArbol(p),newArbol(q)]



--2. Una función que reciba un árbol de sintaxis abstracta y devuelva la fórmula de la lógica
--proposicional asociada a dicho árbol.
newFormula :: Arbol Prop -> Prop
newFormula (Node (Var p) [])= (Var p)
newFormula (Node (Cons b) []) = (Cons b)
newFormula (Node (Op "¬") [p]) = Not (newFormula p ) 
newFormula (Node (Op "∧") [p,q]) = And (newFormula p) (newFormula q)
newFormula (Node (Op "∨") [p,q]) = Or (newFormula p) (newFormula q)
newFormula (Node (Op "→") [p,q]) = Impl (newFormula p) (newFormula q)
newFormula (Node (Op "↔") [p,q]) = Syss (newFormula p) (newFormula q)
--Caso que atrapa errores:
newFormula (Node a xs) = error "Argumentos inválidos en newFormula"


--3. Una función que reciba un árbol de sintaxis abstracta y un estado de las variables. 
--Esta función debe usar directamente el árbol de sintaxis abstracta para devolver 
--la evaluación de la fórmula asociada al árbol.
evaluacionForm :: Arbol Prop -> Estado -> Bool
evaluacionForm (Node (Var p) []) xs = pertenece p xs
evaluacionForm (Node (Cons True) []) xs = True
evaluacionForm (Node (Cons False) []) xs = False
evaluacionForm (Node ((¬)) [p]) xs = not (evaluacionForm p xs)
evaluacionForm (Node (Op "∧") [p,q]) xs = (evaluacionForm p xs) && (evaluacionForm q xs)
evaluacionForm (Node (Op "∨") [p,q]) xs = (evaluacionForm p xs) || (evaluacionForm q xs)
evaluacionForm (Node (Op "→") [p,q]) xs = (not (evaluacionForm p xs)) || (evaluacionForm q xs)
evaluacionForm (Node (Op "↔") [p,q]) xs = (evaluacionForm (Node (Op "→") [p,q]) xs) && (evaluacionForm (Node (Op "→") [q,p]) xs)
--Caso que atrapa errores:
evaluacionForm (Node a xs) _ = error "Argumentos inválidos en evaluacionForm"


-- Función auxiliar pertenece: comprueba si un elemento (==) está en una lista
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece a (x:xs) = if a == x then True else pertenece a xs


--Arboles para probar las demas fucniones-----------------------------

testArbol3 :: Arbol Int
testArbol3 =
    Node 10
        [ Node 5  [Node 2 [], Node 7 []]
        , Node 15 [Node 12 [], Node 20 [], Node 1 []]
        ]

testArbol4 :: Arbol Int
testArbol4 = 
    Node 10
        [ Node 4 
            [ Node 2 []
            , Node 3 []
            ]
        , Node 7
            [ Node 6 []
            ]
        , Node 15
            [ Node 12 []
            , Node 20
                [ Node 18 []
                ]
            ]
        ]

--Con este arbol no se puede usar "sumaElementos" porque char no es un tipo Num
arbolChars :: Arbol Char
arbolChars =
    Node 'A'
        [ Node 'B' [Node 'D' []]
        , Node 'C' []
        ]


--FUNCIONES 2/2:


--1. Una función que recibe un árbol y regresa la cantidad de elementos que
--hay en el árbol.
cantidadElementos :: Arbol a -> Int
cantidadElementos Void = 0
cantidadElementos (Node a xs) = 1+ cantidadElementos2 xs

--Función auxiliar para cantidadElementos
--aplica cantidadElementos a una lista de arboles
cantidadElementos2 :: [Arbol a] -> Int
cantidadElementos2 [] = 0
cantidadElementos2 (x:xs) = (cantidadElementos x) + (cantidadElementos2 (xs))


-------------------------------------------------------------------------------------------------


--2. Una función que reciba un árbol y un elemento. El resultado debe ser un booleano que
--indique si el elemento se encuentra o no en el árbol.
busca :: Eq a => Arbol a -> a -> Bool
busca Void _ = False
busca (Node a xs) n = if n == a then True else busca2 xs n

--Función auxiliar para busca
--aplica busca a una lista de arboles
busca2 :: Eq a => [Arbol a] -> a -> Bool
busca2 [] _ = False
busca2 (x:xs) n = if busca x n then True else busca2 xs n


-------------------------------------------------------------------------------------------------


--3. sumaElementos: Una función que reciba un árbol y devuelva la suma de todos sus elementos.
sumaElementos :: Num a => Arbol a -> a
sumaElementos Void = 0
sumaElementos (Node a xs) = a +sumaElementos2 (xs)

--Función auxiliar para sumaElementos 
--aplica sumaElementos a una lista de arboles
sumaElementos2 :: Num a => [Arbol a] -> a
sumaElementos2 [] = 0
sumaElementos2 (x:xs) = (sumaElementos x) + (sumaElementos2 (xs))


-------------------------------------------------------------------------------------------------


--4. preorden / postorden: Una función que reciba un árbol y haga alguno de estos dos recorridos.
--El resultado debe ser una lista con el orden en que se recorrió el árbol.
preorden :: Arbol a -> [a]
preorden Void = []
preorden (Node a xs) = [a] ++ preorden2 xs

--Función auxiliar para aplicar la función preorden a los arboles de una lista
preorden2 :: [Arbol a] -> [a]
preorden2 [] = []
preorden2 (x:xs) = preorden x ++ preorden2 xs


-------------------------------------------------------------------------------------------------


--5. altura: Una función que reciba un árbol y devuleva la altura del árbol.
altura :: Arbol a -> Int
altura Void = 0
altura (Node a xs) = 1 + maximoList (altura2(xs))

--Función auxiliar de altura: Convierte una lista de subárboles en lista de alturas
altura2 :: [Arbol a] -> [Int]
altura2 [] = []
altura2 (x:xs) = altura x : altura2 xs

--Función auxiliar que encuentra el máximo de una lista
maximoList :: [Int] -> Int
maximoList (x:xs) = maximoAux x xs

-- Función auxiliar que compara un número con el máximo de la lista y devuelve el mayor
maximoAux :: Int -> [Int] -> Int
maximoAux n [] = n
maximoAux n (x:xs) = if n >= x then maximoAux n xs else maximoAux x xs                                    


-------------------------------------------------------------------------------------------------


--6. espejo: Una función que reciba un árbol y devuelva el espejo de dicho árbol.
espejo:: Arbol a -> Arbol a
espejo Void = Void
espejo (Node a xs) = Node a (rev(espejo2(xs)))

--Auxiliares:

--Función auxiliar para busca
--aplica busca a una lista de arboles
espejo2 :: [Arbol a] -> [Arbol a]
espejo2 [] = []
espejo2 (x:xs) = append [(espejo x)]  (espejo2 xs )

--Función reversa de las listas
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = append (rev xs) [x]

-- Función para concatenar 2 listas, se usa en reve
append :: [a] -> [a] -> [a]
append [] xs = xs
append (x:xs) ys = x : append xs ys


-------------------------------------------------------------------------------------------------


--7. podar: Una función que recibe un árbol y un número entero n. Se debe regresar el mismo árbol
--pero eliminando todos los subárboles cuyas raíces se encuentren a profundidad n.
podar :: Arbol a -> Int -> Arbol a
podar Void _ = Void
podar _ 0 = Void
podar (Node a xs) n = (Node a (podar2 xs (n-1)))

--Función auxiliar para aplicar la funcion podar a los arboles de una lista
podar2 :: [Arbol a] -> Int -> [Arbol a]
podar2 [] _ = []
podar2 (x:xs) n = podar x n : podar2 xs n


-------------------------------------------------------------------------------------------------


--8. elementosProfundidad: Una función que recibe un árbol y un número entero n. Se debe
--regresar una lista con todos los elementos que se encuentran a profundidad n.

elementosProfundidad :: Arbol a -> Int -> [a] 
elementosProfundidad Void _ = [] 
elementosProfundidad (Node a xs) 0 = [a] 
elementosProfundidad (Node _ xs) n = profundidad2 xs (n-1)

--Función auxiliar para aplicar la funcion elementosProfundidad a los arboles de una lista
profundidad2 :: [Arbol a] -> Int -> [a]
profundidad2  [] _ = []
profundidad2  (x:xs) n = elementosProfundidad x n ++ profundidad2 xs n


