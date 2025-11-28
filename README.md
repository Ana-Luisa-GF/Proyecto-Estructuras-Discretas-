# Proyecto-Estructuras-Discretas-

# Integrantes:
Nombre:Gómez Ferro Ana Luisa
No.de cuenta:322165566

Nombre: Martínez Martínez Marco Antonio
No. de cuenta:423116344

Nombre: Vargas Haquet Austin Brayan
No. de cuenta:423088153

# Justificación y explicación detallada el porqué de la implementación de los Arboles.

La manera en que definimos a los arboles de n ramas es la siguiente:

data Arbol a = Void | Node a [Arbol a]
           deriving (Show, Eq)

La definición de los arboles esta basada en la vista en clase, tomando Void como la reprecentación de un arbol vacío, y usamos la función constructiva Node para establecer la forma de un árbol no vacío.

En cuanto a este segundo caso la declaración del dato muestra esto:
 Node a [Arbol a]
 
Lo que aquí dice es que para construir un árbol no vacío de tipo "a" necesitas:  un elemento de tipo a y una lista con arboles. 

El elemento de tipo a es necesario para crear el nodo del árbol,
y al pedir una lista con arboles puedes crear cuantas ramas quieras, pues las listas permites agrupar n elementos de cualquier tipo, en esta caso arboles.
