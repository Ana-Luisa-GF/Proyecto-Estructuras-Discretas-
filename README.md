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

La definición de los arboles esta basada en la vista en clase con los arboles binarios
El constructor Void representa un árbol vacío, lo cual sirve como caso base y permite detener la recursión en varias funciones.

El constructor Node modela un árbol no vacío. Su forma es:

Node a [Arbol a]

indica que un nodo contiene dos elementos:

Un valor de tipo a, que corresponde a la información almacenada en la raíz del nodo.

Una lista de subárboles, [Arbol a], que permite representar un número arbitrario de hijos.

Elegimos una lista para los hijos porque permite construir árboles con cero, uno o muchos hijos sin necesidad de declarar un número fijo de ramas, lo que permite representar árboles generales (no necesariamente binarios).
