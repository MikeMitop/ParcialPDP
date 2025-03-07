# Enfoque 1: Ordenamiento de Estudiantes con Bubble Sort

### Descripción del Programa:

Este programa implementa el algoritmo de ordenamiento **"Bubble Sort"** en Python para organizar una lista de estudiantes teniendo en cuenta sus calificaciones. Se siguen los siguientes parametros dentro del ordenamiento:

* Los estudiantes son ordenados en orden descendente según su calificación
* En caso de que dos o más estudiantes tengan la misma calificación, se ordenan alfabéticamente por su nombre.

## Funcionamiento del Programa:

Como se mencionó previamente, el programa utiliza el algoritmo de **"Bubble Sort"** El cual es el encargado de comparar los elementos en la lista y en tal caso de estar ordenados de manera erronea, los intercambia. Este proceso se repite hasta que cumpla con las condiciones, es decir, hasta que esté ordenado.

## Código Fuente:

``` python
def bubble_sort(lista):
    n = len(lista)
    for i in range(n):
        for j in range(n - i - 1):
            if lista[j][1] < lista[j + 1][1] or (lista[j][1] == lista[j + 1][1] and lista[j][0] > lista[j + 1][0]):
                lista[j], lista[j + 1] = lista[j + 1], lista[j]
    return lista

estudiantes = [
    ('Ana',85),
    ('Luis',90),
    ('Carlos',85),
    ('Sofia',92),
    ('Maria',90)
]
print(bubble_sort(estudiantes))
 ``` 

## Entrada y Salida esperada
### Entrada:

 ``` python
estudiantes = [
    ("Ana", 85),
    ("Luis", 90),
    ("Carlos", 85),
    ("Sofía", 92),
    ("María", 90)
]

```

## Salida: 

``` python
[
    ("Sofía", 92),
    ("Luis", 90),
    ("María", 90),
    ("Ana", 85),
    ("Carlos", 85)
]
```


## Explicación del Algoritmo:

1. Se recorre la lista varias veces.
2. Tras cada iteración, se comparan los elementos.
3. Si el primer estudiante tiene menor calificación que el segundo, se intercambian
4. Si tienen la misma calificación, se busca si existe alguna diferencia alfabéticamente para ordenarlos.
5. Este proceso se repite hasta que la lista esté totalmente ordenada.


# Enfoque 2:  Ordenamiento Funcional de Estudiantes en Haskell

## Descripción del Programa:

Este programa hace uso del algoritmo de ordenamiento utilizando un enfoque **funcional** dentro del lenguaje de Haskell. Se ordena una lista de estudiantes siguiendo estos criterios:

- Orden descendente según la calificación del estudiante
- En caso de tener la misma calificación, los estudiantes se ordenan alfabéticamente por su nombre.

*En este caso, el enfoque resulta ser distinto, dado a que se usa una* **abstracción declarativa** *donde se define lo que se desea lograr sin especificar el paso a paso*

## Funcionamiento:

Se utiliza la función ``SortBy`` que se obtiene de importar ``Data.List`` la cual permite definir un criterio de ordenamiento personalizado. De la misma forma, se aprovechan funciones como podrían serlo ``Conmparing`` al momento de importar ``Data.Ord``

## Código Fuente:

``` haskell

import Data.List (sortBy) 
import Data.Ord (comparing, Down(Down))

type Estudiante = (String, Int)

ordenarEstudiantes :: [Estudiante] -> [Estudiante]
ordenarEstudiantes = sortBy (comparing (Down . snd) <> comparing fst)

estudiantes :: [Estudiante]
estudiantes = 
    [("Ana", 85)
    , ("Luis", 90)
    , ("Carlos", 85)
    , ("Sofia", 92)
    , ("Maria", 90)
    ]

main :: IO ()
main = print $ ordenarEstudiantes estudiantes

```

## Entrada y Salida Esperada: 

### Entrada:

``` haskell
estudiantes =
    [ ("Ana", 85)
    , ("Luis", 90)
    , ("Carlos", 85)
    , ("Sofia", 92)
    , ("Maria", 90)
    ]
```

### Salida:

``` haskell
estudiantes =
    [ ("Ana", 85)
    , ("Luis", 90)
    , ("Carlos", 85)
    , ("Sofia", 92)
    , ("Maria", 90)
    ]
```

## Explicación del Algoritmo:

1. ``SortBy`` Es la encargada del ordenamiento sin necesidad de agregar bucles explícitos.
2. ``comparing (Down . snd)`` Es la encargada de ordenar las calificaciones en orden descendente.
3. ``comparing fst`` Ordena alfabéticamente en caso de tener dos calificaciones identicas, como es el caso.
4. El operador ``<>`` combina ambas condiciones del ordenamiento.


