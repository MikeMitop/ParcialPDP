# Ordenamiento de Estudiantes con Bubble Sort

### Descripción del Programa:

Este programa implementa el algoritmo de ordenamiento **"Bubble Sort"** en Python para organizar una lista de estudiantes teniendo en cuenta sus calificaciones. Se siguen los siguientes parametros dentro del ordenamiento:

* Los estudiantes son ordenados en orden descendente según su calificación
* En caso de que dos o más estudiantes tengan la misma calificación, se ordenan alfabéticamente por su nombre.

## Funcionamiento del Programa:

Como se mencionó previamente, el programa utiliza el algoritmo de **"Bubble Sort"** El cual es el encargado de comparar los elementos en la lista y en tal caso de estar ordenados de manera erronea, los intercambia. Este proceso se repite hasta que cumpla con las condiciones, es decir, hasta que esté ordenado.

## Código Fuente:

```
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

 ```
estudiantes = [
    ("Ana", 85),
    ("Luis", 90),
    ("Carlos", 85),
    ("Sofía", 92),
    ("María", 90)
]

```

## Salida: 

```
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
   
