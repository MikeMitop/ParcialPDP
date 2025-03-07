# Comparación de Paradigmas de Programación

Dentro de este documento, se llevará a cabo el análisis de los dos enfoques implementados para el ordenamiento de estudiantes según sus calificaciones. Es decir:

***Programación Imperativa en Python*** y ***Programación Funcional en Haskell***

A través de distintos conceptos y análisis, se tomará en cuenta sus diferencias, ventajas, desventajas al hablar de claridad, expresividad, mutabilidad, manejo del estado y eficiencia.

## 1. Comparación de Claridad y Legibilidad del código.

La **Programación Imperativa** (Python) tiende a ser mas intuitiva al momento de su lectura, ya que esta contiene un flujo de control explícito mediante ciclos y asignaciones. Refleja de manera directa lo que se debe hacer para ordenar la lista dentro de los bucles anidados, dentro del punto de vista de un progamador principiante, es mas fácil entenderlo por el como está construido y el lenguaje que usa Python.

Por otro lado, en el caso de la **Programación Funcional** (Haskell) se adapta hacia un enfoque mas declarativo, en donde se describe lo que se quiere hacer/lograr sin necesidad de profundizar un paso a paso. En lugar de usar bucles, se utilizan funciones como en este caso, ``sortBy`` y ``comparing``. Es así como el codigo logra ser mas conciso y expresivo, por otro lado, este código puede ser mas tedioso al momento de entenderlo debido a que utiliza funciones complejas para lograr el correcto funcionamiento.

### Ejemplos de Diferencia:

### Programación Imperativa (Python)

``` python
for i in range(n):
    for j in range(n - i - 1):
        if lista[j][1] < lista[j + 1][1] or (lista[j][1] == lista[j + 1][1] and lista[j][0] > lista[j + 1][0]):
            lista[j], lista[j + 1] = lista[j + 1], lista[j]
```

### Programación Funcional (Haskell) 

``` haskell
ordenarEstudiantes = sortBy (comparing (Down . snd) <> comparing fst)
```

Dentro de estos ejemplos, se puede visualizar las diferencias entre uno y otro, en este caso la principal diferencia es que al momento de implementar el ordenamiento en Haskell, este se realiza en una sola línea mediante funciones complejas, en cambio, en Python únicamente se necesita un ciclo anidado para lograr una ejecución similar.

## 2. Nivel de Expresividad y Abstracción.

El código funcional (Haskell) es más expresivo dado a que permite la definición de operaciones y funciones de alto nivel sin necesidad de detallar lo implementado. Es así como ``sortBy`` encapsula la lógica de ordenamiento sin necesidad de estructuras repetitivas o de códigos de muchas líneas.

En cambio, al hablar del código imperativo, requiere una implementación manual del algoritmo dado, lo que hace que el código sea aún mas detallado pero sea menos reutilizable, es por eso mismo que reduce la abstracción del algoritmo.

## 3. Manejo de Estructura de Datos (Mutabilidad vs Inmutabilidad)

   **- Programación Imperativa:** Usa estructuras mutables, donde la lista se modifica durante el proceso de ordenamiento
   **- Programación Funcional:** Usa estructuras inmutables, lo que quiere decir que la lista original no se altera, sino que se genera una nueva lista con los datos ya ordenados.

   En resumen, la mutabilidad en Python permite realizar distintas modificaciones directamente a la memoria, lo cual resulta util en muchos casos, pero puede generar errores al momento de hacer el debugging. En Haskell, la inmutabilidad mejora la seguridad y previsibilidad del código, evitando así errores o efectos inesperados.

## 4. Manejo de Estado en Cada Paradigma:

Dentro del Enfoque imperativo, se maneja el estado explícitamente mediante distintas variables que cambian de valor en las iteraciones a realizar, dentro de Python se declara ``lista[j]`` y ``lista[j+1]`` donde se van intercambiando continuamente, lo cual es un ejemplo claro de una actualización del estado tras cada iteración hecha.

Por otro lado, en el paradigma funcional, el estado no se refleja cambiado de forma explícita, sino que las funciones regresan nuevas estructuras de datos sin realizar modificaciones en las iniciales o originales. Haskell aprovecha la inmutabilidad y el uso de funciones puras, lo que facilita la depuración en la gran mayoria de casos.

## 5. Facilidad de Mantenimiento y Extensión:

El código funcional suele ser más fácil de mantener porque evita los efectos secundarios, tal como se mencionó previamente mediante funciones modulares y reutilizables. Dentro de Haskell, es suficiente con solo modificar la función ``comparing`` dentro de ``sortBy`` para cambiar el criterio de ordenamiento en Haskell.

Por parte del código imperativo, con el simple hecho de modificar el criterio de ordenamiento, puede tener implicaciones de alteración de la lógica dentro de los bucles que se utilizan, lo que puede hacer que el código se extienda o que sea propenso a generar errores o dificultad al momento de comprender lo que se hace.

## 6. Eficiencia de Cada Solución:

### Imperativo 
En este caso, para la solución se utilizó el algoritmo de ordenamiento **"Bubble Sort"** 
Basandonos en esto, dentro de la página de geeksforgeeks.org nos dice lo siguiente:

***"Bubble Sort is typically used for small datasets or when you want a simple sorting algorithm."*** (GeekForGeeks, 2025)

Lo que quiere indicar que al momento de usar grandes volúmenes de datos, este puede ser ineficiente o incluso puede ser tedioso trabajar con el mismo.
Por otro lado, se comenta que:

***"It is not suitable for large datasets because of its O(n^2) time complexity."*** (GeekForGeeks, 2025) 

En este caso, se indica que no es adecuado para grandes volumenes de datos dado a su complejidad de O(n^2)

### Funcional

Dentro de la programación funcional, se usó ``sortBy``, que internamente implementa QuickSort o MergeSort. Logrando una complejidad promedio de O(n log n), lo que es significativamente mas eficiente.  

Nos podemos respaldar mediante la siguiente información:

***"The time complexity of Quick Sort is O(n log n) on average case, but can become O(n^2) in the worst-case. The space complexity of Quick Sort in the best case is O(log n)"*** (GeeksForGeeks, 2024)


## 7. Conclusiones:

Como conclusión, se puede decir que Python, usando **"Bubble Sort"** Resulta ser mas adecuada para la enseñanza dado a sus conceptos básicos de algoritmia y estructuras de control, mientras que en el caso de Haskell, usando ``sortBy`` resulta ser mas eficiente y modular dado a su inmutabilidad y uso de las funciones avanzadas. Sin embargo no necesariamente quiere decir que la programación funcional sea la mejor en estos casos, todo va a depender del contexto y los requisitos de uso que requiera el programa.
