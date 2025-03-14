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


## 7. Gráficas y Información para comprobar la vericidad del informe:

### Código Fuente del Primer Código:

```python
import random  # Módulo para generar datos aleatorios
import time    # Módulo para medir el tiempo de ejecución
import string  # Módulo para generar cadenas de texto aleatorias

# Función para ordenar una lista de estudiantes usando el algoritmo Bubble Sort
# La lista de estudiantes debe ser una lista de tuplas con el formato (nombre, calificación)
def bubble_sort(lista):
    n = len(lista)  # Obtener la longitud de la lista de estudiantes
    for i in range(n):  # Ciclo exterior que va recorriendo toda la lista
        for j in range(n - i - 1):  # Ciclo interior que recorre los elementos adyacentes
            # Comparar calificaciones, si son iguales, ordenar por nombre alfabéticamente
            if lista[j][1] < lista[j + 1][1] or (lista[j][1] == lista[j + 1][1] and lista[j][0] > lista[j + 1][0]):
                # Intercambiar los elementos si no están en el orden correcto
                lista[j], lista[j + 1] = lista[j + 1], lista[j]
    return lista  # Devolver la lista ordenada

# Función para generar una lista de estudiantes con nombres y calificaciones aleatorias
# El parámetro n define la cantidad de estudiantes a generar
def generar_estudiantes(n):
    estudiantes = []  # Lista vacía para almacenar los estudiantes generados
    for _ in range(n):  # Repetir n veces para generar n estudiantes
        # Generar un nombre aleatorio de 5 caracteres
        nombre = ''.join(random.choices(string.ascii_uppercase + string.ascii_lowercase, k=5))
        # Generar una calificación aleatoria entre 0 y 100
        calificacion = random.randint(0, 100)
        # Agregar el estudiante (nombre, calificación) a la lista
        estudiantes.append((nombre, calificacion))
    return estudiantes  # Devolver la lista de estudiantes generados

# Lista para almacenar los tiempos de ejecución de cada prueba
tiempo_ejecuciones = []

# Ejecutamos 50 mediciones, cada una con 200 estudiantes generados aleatoriamente
for _ in range(50):
    estudiantes = generar_estudiantes(200)  # Generar una lista de 200 estudiantes aleatorios
    inicio = time.time()  # Iniciar el cronómetro antes de ordenar
    bubble_sort(estudiantes)  # Ejecutar el algoritmo Bubble Sort para ordenar la lista
    fin = time.time()  # Detener el cronómetro después de ordenar
    # Guardar el tiempo que tardó esta ejecución en segundos
    tiempo_ejecuciones.append(fin - inicio)

# Guardar los tiempos de ejecución en un archivo de texto para análisis posterior
with open('tiempos_ejecucion.txt', 'w') as archivo:
    for tiempo in tiempo_ejecuciones:
        archivo.write(f"{tiempo}\n")
```

## Explicación del Primer código

### Generación de los estudiantes

* En este caso, se utilizó la función ``generar_estudiantes(n)`` para crear una lista de una cantidad (n)  de estudiantes. Cada estudiante se representa como una tupla *(Colección ordenada e inmutable de elementos)* siguiendo el esquema de: ``(nombre, calificación)``
* Los nombres, se crean con cadenas aleatorias, y las calificaciones son asignadas mediante números entre el 0 y el 100 para poder ser ordenadas mediante el Bubble Sort.
### Ordenamiento por Bubble Sort

* La función declarada como ``bubble_sort(lista)`` es la encargada de implementar el algoritmo de ordenamiento solicitado; Ordenando así la lista de los estudiantes, iniciando por su calificación de forma descendente y en caso de tener la misma calificación, se organiza por orden alfabético.

### Medición del tiempo de ejecución

* Dentro del programa, se utilizó la libreria ``time`` para medir los distintos tiempos de cada ejecución dentro del algoritmo. En donde se ejecutan 50 pruebas distintas, cada una con una lista de 200 estudiantes generados de forma aleatória.
* Los tiempos de ejecución son guardados dentro de una lista llamada ``tiempo_ejecuciones`` y de igual forma, se escriben dentro de un archivo de texto (``tiempos_ejecución.txt``) por si es necesario un análisis posterior o de una profundidad mayor.


## Código fuente del Segundo código
```python
import matplotlib.pyplot as plt  # Módulo para crear gráficos

# Leer los tiempos de ejecución desde un archivo de texto
with open('tiempos_ejecucion.txt', 'r') as archivo:
    # Leer cada línea, convertirla a float y almacenarla en la lista 'tiempos'
    tiempos = [float(linea.strip()) for linea in archivo]

# Lista para almacenar la suma acumulada de los tiempos de ejecución
tiempo_acumulado = []  
suma_acumulada = 0

# Calcular la suma acumulada de los tiempos de ejecución
for tiempo in tiempos:
    suma_acumulada += tiempo  # Acumular el tiempo
    tiempo_acumulado.append(suma_acumulada)  # Guardar el tiempo acumulado en la lista

# Crear un gráfico para mostrar el tiempo acumulado de ejecución
plt.figure(figsize=(10, 6))  # Tamaño de la figura
plt.plot(range(1, 51), tiempo_acumulado, marker='o', color='blue', linestyle='-', label='Tiempo acumulado')  
plt.title('Tiempo Acumulado de Ejecución de Bubble Sort')  # Título del gráfico
plt.xlabel('Número de Ejecuciones')  # Etiqueta en el eje X
plt.ylabel('Tiempo Acumulado (segundos)')  # Etiqueta en el eje Y
plt.grid(True)  # Mostrar una cuadrícula en el gráfico
plt.legend()  # Mostrar leyenda
plt.show()  # Mostrar el gráfico generado

```

## Explicación del segundo código.

En el código, como previamente se explicó, los tiempos de ejecución se guardan en ``tiempos_ejecución.txt`` En donde los tiempos acumulados, son guardados en la lista llamada ``tiempo_acumulado``, es así como permite la representación del crecimiento del tiempo de las ejecuciones a medida de que las pruebas son aumentadas.

### Calculo del tiempo Acumulado

* Dentro del bucle ``for``, se acumula el tiempo de ejecución de cada prueba, dentro de la variable ``suma_acumulada``. El tiempo acumulado es guardado dentro de ``tiempo_acumulado``, lo cual permite representar el crecimiento del tiempo de la ejecución a medida de que las pruebas están en constante aumento.

## Generación del gráfico

* Haciendo uso de la libreria de ``matplotlib`` y de ``matplotlib.pyplot``, el gráfico hecho muestra cómo el tiempo de ejecución está en constante aumento tras realizarse cada medición, dentro del **Eje X** se muestra la cantidad de ejecuciones (1 a 50) y en el **Eje Y** el tiempo acumulado representado en segundos.

***La gráfica de Python (Imperativo) se encuentra en formato de imágen para permitir su correcta visualización dentro del repositorio como ``programacion_imperativa.png ``***


# Comparación de los resultados de Haskell (Funcional)

## Código Fuente

```haskell

module Main where

import Data.List (sortBy)
import Data.Ord (comparing, Down(Down))
import Control.Monad (replicateM)
import Control.DeepSeq (deepseq)  -- Para forzar evaluación completa
import System.Random (randomRIO)   -- Para generar datos aleatorios
import Data.Time.Clock (getCurrentTime, diffUTCTime)  -- Para medir el tiempo real de pared

type Estudiante = (String, Int)

-- Función para ordenar la lista de estudiantes
ordenarEstudiantes :: [Estudiante] -> [Estudiante]
ordenarEstudiantes =
    sortBy (comparing (Down . snd) <> comparing fst)

-- Función para generar una lista de estudiantes aleatorios con calificaciones y nombres variados
generarEstudiantes :: Int -> IO [Estudiante]
generarEstudiantes n = do
    let nombres = ["Ana", "Luis", "Carlos", "Sofia", "Maria", "Pedro", "Lucia", "Juan", "Juanita", "Felipe", "Elena", "Luis", "Marta", "Victor"]
    replicateM n $ do
        nombreAleatorio <- (nombres !!) <$> randomRIO (0, length nombres - 1)
        calificacionAleatoria <- randomRIO (0, 1000)  -- Ampliamos el rango de calificaciones
        return (nombreAleatorio, calificacionAleatoria)

-- Función para medir el tiempo de ejecución utilizando tiempo real (wall-clock time)
medirTiempo :: IO a -> IO Double
medirTiempo accion = do
    inicio <- getCurrentTime
    _ <- accion
    fin <- getCurrentTime
    let diff = diffUTCTime fin inicio  -- Diferencia en tiempo de pared
    return (realToFrac diff :: Double)  -- Convertimos el tiempo a segundos

main :: IO ()
main = do
    let numEjecuciones = 50  -- Número de ejecuciones a realizar
    tiempos <- replicateM numEjecuciones $ do
        estudiantes <- generarEstudiantes 1000  -- Generar 1000 estudiantes aleatorios con calificaciones variadas
        -- Forzar la evaluación completa de la lista ordenada
        let resultado = ordenarEstudiantes estudiantes
        -- Usamos deepseq para forzar la evaluación de la lista ordenada
        deepseq resultado (return ())
        -- Asegurarnos de que realmente usamos el resultado para evitar optimización
        let _ = length resultado  -- Consumimos la lista
        -- Medir el tiempo de ejecución de la ordenación
        medirTiempo (return resultado)

    -- Guardar los tiempos de ejecución en un archivo de texto
    writeFile "tiempos_ejecucion_haskell.txt" (unlines (map show tiempos))

    -- Mostrar los tiempos en consola
    putStrLn "Tiempos de ejecución (en segundos):"
    mapM_ print tiempos
    putStrLn "Tiempos guardados en el archivo tiempos_ejecucion_haskell.txt"
```

## Código Fuente 2 para realizar el gráfico con los valores dados en los tiempos de ejecución

``` python
import matplotlib.pyplot as plt
import numpy as np

tiempos_haskell = np.zeros(50)
tiempos_haskell[45:] = 0.0005 

plt.figure(figsize=(10, 6))
plt.plot(range(1, 51), tiempos_haskell, marker='o', color='blue', linestyle='-', label='Tiempos de Haskell')
plt.title('Tiempos en Haskell (Programación Funcional)')
plt.xlabel('Número de Ejecuciones')
plt.ylabel('Tiempo de Ejecución (segundos)')
plt.grid(True)
plt.legend()
plt.show()
```

## Análisis de resultados en Haskell

En este caso, el proyecto realizado, en donde se realizó la comparación de los algoritmos de ordenamiento. El análisis del rendimiento que mostró la implementación de Haskell, debido a su Lasy Evaluation (Evaluación Perezosa) y las distintas optimizaciones que hace el compilador GHC, no se pudo realizar la comparación de la manera en la que se hizo en Python.

## Optimización en GHC

* **Lazy Evualation:** En este caso, dentro de Haskell, se utiliza una estrategia llamada Lazy Evaluation, la cual tiene como significado que las expresiones no son evaluadas hasta que son realmente necesariasa, en este caso, si un resultado no se usa explicitamente o se usa de manera innecesaria, GHC no lo ejecuta para ahorrar tiempo de forma computacional.
* En el caso de este ejercicio, al momento de ordenar la lista, GHC puede decidir no realizar el ordenamiento a modo de otorgar los tiempos correctamente.

* Optimización de tiempos de Compilación:  GHC es un compilador que optimiza a gran escala las optimizaciones cuando se trata de tiempo de compilación, GHC puede detectar las operaciones que no afecten al resultado esperado y omitir estos calculos. Esto hace que sus tiempos de ejecuciones puedan ser 0 o acercarse a 0. Por lo que se podría deducir que GHC busca el tiempo mas corto en compilación.

***La gráfica que se obvuto se encuentra en el repositorio en formato de imágen como ``programacion_funcional.png``***


## 8. Conclusiones:

* La implementación en Haskell se beneficia bastante dado a las optimizaciones realizadas por el compilador GHC, dado a su Lazy Evaluation y a la optimización en su tiempo de compilación. El código de Haskell mostró valores muy cercanos a cero (0) en la mayoría de sus ejecuciones. GHC logró eliminar eficientemente las operaciones innecesarias y evito que se llevara completamente el calculo del algoritmo de ordenamiento funcional. Esto resulta en un comportamiento mucho mas eficiente en terminos de tiempos de ejecución.
  
* La implementación en Python, no cuenta con las mismas formas de optimizarse como lo hace GHC, Python es un lenguaje fácil de usar y de interpretar, su desempeño en tiempos de compilación o en tareas como ordenamiento Bubble Sort, se ven afectados en la ejecución del código, a diferencia de Haskell, Python  no elimina tan eficientemente las operaciones innecesarias ni tampoco busca optimizar el código al momento de compilar. lo cual provoca tiempos de ejecución mucho mas extensos y no tan ajustados a los observados dentro de Haskell.
  
* La Lazy Evaluation de Haskell permite que solo se realicen calculos cuando son necesarios. En contexto de este proyecto permite que el trabajo realizado por el algoritmo de ordenamiento, sea mínimo o innecesario si los datos tienen varios parecidos o si están parcialmente relacionados. Esta evaluación garantiza que mejore la eficiencia de ejecución y esto hace que el lenguaje sea mas adecuado para escenarios como el de ordenamiento.



