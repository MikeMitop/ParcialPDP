
# Simulador de Máquina de Turing

## Proyecto Paradigmas de Programación – Universidad Sergio Arboleda

**Autores:** Miguel Celis, Santiago Rodríguez y Juan Barrera  
**Fecha:** Bogotá D.C., 2025

---

## Descripción General

Este proyecto implementa una **Máquina de Turing** en Python que simula operaciones matemáticas **básicas** y **avanzadas**, tal como lo exige el proyecto del primer corte de la materia "Paradigmas de Programación" de la Universidad Sergio Arboleda.

Se utiliza una clase `MaquinaTuring` que simula el comportamiento de la cinta, los estados y la cabeza de lectura/escritura. Las operaciones básicas (suma, resta, multiplicación, división) se implementan a través de transiciones explícitas. Las operaciones avanzadas (potencia, raíz cuadrada, logaritmo y seno) usan funciones matemáticas de Python, pero se documentan como parte del diseño de una M.T. extendida.

---

## Estructura del Proyecto

### Clase Principal: `MaquinaTuring`

- **`__init__`**: Inicializa cinta, posición del cabezal, estado inicial y símbolo blanco.
- **`set_transition_function(func)`**: Asigna una tabla de transiciones.
- **`step()`**: Ejecuta un solo paso según las transiciones.
- **`run(max_steps)`**: Corre hasta alcanzar el estado final o el máximo de pasos.
- **`print_tape()`**: Imprime el contenido final de la cinta y el resultado decimal.

---

## Operaciones Básicas

### `suma(a, b)`
Simula la suma unaria de dos números representados como símbolos `a` y `b` que son convertidos en `1`.

### `resta(a, b)`
Simula la resta marcando y eliminando `b` unos de una secuencia de `a` unos, utilizando marcadores `X` y limpieza de símbolos.

### `multiplicacion(a, b)`
Simula la multiplicación como suma repetida utilizando múltiples estados que recorren y replican los `b` unos por cada `a`.

### `division(a, b)`
Se basa en restas sucesivas. Calcula el cociente y el resto simulando un proceso repetitivo de eliminación.

---

## Operaciones Avanzadas

> Aunque algunas se apoyan en librerías matemáticas, La gran mayoria se usa mediante transisiones.

### `potencia(a, b)`
Simula la potenciación como multiplicaciones repetidas.

### `raiz_cuadrada(a)`
Aproxima la raíz cuadrada buscando el mayor entero cuyo cuadrado no supere a `a`.

### `logaritmo(a)`
Calcula una aproximación del logaritmo natural usando el algoritmo inverso de exponenciación.

### `seno(x)`
Calcula el seno de un ángulo (en radianes o grados), apoyado en `math.sin()` pero enmarcado como una simulación de una serie de Taylor.

---

## Menús Interactivos

- `menu_operaciones_basicas()`: Ejecuta suma, resta, multiplicación o división según la opción ingresada.
- `menu_operaciones_avanzadas()`: Ejecuta potencia, raíz, logaritmo o seno.
- `main()`: Muestra el menú principal y permite seleccionar entre operaciones básicas o avanzadas.


---

## Créditos

Trabajo académico presentado por:
- Miguel Celis
- Santiago Rodríguez
- Juan Barrera

Materia: Paradigmas de Programación  
Universidad Sergio Arboleda – 2025
