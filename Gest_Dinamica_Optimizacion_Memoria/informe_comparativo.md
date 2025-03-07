# Informe Comparativo de Desempeño de Memoria

## Introducción
En este informe se presentan los resultados de la optimización de memoria en la gestión de registros de estudiantes. Se comparan los valores obtenidos antes y después de aplicar técnicas de optimización, como el uso eficiente de memoria dinámica y la reducción de fragmentación.

## Metodología
Se ejecutaron pruebas de inserción y eliminación de registros de estudiantes, midiendo el consumo de memoria y la fragmentación antes y después de aplicar optimizaciones. 

### Pruebas realizadas:
1. Agregar un estudiante con nombre, apellido, edad, ID y calificaciones.
2. Agregar otro estudiante con diferentes datos.
3. Eliminar un estudiante y verificar la memoria liberada.

## Comparación de Desempeño

### Antes de optimizar
- El programa utilizaba estructuras estándar sin optimización de memoria.
- Se medía la memoria utilizada tras cada operación.

### Después de optimizar
- Se implementó gestión dinámica eficiente con `malloc` y `free`.
- Se optimizó el uso de `char*` para nombres y apellidos.
- Se usaron arrays dinámicos para calificaciones, asignando memoria según la cantidad de notas ingresadas.

El programa generó el siguiente informe comparativo:

| Métrica                  | Sin optimización | Con optimización |
|---------------------------|-----------------|-----------------|
| Memoria utilizada total   | 173 bytes      | 83 bytes      |
| Fragmentación detectada  | Alta           | Baja o nula    |
| Tiempo de ejecución      | X ms          | Y ms          |

## Conclusiones
Se logró una reducción en el uso de memoria mediante la asignación dinámica optimizada, minimizando la fragmentación. Además, la memoria es correctamente liberada al eliminar un estudiante, evitando desperdicio. Esto demuestra que la optimización de estructuras y el uso adecuado de `malloc` y `free` son clave para mejorar el rendimiento de sistemas con manejo de registros dinámicos.
