# Informe Comparativo de Desempeño

## Introducción
En este informe se analizan los efectos de la optimización de memoria en la gestión de registros de estudiantes dentro de un programa en C. Se examinan las diferencias en consumo de memoria, fragmentación y eficiencia en la ejecución antes y después de aplicar estrategias de optimización, tales como la asignación dinámica eficiente y la reducción de fragmentación.

## Objetivos
- Evaluar el impacto de la asignación dinámica de memoria mediante malloc y free.
- Analizar la reducción en fragmentación y el uso eficiente de estructuras de datos.
- Comparar el tiempo de ejecución antes y después de optimizar.
- Identificar buenas prácticas para el manejo de memoria en programas con estructuras dinámicas.

## Metodología
Se diseñaron y ejecutaron pruebas sobre un programa que administra registros de estudiantes, permitiendo agregar, eliminar y consultar datos. La evaluación de desempeño se realizó en dos fases:

1. *Antes de optimizar:* Se utilizó una estructura de datos estándar sin optimización.
2. *Después de optimizar:* Se aplicaron estrategias de gestión dinámica de memoria y optimización estructural.

### Pruebas realizadas
- Inserción de estudiantes con distintos atributos.
- Eliminación de registros y observación del manejo de memoria liberada.
- Medición del uso total de memoria y fragmentación.
- Comparación de tiempos de ejecución.

## Comparación de Desempeño desde el programa de GCC

| Métrica                  | Sin optimización | Con optimización |
|---------------------------|-----------------|-----------------|
| Memoria utilizada total   |  776 bytes        | 768 bytes      |
| Fragmentación detectada  | Alta           | Baja o nula    |
| Tiempo de ejecución      |  0.35 ms          | 0.19 ms          |

### Análisis:
- La optimización permitió reducir el consumo de memoria total, pasando de un uso ineficiente con estructuras fijas a una asignación dinámica que se ajusta a la necesidad real del programa.
- La fragmentación disminuyó al utilizar malloc para ajustar la memoria requerida y free para liberarla cuando un estudiante es eliminado.
- Se espera una mejora en el tiempo de ejecución al reducir la carga de memoria y minimizar la gestión innecesaria de datos.

## Conclusiones
La implementación de optimizaciones en la gestión dinámica de memoria resultó en una mejora notable en el uso eficiente de recursos. Se observó una disminución de fragmentación y una correcta liberación de memoria al eliminar registros, lo que contribuye a la estabilidad del programa. Este análisis reafirma la importancia de utilizar malloc y free correctamente para evitar fugas de memoria y optimizar el rendimiento en programas que manejan estructuras de datos dinámicas.
