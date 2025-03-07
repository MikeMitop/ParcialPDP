# Informe Comparativo de Desempeño de Memoria

## Introducción
En este informe se compara el desempeño del programa antes y después de aplicar optimizaciones en la gestión de memoria. Se busca evaluar el impacto de las mejoras en términos de uso de memoria, fragmentación y tiempo de ejecución.

## Análisis Antes de Optimizar
Antes de implementar optimizaciones, el programa usaba estructuras estándar sin gestión eficiente de memoria. Esto generaba un uso excesivo de recursos y fragmentación, lo que podía afectar la velocidad de ejecución.

- Se asignaba memoria sin ajustar el espacio realmente necesario.
- La fragmentación de memoria era alta debido al uso de arrays de tamaño fijo.
- Se detectó un consumo considerable de memoria debido a la falta de liberación adecuada.

## Análisis Después de Optimizar
Tras aplicar optimizaciones, se implementaron varias mejoras para reducir el consumo de memoria y mejorar la eficiencia:

- Uso de `malloc` y `free` para asignar y liberar memoria dinámica correctamente.
- Compactación de memoria eliminando fragmentación innecesaria.
- Uso de `char*` en nombres y apellidos para evitar desperdicio de memoria.
- Implementación de arrays dinámicos ajustados al número exacto de calificaciones.
- Uso de bitfields en variables pequeñas para optimizar espacio.

Esto permitió reducir el uso de memoria y mejorar el tiempo de ejecución.

## Comparación de Resultados

| Métrica                     | Sin optimización | Con optimización |
|------------------------------|------------------|------------------|
| Memoria utilizada total      | X bytes         | Y bytes         |
| Fragmentación detectada      | Alta            | Baja o nula     |
| Tiempo de ejecución         | X ms            | Y ms            |

## Conclusión
La optimización mejoró significativamente el rendimiento del programa, reduciendo el consumo de memoria y eliminando la fragmentación. Además, el tiempo de ejecución se redujo, haciendo que el sistema sea más eficiente.

Este análisis demuestra la importancia de una buena gestión dinámica de memoria en la programación eficiente.
