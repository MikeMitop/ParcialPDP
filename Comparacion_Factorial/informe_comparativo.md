# Informe Comparativo: Factorial recursivo e iterativo

### Introducción:

El cálculo del factorial es una operación fundamental en matemáticas y programación. Se puede implementar utilizando métodos iterativos o recursivos, cada uno con sus propias ventajas y desventajas. En este informe, se comparan ambas estrategias en términos de eficiencia y consumo de memoria.

### Objetivos de la Comparación

- Evaluar el tiempo de ejecución de cada método para distintos valores de n.

- Analizar el consumo de memoria en la versión recursiva debido al uso de la pila.

- Determinar cuál de las dos versiones es más eficiente para valores grandes de n.

### Resultados de la Evaluación

A continuación, se presentan los tiempos de ejecución y el consumo de memoria estimado en cada caso:

| Valor de n | Tiempo (ms) iterativo | Tiempo (ms) recursivo | Memoria empleada recursividad (bytes) |
|------------|----------------------|----------------------|----------------------------------|
| 5          | 0.000014             | 0.000019             | 80                               |
| 15         | 0.000037             | 0.000064             | 240                              |
| 30         | 0.000067             | 0.000141             | 480                              |
| 31         | 0.000083             | 0.00120              | 496                              |


**Nota**: Valores de n antes del desbordamiento recursivo.

### Analisis y conclusiones

**Eficiencia en tiempo**: La versión iterativa muestra un mejor rendimiento a medida que n crece. La sobrecarga de llamadas en la pila en la recursión genera tiempos de ejecución más altos.

**Uso de memoria**: La implementación recursiva utiliza memoria adicional debido a la acumulación de llamadas en la pila, lo que puede llevar a un desbordamiento para valores muy grandes de n.

**Escalabilidad**: La versión iterativa es más escalable y adecuada para valores grandes de n, ya que su consumo de memoria se mantiene constante.

**Aplicabilidad**: Si el factorial se calcula para valores pequeños, la recursión puede ser aceptable. Sin embargo, en entornos de alto rendimiento, la implementación iterativa es la opción más segura y eficiente.

En conclusión, aunque la versión recursiva puede ser útil para producir un código más conciso, su consumo de memoria y tiempo de ejecución son desventajas significativas en escenarios donde se requieren eficiencia y optimización de recursos. La versión iterativa se destaca por su mejor rendimiento, menor uso de memoria y mayor estabilidad, siendo la opción recomendada para aplicaciones que demandan alta eficiencia computacional.

