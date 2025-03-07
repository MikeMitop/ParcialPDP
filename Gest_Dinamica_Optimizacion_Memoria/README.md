Punto 3: Sistema de Gestión de Estudiantes con Optimización de Memoria

## Introducción
En este ejercicio, desarrollamos un sistema de gestión de estudiantes en *C*, aplicando técnicas de optimización de memoria para garantizar un uso eficiente de los recursos. El programa permite agregar, actualizar y eliminar estudiantes, ajustando dinámicamente la memoria utilizada para evitar desperdicios y fragmentación. Además, implementamos mecanismos de seguimiento del uso de memoria para evaluar el impacto de nuestras operaciones.

## Objetivos
1. Implementar una estructura de datos eficiente para almacenar información de estudiantes.
2. Utilizar *memoria dinámica* con malloc y free para gestionar registros de estudiantes sin desperdiciar espacio.
3. Optimizar el almacenamiento de información con técnicas como *bitfields* y cadenas dinámicas.
4. Minimizar la fragmentación y compactar la memoria de manera eficiente.
5. Registrar y reportar el uso de memoria tras cada operación para evaluar su impacto.

## Desarrollo
### 1. Estructura de Datos
Cada estudiante se representa mediante una *estructura (struct) optimizada*, almacenando:
- *Nombre y apellido* como punteros (char*), reservando solo la memoria estrictamente necesaria.
- *Edad, usando un **bitfield* de 7 bits para reducir el uso de memoria.
- *ID* como cadena dinámica (char*).
- *Calificaciones, almacenadas en un **array dinámico* ajustado al número exacto de materias.
- *Puntero al siguiente estudiante*, formando una lista enlazada.

### 2. Funcionalidades Implementadas
#### a) Agregar Estudiante
Se asigna memoria dinámica para un nuevo estudiante, almacenando su información de manera eficiente y vinculándolo a la lista de estudiantes.

#### b) Actualizar Estudiante
Permite modificar los datos de un estudiante a partir de su *ID*, liberando y reasignando memoria cuando sea necesario.

#### c) Eliminar Estudiante
Busca y elimina un estudiante por *ID*, liberando toda la memoria utilizada por su registro.

#### d) Reporte de Uso de Memoria
Tras cada operación, el programa calcula la memoria total utilizada y muestra un reporte detallado del consumo de cada estudiante.

### 3. Gestión de Memoria
- *Uso de malloc y free* para manejar nombres, apellidos, IDs y calificaciones sin reservas innecesarias.
- *Bitfield para la edad*, optimizando el espacio en memoria.
- *Liberación adecuada de memoria*, asegurando que no queden fugas tras eliminar estudiantes.

## Resultados y Ejemplo de Ejecución
### Entrada
``` c
Agregar Estudiante: Nombre="Carlos", Apellido="Gomez", Edad=20, ID="12345678", Calificaciones=[85, 90, 78]
Eliminar Estudiante: ID="12345678"
```

### Salida Esperada

``` c
Estudiante "Carlos Gomez" agregado correctamente. Memoria utilizada: 128 bytes.
Estudiante con ID 12345678 eliminado correctamente. Memoria liberada: 128 bytes.
```

## Conclusión
Este ejercicio nos permitió aplicar conceptos fundamentales de *gestión de memoria en C, asegurando un uso eficiente de los recursos y evitando desperdicio. La implementación de **cadenas dinámicas, arrays ajustables y bitfields* optimizó el consumo de memoria. Además, con la lista enlazada, logramos una estructura flexible y escalable para futuras mejoras en la gestión de estudiantes.
