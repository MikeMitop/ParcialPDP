#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Estudiante {
    char *nombre;
    char *apellido;
    unsigned int edad : 7;  // Uso de bitfield para optimizar el almacenamiento de edad
    char *id;
    int *calificaciones;
    int num_calificaciones;
    struct Estudiante *siguiente;
} Estudiante;

Estudiante *listaEstudiantes = NULL;
size_t memoriaTotal = 0;

void mostrarUsoMemoria() {
    printf("Memoria total utilizada: %lu bytes\n", memoriaTotal);
}

void agregarEstudiante(const char *nombre, const char *apellido, unsigned int edad, const char *id, int *calificaciones, int num_calificaciones) {
    Estudiante *nuevo = (Estudiante *)malloc(sizeof(Estudiante));
    nuevo->nombre = strdup(nombre);
    nuevo->apellido = strdup(apellido);
    nuevo->edad = edad;
    nuevo->id = strdup(id);
    nuevo->num_calificaciones = num_calificaciones;
    nuevo->calificaciones = (int *)malloc(num_calificaciones * sizeof(int));
    memcpy(nuevo->calificaciones, calificaciones, num_calificaciones * sizeof(int));
    
    nuevo->siguiente = listaEstudiantes;
    listaEstudiantes = nuevo;
    
    size_t memoriaUsada = sizeof(Estudiante) + strlen(nombre) + 1 + strlen(apellido) + 1 + strlen(id) + 1 + (num_calificaciones * sizeof(int));
    memoriaTotal += memoriaUsada;
    
    printf("Agregar Estudiante: Nombre=\"%s\", Apellido=\"%s\", Edad=%u, ID=\"%s\", Calificaciones=[",
           nombre, apellido, edad, id);
    for (int i = 0; i < num_calificaciones; i++) {
        printf("%d%s", calificaciones[i], (i < num_calificaciones - 1) ? ", " : "");
    }
    printf("]\n");
    printf("Estudiante \"%s %s\" agregado correctamente. Memoria utilizada: %lu bytes.\n", nombre, apellido, memoriaUsada);
    mostrarUsoMemoria();
}

void eliminarEstudiante(const char *id) {
    Estudiante *actual = listaEstudiantes, *anterior = NULL;
    while (actual) {
        if (strcmp(actual->id, id) == 0) {
            if (anterior) {
                anterior->siguiente = actual->siguiente;
            } else {
                listaEstudiantes = actual->siguiente;
            }
            
            size_t memoriaLiberada = sizeof(Estudiante) + strlen(actual->nombre) + 1 + strlen(actual->apellido) + 1 + strlen(actual->id) + 1 + (actual->num_calificaciones * sizeof(int));
            memoriaTotal -= memoriaLiberada;
            
            printf("Eliminar Estudiante: ID=\"%s\"\n", id);
            printf("Estudiante con ID %s eliminado correctamente. Memoria liberada: %lu bytes.\n", id, memoriaLiberada);
            
            free(actual->nombre);
            free(actual->apellido);
            free(actual->id);
            free(actual->calificaciones);
            free(actual);
            mostrarUsoMemoria();
            return;
        }
        anterior = actual;
        actual = actual->siguiente;
    }
    printf("Estudiante con ID %s no encontrado.\n", id);
}

void liberarMemoria() {
    Estudiante *actual = listaEstudiantes;
    while (actual) {
        Estudiante *temp = actual;
        actual = actual->siguiente;
        free(temp->nombre);
        free(temp->apellido);
        free(temp->id);
        free(temp->calificaciones);
        free(temp);
    }
    listaEstudiantes = NULL;
    memoriaTotal = 0;
    printf("Toda la memoria ha sido liberada.\n");
}

int main() {
    int calificaciones1[] = {85, 90, 78};
    agregarEstudiante("Carlos", "Gomez", 20, "12345678", calificaciones1, 3);
    
    int calificaciones2[] = {88, 76};
    agregarEstudiante("Ana", "Lopez", 22, "87654321", calificaciones2, 2);
    
    eliminarEstudiante("12345678");
    
    liberarMemoria();
    return 0;
}
