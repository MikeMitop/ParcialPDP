#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

// Función para calcular memoria consumida (simulación)
size_t get_memory_usage() {
    return sizeof(long) * 1; // Memoria estática
}

long factorial(int n) {
    long f = 1;
    for (int i = 1; i <= n; i++) {
        f *= i;
    }
    return f;
}

int main() {
    int n;
    long r;
    printf("Ingrese el numero: ");
    
    // Validación de entrada
    if (scanf("%d", &n) != 1 || n < 0) {
        printf("Error: Entrada no válida.\n");
        return 1;
    }
    
    // Múltiples ejecuciones para obtener un tiempo más preciso
    int repeticiones = (n < 10) ? 1000000 : 10000;
    
    struct timeval inicio, fin;
    
    gettimeofday(&inicio, NULL);
    
    for (int j = 0; j < repeticiones; j++) {
        r = factorial(n);
    }
    
    gettimeofday(&fin, NULL);
    
    // Calcular tiempo promedio
    double tiempo_transcurrido = (fin.tv_sec - inicio.tv_sec) + 
                                  (fin.tv_usec - inicio.tv_usec) / 1000000.0;
    tiempo_transcurrido /= repeticiones;
    
    // Obtener uso de memoria
    size_t memoria_usada = get_memory_usage();
    
    printf("Factorial de %d es: %ld \n", n, r);
    printf("Tiempo de ejecucion promedio: %.9f segundos\n", tiempo_transcurrido);
    printf("Memoria estimada: %zu bytes\n", memoria_usada);
    
    return 0;
}
