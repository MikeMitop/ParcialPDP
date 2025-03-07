#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

long factorial(int n) {
    if (n == 0 || n == 1) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

// Función para estimar memoria consumida por recursividad
size_t memoria(int n) {
    size_t tamano_stack_frame = 16; // Aprox. 16 bytes por llamada
    return tamano_stack_frame * n;
}

// Función para medir tiempo
double medir_tiempo(long (*func)(int), int n, int repeticiones) {
    struct timeval inicio, fin;
    gettimeofday(&inicio, NULL);

    for (int i = 0; i < repeticiones; i++) {
        func(n);
    }

    gettimeofday(&fin, NULL);

    double tiempo_total = (fin.tv_sec - inicio.tv_sec) + 
                          (fin.tv_usec - inicio.tv_usec) / 1000000.0;
    return tiempo_total / repeticiones;
}

int main() {
    int n;
    long r;
    printf("Ingrese el numero: ");
    
    // Validación de entrada
    if (scanf("%d", &n) != 1 || n < 0) {
        printf("Error: Numero no valido. Debe ser un entero positivo.\n");
        return 1;
    }
    
    // Determinar repeticiones fijas
    int repeticiones = 10000000;

    // Medir tiempo de ejecución
    double tiempo_transcurrido = medir_tiempo(factorial, n, repeticiones);
    
    // Calcular factorial
    r = factorial(n);
    
    // Estimar memoria usada
    size_t memoria_usada = memoria(n);
    
    printf("Factorial de %d es: %ld\n", n, r);
    printf("Tiempo de ejecucion promedio: %.9f segundos\n", tiempo_transcurrido);
    printf("Memoria estimada: %zu bytes\n", memoria_usada);
    
    return 0;
}
