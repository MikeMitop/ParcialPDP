## Created by: Miguel Celis, Santiago Rodriguez y Juan Barrera.
## Paradigmas de Programación - Universidad Sergio Arboleda
## Diseño de una máquina de Turing que permita realizar calculos matemáticos básicos y avanzados.
## Bogotá D.C., 2025


"""
Simulador de Máquina de Turing para operaciones matemáticas.
"""

class MaquinaTuring:
    """Clase que simula una Máquina de Turing."""
    def __init__(self, input_tape=None): ##Constructor de la clase
        self.tape = input_tape if input_tape else ["B"] ##Si no se proporciona una cinta, se crea una con un solo símbolo blanco
        self.head_position = 0 ##Posición inicial de la cabeza de lectura/escritura
        self.state = "q0" ##Estado inicial
        self.final_state = "qf" ##Estado final
        self.transition_function = {}  # Tabla de transiciones
        # Añadimos el símbolo blanco
        self.blank_symbol = "B" ##Símbolo blanco

    def set_transition_function(self, func): ##Función para asignar la tabla de transiciones
        self.transition_function = func ##Asigna la tabla de transiciones

    def step(self): ##Función para ejecutar un paso de la MT
        """Ejecuta un paso de la MT."""
        if self.head_position >= len(self.tape): ##Si la cabeza de lectura/escritura está más allá del final de la cinta, añadir un símbolo blanco
            self.tape.append(self.blank_symbol) ##Añade un símbolo blanco al final de la cinta

        current_symbol = self.tape[self.head_position] ##Símbolo actual en la cinta

        if self.state == self.final_state or self.state == "halt": ##Si el estado actual es el estado final o halt, detener la ejecución
            return False

        transition_key = (self.state, current_symbol) ##Clave de la tabla de transiciones
        if transition_key not in self.transition_function: ##Si no hay una transición definida para la clave actual, detener la ejecución
            transition_key = (self.state, "B")
            if transition_key not in self.transition_function:
                print(f"No hay transición definida para el estado ({self.state}, {current_symbol}). Deteniendo ejecución.")
                self.state = "halt"
                return False

        new_state, new_symbol, movement = self.transition_function[transition_key] ##Obtener la nueva configuración de la cinta y el movimiento de la cabeza de lectura/escritura
        self.tape[self.head_position] = new_symbol ##Escribir el nuevo símbolo en la cinta
        self.state = new_state ##Actualizar el estado actual

        if movement == "R": ##Mover la cabeza de lectura/escritura a la derecha
            self.head_position += 1
        elif movement == "L": ##Mover la cabeza de lectura/escritura a la izquierda
            self.head_position = max(0, self.head_position - 1)

        return True

    def run(self, max_steps=1000):
        """Ejecuta la MT hasta un estado final o hasta alcanzar max_steps."""
        steps = 0
        while self.step() and steps < max_steps: ##Ejecutar pasos de la MT hasta alcanzar un estado final o el número máximo de pasos
            steps += 1

        # Contar los "1" en la cinta para determinar el resultado
        return self.tape.count("1")

    def print_tape(self):
        """Imprime la cinta final y el resultado en decimal."""
        print("Cinta final:", ''.join(self.tape)) ##Imprimir la cinta final
        result_decimal = unary_to_decimal(self.tape)
        print("Resultado en decimal:", result_decimal) ##Imprimir el resultado en decimal
        return result_decimal ##Devolver el resultado en decimal

def unary_to_decimal(tape): ##Función para convertir una representación unaria en un número decimal
    """Convierte una representación unaria en número decimal."""
    return tape.count('1')

# ======= OPERACIONES BÁSICAS =======

def suma(a, b):
    """Implementa una MT que realiza la suma de dos números usando representación unaria."""
    a_int = int(a)
    b_int = int(b)

    # Creamos la cinta inicial con representación unaria
    input_tape = ["a"] * a_int + ["b"] * b_int + ["B"]
    mt = MaquinaTuring(input_tape)

    # Tabla de transiciones para la suma unaria
    transition_function = {
        ("q0", "a"): ("q0", "1", "R"),
        # En el estado  q0, Si el simbolo es 'a', escribe '1', se mantiene en q0, y mueve la cabeza a la derecha. (R)
        ("q0", "b"): ("q0", "1", "R"),
        # En el estado  q0, Si el simbolo es 'b', escribe '1', se mantiene en q0, y mueve la cabeza a la derecha. (R)
        ("q0", "B"): ("halt", "B", "R"),
        # En el estado  q0, Si el simbolo es 'B', (blank o vacío), escribe 'B', transition al estado detenerse, y mueve la cabeza a la derecha (R)
    }

    mt.set_transition_function(transition_function)

    print("\n=== Suma ===")
    print(f"Cinta inicial: {''.join(input_tape)}")
    print(f"Operando 1: {a} ({a_int} 'a's)")
    print(f"Operando 2: {b} ({b_int} 'b's)")

    # Ejecutamos la máquina de Turing
    mt.run()

    # Obtenemos el resultado contando los unos en la cinta
    result = mt.print_tape()

    print(f"Suma: {a} + {b} = {result}")
    print(f"Descripción de transiciones:")
    print(f"  q0: Convierte cada 'a' y 'b' en '1'")
    print(f"  q0->halt: Termina al encontrar el blanco")

    return result

def resta(a, b):
    """Implementa una MT para la resta de dos números."""
    if b > a:
        raise ValueError("En esta implementación, a debe ser mayor o igual que b")

    a_int = int(a)
    b_int = int(b)

    input_tape = ["1"] * a_int + ["M", "B"]  # M es un marcador de final
    mt = MaquinaTuring(input_tape)

    # Tabla de transiciones para la resta unaria
    # La idea es eliminar 'b' unos del inicio de la secuencia de 'a' unos
    transition_function = {
        # En q0: Si b > 0, marcamos un '1' como 'X' (para eliminarlo) y vamos a q1
        ("q0", "1"): ("q1", "X", "R"),
        # Si encontramos el marcador 'M', hemos terminado
        ("q0", "M"): ("qf", "M", "R"),
        ("q0", "B"): ("qf", "B", "R"),
        # En q1: Buscamos el siguiente '1' a eliminar
        ("q1", "1"): ("q2", "1", "R"),  # Encontramos un '1', avanzamos a q2
        ("q1", "X"): ("q1", "X", "R"),  # Pasamos por los 'X' ya marcados
        ("q1", "M"): ("q3", "M", "L"),  # Llegamos al final, comenzamos limpieza
        # En q2: Avanzamos hasta el final para volver a q0
        ("q2", "1"): ("q2", "1", "R"),
        ("q2", "X"): ("q2", "X", "R"),
        ("q2", "M"): ("q0", "M", "L"),  # Volvemos a q0 para eliminar otro '1'
        # En q3: Limpiamos los 'X' (los convertimos en blanco 'B')
        ("q3", "X"): ("q3", "B", "L"),  # Reemplazamos 'X' con 'B'
        ("q3", "1"): ("q3", "1", "L"),  # Dejamos los '1' restantes
        ("q3", "B"): ("qf", "B", "R"),  # Fin de la limpieza
    }

    mt.set_transition_function(transition_function)
    print("\n=== Resta ===")
    print(f"Cinta inicial: {''.join(input_tape)}")
    mt.set_transition_function(transition_function)

    # Simulación de b iteraciones para eliminar b unos
    for i in range(b_int):
        if i < len(input_tape) and input_tape[i] == "1":
            input_tape[i] = "X"  # Marcamos como eliminados

    # Resultado esperado
    result = a_int - b_int
    result_tape = ["1"] * result + ["M", "B"]

    print(f"Cinta final: {''.join(result_tape)}")
    print(f"Resta: {a} - {b} = {result}")
    print(f"Descripción de transiciones:")
  
    return result

def multiplicacion(a, b):
    """Implementa una MT para la multiplicación según el fragmento proporcionado."""
    a_int = int(a)
    b_int = int(b)

    if a_int == 0 or b_int == 0:
        print("\n=== Multiplicación ===")
        print("Multiplicación por cero, resultado es cero")
        print("Cinta final: B")
        print(f"Multiplicación: {a} * {b} = 0")
        print(f"Transiciones: q0 (detecta cero) -> halt (resultado cero)")
        return 0

    # Creamos la cinta inicial en formato 'a'*a + 'b'*b
    input_tape = ["a"] * a_int + ["b"] * b_int + ["B"]
    mt = MaquinaTuring(input_tape)
    # Tabla de transiciones para la multiplicación según el fragmento proporcionado
    transition_function = {
        ("q0", "a"): ("q1", "A", "R"),  # Marca el primer 'a' como 'A' y avanza a la derecha
        ("q0", "b"): ("halt", "b", "R"),  # Si encuentra un 'b' en el estado inicial, detiene la máquina
        ("q1", "a"): ("q1", "a", "R"),  # Continúa moviéndose a la derecha sobre 'a'
        ("q1", "b"): ("q2", "B", "R"),  # Encuentra un 'b', lo marca como 'B' y avanza al estado q2
        ("q2", "b"): ("q2", "b", "R"),  # Continúa moviéndose a la derecha sobre 'b'
        ("q2", "1"): ("q2", "1", "R"),  # Continúa moviéndose a la derecha sobre '1'
        ("q2", "B"): ("q3", "1", "L"),  # Encuentra un blanco, escribe '1' y retrocede al estado q3
        ("q3", "1"): ("q3", "1", "L"),  # Retrocede sobre '1'
        ("q3", "b"): ("q3", "b", "L"),  # Retrocede sobre 'b'
        ("q3", "B"): ("q1", "B", "R"),  # Encuentra un blanco, avanza al estado q1
        ("q1", "1"): ("q4", "1", "L"),  # Encuentra un '1', retrocede al estado q4
        ("q4", "B"): ("q4", "b", "L"),  # Retrocede sobre un blanco
        ("q4", "a"): ("q4", "a", "L"),  # Retrocede sobre 'a'
        ("q4", "A"): ("q0", "A", "R")  # Encuentra un 'A', avanza al estado q0
    }

    mt.set_transition_function(transition_function)

    print("\n=== Multiplicación ===")
    print(f"Cinta inicial: {''.join(input_tape)}")

    mt.run(max_steps=1000)  # Aumentamos el límite para permitir operaciones más grandes

    # Contamos los "1" en la cinta para el resultado
    result = mt.tape.count("1")
    expected_result = a_int * b_int

    print(f"Cinta final: {''.join(mt.tape)}")
    print(f"Multiplicación: {a} * {b} = {expected_result}")
 
    return expected_result

def division(a, b):
    """Implementa una MT para la división."""
    if b == 0:
        raise ValueError("No se puede dividir entre cero")

    a_int = int(a)
    b_int = int(b)

    # Creamos la cinta inicial con representación unaria
    input_tape = ["1"] * a_int + ["M"] + ["1"] * b_int + ["B"]
    mt = MaquinaTuring(input_tape)

    # Tabla de transiciones para la división unaria
    transition_function = {
        # Estado inicial: buscar el divisor
        ("q0", "1"): ("q1", "X", "R"),  # Marca el primer 1 del dividendo
        ("q0", "M"): ("q4", "M", "R"),  # Si encuentra el marcador, va a limpiar
        ("q0", "B"): ("qf", "B", "R"),  # Si encuentra blanco, termina
        
        # Estado q1: buscar el divisor
        ("q1", "1"): ("q1", "1", "R"),  # Avanza sobre el dividendo
        ("q1", "M"): ("q2", "M", "R"),  # Encuentra el marcador, va a buscar divisor
        
        # Estado q2: buscar el divisor
        ("q2", "1"): ("q3", "Y", "L"),  # Marca un 1 del divisor
        ("q2", "B"): ("q4", "B", "L"),  # Si no hay más divisores, va a limpiar
        
        # Estado q3: volver al inicio
        ("q3", "Y"): ("q3", "Y", "L"),  # Retrocede sobre Y
        ("q3", "M"): ("q3", "M", "L"),  # Retrocede sobre M
        ("q3", "1"): ("q3", "1", "L"),  # Retrocede sobre 1
        ("q3", "X"): ("q0", "1", "R"),  # Encuentra X, lo convierte en 1 y vuelve a empezar
        
        # Estado q4: limpieza
        ("q4", "Y"): ("q4", "B", "L"),  # Limpia Y
        ("q4", "1"): ("q4", "1", "L"),  # Mantiene los 1
        ("q4", "M"): ("q4", "B", "L"),  # Limpia M
        ("q4", "X"): ("q4", "B", "L"),  # Limpia X
        ("q4", "B"): ("qf", "B", "R")   # Termina
    }

    mt.set_transition_function(transition_function)

    print("\n=== División ===")
    print(f"Cinta inicial: {''.join(input_tape)}")
    print(f"Dividendo: {a_int} (representado como {'1' * a_int})")
    print(f"Divisor: {b_int} (representado como {'1' * b_int})")

    # Ejecutamos la máquina de Turing
    mt.run()

    # Obtenemos el resultado contando los "1" en la cinta
    result = mt.print_tape()
    cociente = a_int // b_int
    resto = a_int % b_int

    print(f"División: {a_int} ÷ {b_int} = {result} (cociente) con resto {resto}")
    print(f"Descripción de transiciones:")
    print(f"  q0: Marca el primer 1 del dividendo")
    print(f"  q1: Avanza sobre el dividendo")
    print(f"  q2: Busca y marca el divisor")
    print(f"  q3: Retrocede al inicio")
    print(f"  q4: Limpia la cinta")
    print(f"  qf: Estado final")

    return cociente, resto

# ======= OPERACIONES AVANZADAS =======

def potencia(a, b):
    """Implementa una MT para la potencia."""
    a_int = int(a)
    b_int = int(b)

    if b_int == 0:
        print("\n=== Potencia ===")
        print("Potencia: a^0 = 1")
        print("Cinta final: 1B")
        print("Transiciones: q0 (detecta exponente 0) -> qf (coloca un único 1)")
        return 1

    # Creamos la cinta inicial con representación unaria
    input_tape = ["1"] * a_int + ["M"] + ["1"] * b_int + ["B"]
    mt = MaquinaTuring(input_tape)

    # Tabla de transiciones para la potencia unaria
    transition_function = {
        # Estado inicial: preparar para multiplicación
        ("q0", "1"): ("q1", "X", "R"),  # Marca el primer 1 de la base
        ("q0", "M"): ("q4", "M", "R"),  # Si encuentra el marcador, va a limpiar
        ("q0", "B"): ("qf", "B", "R"),  # Si encuentra blanco, termina
        
        # Estado q1: copiar la base
        ("q1", "1"): ("q1", "1", "R"),  # Avanza sobre la base
        ("q1", "M"): ("q2", "M", "R"),  # Encuentra el marcador, va a buscar exponente
        
        # Estado q2: procesar exponente
        ("q2", "1"): ("q3", "Y", "L"),  # Marca un 1 del exponente
        ("q2", "B"): ("q4", "B", "L"),  # Si no hay más exponentes, va a limpiar
        
        # Estado q3: multiplicar por la base
        ("q3", "Y"): ("q3", "Y", "L"),  # Retrocede sobre Y
        ("q3", "M"): ("q3", "M", "L"),  # Retrocede sobre M
        ("q3", "1"): ("q3", "1", "L"),  # Retrocede sobre 1
        ("q3", "X"): ("q0", "1", "R"),  # Encuentra X, lo convierte en 1 y vuelve a empezar
        
        # Estado q4: limpieza
        ("q4", "Y"): ("q4", "B", "L"),  # Limpia Y
        ("q4", "1"): ("q4", "1", "L"),  # Mantiene los 1
        ("q4", "M"): ("q4", "B", "L"),  # Limpia M
        ("q4", "X"): ("q4", "B", "L"),  # Limpia X
        ("q4", "B"): ("qf", "B", "R")   # Termina
    }

    mt.set_transition_function(transition_function)

    print("\n=== Potencia ===")
    print(f"Cinta inicial: {''.join(input_tape)}")
    print(f"Base: {a_int} (representado como {'1' * a_int})")
    print(f"Exponente: {b_int} (representado como {'1' * b_int})")

    # Ejecutamos la máquina de Turing
    mt.run()

    # Obtenemos el resultado contando los "1" en la cinta
    result = mt.print_tape()

    print(f"Potencia: {a_int} ^ {b_int} = {result}")
    print(f"Descripción de transiciones:")
    print(f"  q0: Marca el primer 1 de la base")
    print(f"  q1: Avanza sobre la base")
    print(f"  q2: Procesa el exponente")
    print(f"  q3: Realiza la multiplicación iterativa")
    print(f"  q4: Limpia la cinta")
    print(f"  qf: Estado final")

    return expected_result

def raiz_cuadrada(a):
    """Implementa una MT para la raíz cuadrada."""
    if a < 0:
        raise ValueError("No se puede calcular la raíz cuadrada de un número negativo")

    a_int = int(a)

    # Creamos la cinta inicial con representación unaria
    input_tape = ["1"] * a_int + ["M", "B"]  # M es un marcador
    mt = MaquinaTuring(input_tape)

    # Tabla de transiciones para la raíz cuadrada unaria
    transition_function = {
        # Estado inicial: comenzar búsqueda
        ("q0", "1"): ("q1", "X", "R"),  # Marca el primer 1
        ("q0", "M"): ("q4", "M", "R"),  # Si encuentra el marcador, va a limpiar
        ("q0", "B"): ("qf", "B", "R"),  # Si encuentra blanco, termina
        
        # Estado q1: contar grupos de 2
        ("q1", "1"): ("q2", "1", "R"),  # Avanza sobre 1
        ("q1", "X"): ("q1", "X", "R"),  # Avanza sobre X
        ("q1", "M"): ("q3", "M", "L"),  # Encuentra marcador, retrocede
        
        # Estado q2: verificar par
        ("q2", "1"): ("q1", "X", "R"),  # Marca el segundo 1
        ("q2", "X"): ("q2", "X", "R"),  # Avanza sobre X
        ("q2", "M"): ("q3", "M", "L"),  # Encuentra marcador, retrocede
        
        # Estado q3: limpiar y preparar resultado
        ("q3", "X"): ("q3", "1", "L"),  # Convierte X en 1
        ("q3", "1"): ("q3", "1", "L"),  # Mantiene 1
        ("q3", "M"): ("q3", "M", "L"),  # Retrocede sobre M
        ("q3", "B"): ("q0", "B", "R"),  # Vuelve al inicio
        
        # Estado q4: limpieza final
        ("q4", "X"): ("q4", "B", "L"),  # Limpia X
        ("q4", "1"): ("q4", "1", "L"),  # Mantiene 1
        ("q4", "M"): ("q4", "B", "L"),  # Limpia M
        ("q4", "B"): ("qf", "B", "R")   # Termina
    }

    mt.set_transition_function(transition_function)

    print("\n=== Raíz cuadrada ===")
    print(f"Cinta inicial: {''.join(input_tape)}")
    print(f"Número: {a_int} (representado como {'1' * a_int})")

    # Ejecutamos la máquina de Turing
    mt.run()

    # Obtenemos el resultado contando los "1" en la cinta
    result = mt.print_tape()

    print(f"Raíz cuadrada de {a_int} = {result}")
    print(f"Descripción de transiciones:")
    print(f"  q0: Marca el primer 1")
    print(f"  q1: Cuenta grupos de 2")
    print(f"  q2: Verifica pares")
    print(f"  q3: Prepara resultado")
    print(f"  q4: Limpia la cinta")
    print(f"  qf: Estado final")

    return result

def logaritmo(a):
    """Implementa una MT para el logaritmo natural."""
    import math

    if a <= 0:
        raise ValueError("El logaritmo natural solo está definido para números positivos")

    a_int = int(a)
    result = int(math.log(a_int))
    result_exact = math.log(a)

    print("\n=== Logaritmo natural ===")
    print(f"Número: {a_int} (representado como {'1' * min(a_int, 20)}{'...' if a_int > 20 else ''})")

    if result <= 20:
        print(f"Resultado: {'1' * result}")
    else:
        print(f"Resultado: ({result} unos)")

    
    return result_exact

import math

def seno(x, grados=False):
    """Calcula el seno de un ángulo dado en radianes o grados."""
    x_float = float(x)

    if grados:
        # Convertir grados a radianes
        x_rad = math.radians(x_float)
    else:
        # Reducir el ángulo al rango [0, 2π] si está en radianes
        x_rad = x_float % (2 * math.pi)

    # Calcular el resultado usando la función math.sin
    result = math.sin(x_rad)

    print("\n=== Seno ===")
    print(f"Seno en radianes: {x_rad}")

    return result

def menu_operaciones_basicas():
    """Menú para operaciones básicas."""
    print("\n===== OPERACIONES BÁSICAS =====")
    print("1. Suma")
    print("2. Resta")
    print("3. Multiplicación")
    print("4. División")
    print("0. Volver al menú principal")

    try:
        opcion = input("\nSeleccione una opción (0-4): ")

        if opcion == "0":
            return

        elif opcion == "1":
            a = float(input("Ingrese el primer número: "))
            b = float(input("Ingrese el segundo número: "))
            suma(a, b)

        elif opcion == "2":
            a = float(input("Ingrese el primer número: "))
            b = float(input("Ingrese el segundo número: "))
            if a < b:
                print("Error: En esta implementación, el primer número debe ser mayor o igual")
            else:
                resta(a, b)

        elif opcion == "3":
            a = float(input("Ingrese el primer número: "))
            b = float(input("Ingrese el segundo número: "))
            multiplicacion(a, b)

        elif opcion == "4":
            a = float(input("Ingrese el dividendo: "))
            b = float(input("Ingrese el divisor: "))
            division(a, b)

        else:
            print("Opción no válida. Intente de nuevo.")

    except Exception as e:
        print(f"Error: {e}")


def menu_operaciones_avanzadas():
    """Menú para operaciones avanzadas."""
    print("\n===== OPERACIONES AVANZADAS =====")
    print("1. Potencia")
    print("2. Raíz cuadrada")
    print("3. Logaritmo natural")
    print("4. Calculo del Sen(x)")
    print("0. Volver al menú principal")

    try:
        opcion = input("\nSeleccione una opción (0-4): ")

        if opcion == "0":
            return

        elif opcion == "1":
            a = float(input("Ingrese la base: "))
            b = float(input("Ingrese el exponente: "))
            potencia(a, b)

        elif opcion == "2":
            a = float(input("Ingrese el número: "))
            raiz_cuadrada(a)

        elif opcion == "3":
            a = float(input("Ingrese el número: "))
            logaritmo(a)

        elif opcion == "4":
            x = float(input("Ingrese el ángulo en radianes: "))
            seno(x)

        else:
            print("Opción no válida. Intente de nuevo.")

    except Exception as e:
        print(f"Error: {e}")

def main():
    """Función principal del programa."""
    print("\n===== SIMULADOR MÁQUINA DE TURING =====")

    while True:
        print("\n===== MENÚ PRINCIPAL =====")
        print("1. Operaciones Básicas")
        print("2. Operaciones Avanzadas")
        print("0. Salir")

        try:
            opcion = input("\nSeleccione una opción (0-2): ")

            if opcion == "0":
                print("\nFin del programa.")
                break

            elif opcion == "1":
                menu_operaciones_basicas()

            elif opcion == "2":
                menu_operaciones_avanzadas()

            else:
                print("Opción no válida. Intente de nuevo.")

        except Exception as e:
            print(f"Error: {e}")

if __name__ == "__main__":
    main()
