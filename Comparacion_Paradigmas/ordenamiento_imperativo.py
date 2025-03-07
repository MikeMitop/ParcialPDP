def bubble_sort(lista):
    n = len(lista)
    for i in range(n):
        for j in range(n - i - 1):
            if lista[j][1] < lista[j + 1][1] or (lista[j][1] == lista[j + 1][1] and lista[j][0] > lista[j + 1][0]):
                lista[j], lista[j + 1] = lista[j + 1], lista[j]
    return lista

estudiantes = [
    ('Ana',85),
    ('Luis',90),
    ('Carlos',85),
    ('Sofia',92),
    ('Maria',90)
]
print(bubble_sort(estudiantes))
