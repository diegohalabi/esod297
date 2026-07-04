#!/usr/bin/env python3
"""Calculadora de notas para ESOD297 (Tesina, Escuela de Odontología UACh).

Lee las evaluaciones de los profesores informantes desde ``eval.csv``,
transforma las categorías de la rúbrica a puntaje, calcula la nota de cada
evaluador (con PREMA de 60%) y promedia las notas por estudiante.

El resultado corresponde SOLO a la componente de los profesores informantes
(el 80% de la nota final, según el Programa). No incluye la evaluación del
patrocinante (10%) ni los avances (10%): esa ponderación se hace aparte.

No requiere dependencias externas: usa solo la librería estándar de Python 3.
Puede ser utilizado, distribuido y modificado libremente.

Uso:
    python3 notas.py               # lee ./eval.csv y escribe ./notas.csv
    python3 notas.py eval.csv out.csv   # rutas de entrada y salida a medida
"""

import csv
import sys
from collections import defaultdict
from pathlib import Path

# Columnas de eval.csv con los nombres de los estudiantes de cada Tesina.
STUDENT_COLS = ["Estudiante.1", "Estudiante.2"]

# Equivalencia entre las categorías de la rúbrica y su puntaje. La rúbrica del
# profesor informante (pautas/pauta_evaluacion_prof_Informante.pdf) tiene 11
# ítems; cualquier respuesta fuera de esta tabla vale 0.
#
# OJO: las llaves deben coincidir EXACTAMENTE con el texto que exporta SIVEDUC
# en eval.csv. Si SIVEDUC exporta, por ejemplo, "1. No cumple con requisitos
# mínimos" (como en el PDF), ajuste la llave correspondiente aquí abajo.
PUNTAJE = {
    "4. No requiere cambios en su forma actual": 3.0,
    "3. Requiere cambios menores": 2.5,
    "2. Requiere cambios mayores": 2.0,
    "1. No cumple con lo mínimo": 1.0,
}

# Escala de nota con PREMA (exigencia mínima de aprobación) de 60% fijo, según
# el Programa. El 60% del puntaje máximo equivale a la nota de aprobación (4.0).
PUNTAJE_MAX = 33.0          # 11 ítems x 3 puntos
PREMA = 0.60               # 60% del puntaje máximo -> nota de aprobación
NOTA_MIN = 1.0
NOTA_APROBACION = 4.0
NOTA_MAX = 7.0


def puntaje_a_nota(puntaje):
    """Convierte un puntaje (0..PUNTAJE_MAX) a nota (1.0..7.0) con PREMA 60%.

    Usa dos tramos lineales: de 1.0 a 4.0 bajo la exigencia (60%) y de 4.0 a
    7.0 sobre ella. Así, el 60% del puntaje máximo da exactamente un 4.0.
    """
    corte = PREMA * PUNTAJE_MAX
    if puntaje >= corte:
        return NOTA_APROBACION + (puntaje - corte) / (PUNTAJE_MAX - corte) * (NOTA_MAX - NOTA_APROBACION)
    return NOTA_MIN + puntaje / corte * (NOTA_APROBACION - NOTA_MIN)


def detectar_columnas_rubrica(filas):
    """Devuelve las columnas cuyas respuestas corresponden a la rúbrica."""
    if not filas:
        return []
    columnas = [c for c in filas[0] if c not in STUDENT_COLS]
    return [c for c in columnas
            if any(fila.get(c) in PUNTAJE for fila in filas)]


def calcular_notas(filas):
    """Promedia por estudiante la nota obtenida en cada evaluación."""
    rubrica = detectar_columnas_rubrica(filas)
    notas_por_estudiante = defaultdict(list)

    for fila in filas:
        # Puntaje total y nota de esta evaluación (de un evaluador).
        score = sum(PUNTAJE.get(fila.get(col), 0.0) for col in rubrica)
        grade = puntaje_a_nota(score)
        # La evaluación aplica a cada estudiante de la Tesina.
        for col in STUDENT_COLS:
            estudiante = (fila.get(col) or "").strip()
            if estudiante:
                notas_por_estudiante[estudiante].append(grade)

    return {
        estudiante: sum(notas) / len(notas)
        for estudiante, notas in notas_por_estudiante.items()
    }


def main(argv):
    base = Path(__file__).resolve().parent
    entrada = Path(argv[1]) if len(argv) > 1 else base / "eval.csv"
    salida = Path(argv[2]) if len(argv) > 2 else base / "notas.csv"

    with open(entrada, newline="", encoding="utf-8-sig") as f:
        filas = list(csv.DictReader(f))

    notas = calcular_notas(filas)

    with open(salida, "w", newline="", encoding="utf-8") as f:
        escritor = csv.writer(f)
        escritor.writerow(["student", "total"])
        for estudiante in sorted(notas):
            escritor.writerow([estudiante, round(notas[estudiante], 2)])

    print(f"Notas guardadas en {salida}")
    for estudiante in sorted(notas):
        print(f"  {estudiante}: {notas[estudiante]:.2f}")


if __name__ == "__main__":
    main(sys.argv)
