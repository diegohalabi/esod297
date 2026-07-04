# Pautas y herramientas de evaluación

En esta carpeta están las rúbricas y pautas con las que se evalúa la asignatura, además de la calculadora de notas.

### 📋 Rúbricas y pautas

- `pauta_1er_avance.pdf` — pauta del **primer avance de datos** (Anexo 3 del Programa).
- `pauta_evaluacion_prof_Informante.pdf` — rúbrica de evaluación de la Tesina que usa el profesor informante (Anexo 5 del Programa).
- `Instrucciones-prof-informante.md` — instrucciones detalladas para el profesor informante.

Las pautas del **profesor patrocinante** (Anexo 2) y del **segundo avance de datos** (Anexo 4) están por ahora solo dentro del Programa de Asignatura; queda pendiente publicarlas también como documentos sueltos aquí.

### 🧮 Calculadora de notas (`notas.py`)

Script en Python que toma las evaluaciones de los profesores informantes desde `eval.csv`, convierte cada categoría de la rúbrica a puntaje, calcula la nota de cada evaluador con **PREMA de 60%** (60% del puntaje = nota 4,0) y promedia las notas por estudiante.

El resultado es solo la **componente de los profesores informantes** (el 80% de la nota final). No incluye la ponderación con el patrocinante (10%) ni los avances (10%).

No requiere instalar nada: funciona solo con Python 3 (librería estándar).

```bash
python3 notas.py                    # lee ./eval.csv y escribe ./notas.csv
python3 notas.py eval.csv salida.csv   # rutas de entrada y salida a medida
```

El archivo `eval.csv` debe incluir las columnas `Estudiante.1` y `Estudiante.2` con los nombres de los estudiantes, y las 11 columnas de la rúbrica con las respuestas tal cual aparecen en `pauta_evaluacion_prof_Informante.pdf`.
