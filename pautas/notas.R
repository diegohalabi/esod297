#############################################################
## Este es el script para calcular notas en ESOD297, puede ##
##    ser utilizado, distribuido y modificado libremente   ##
#############################################################
# Cargar paquetes requeridos
library(tidyverse)
library(here)
# Importa todas las evaluaciones (https://github.com/diegohalabi/ESOD299/blob/master/rubricas/rubrica_evaluacion_prof_Informante.pdf)
here()
df <- read.csv('eval.csv',sep = ',')
str(df)
# Ordena los datos
grades <- df %>% 
  gather(nstudent,student,Estudiante.1:Estudiante.2) %>% 
  # Reemplaza las categorías por números (puntaje)
  mutate_at(vars(5:15),
            funs(recode(.,
                        '4. No requiere cambios en su forma actual' = 3,
                        '3. Requiere cambios menores' = 2.5,
                        '2. Requiere cambios mayores' = 2,
                        '1. No cumple con lo mínimo' = 1,
                        .default = 0
            ))) %>% 
  # Agrega una nueva columna (score) con el puntaje total de cada evaluador
  mutate(score = rowSums(.[5:15])) %>% 
  # Agrega una nueva columna (grades) con la nota de cada evaluador
  mutate(grade = (score * 7)/33) %>% 
  # Muestra la nota para cada estudiante
  group_by(student) %>% 
  summarize(total=mean(grade))
# Guarda el archivo
write.csv(grades,'notas.csv')
