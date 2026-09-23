setwd("C:/Users/Kote/Desktop/Programacion/tarea5")
# Autor: José Elías Muñoz Gutiérrez
# Fecha: 22-09-2026
# Qué hace: usa los cinco verbos de dplyr + group_by() sobre la CASEN
#           reducida para responder: ¿qué sector tiene el mayor
#           ingreso promedio, y cómo cambia eso según la región?


library(dplyr)


# 1. Pregunta: ¿Qué sector económico tiene el mayor ingreso promedio, y en qué
# región se da la combinación sector-región con el ingreso más alto?


# 2. Carga

casen    <- read.csv("data/raw/casen_reducido.csv")   
ingresos <- read.csv("data/raw/casen_ingresos.csv")   

dim(casen)
str(casen)
sum(is.na(casen$ingreso))   # cuántos NA hay en ingreso


# 2b. Elegir columnas por patrón (sobre casen_ingresos.csv)

names(select(ingresos, starts_with("ing")))   # por el NOMBRE  -> columnas de ingreso
names(select(ingresos, where(is.numeric)))    # por el TIPO    -> también trae educ, edad, horas

# El primero busca columnas cuyo NOMBRE empieza con "ing" (las 4 fuentes
# de ingreso). El segundo busca columnas por su TIPO (numeric), así que
# además de las de ingreso trae educ, edad y horas, que no son ingresos ni 
# comienzan con esas iniciales



# 3. Los cinco verbos (sobre casen)


# filter(): me quedo con adultos en edad de trabajar y con ingreso
# registrado (condición combinada con &)
casen_filtrado <- filter(casen, edad >= 18 & !is.na(ingreso))

# select: reduzco a las columnas que voy a usar
casen_sel <- select(casen_filtrado, region, sector, educ, edad, ingreso)

# mutate: variable derivada obligatoria
casen_mut <- mutate(casen_sel, experiencia = pmax(edad - educ - 6, 0))

# arrange: ordeno por ingreso, de mayor a menor
casen_ord <- arrange(casen_mut, desc(ingreso))
head(casen_ord, 5)

# summarise: estadístico que responde la pregunta (ingreso promedio global)
summarise(casen_ord, ingreso_prom = mean(ingreso, na.rm = TRUE))


# 3b. case_when: variable categórica de 3+ niveles

casen_mut <- mutate(
  casen_mut,
  nivel_educ = case_when(
    educ <  12 ~ "Sin media completa",
    educ == 12 ~ "Media completa",
    TRUE       ~ "Superior"
  )
)

table(casen_mut$nivel_educ, useNA = "ifany")   # verifico que no quedó ningún NA


# 4. Agregación con group_by()


# (a1) Un grupo: ingreso promedio por sector
por_sector <- casen_mut |>
  group_by(sector) |>
  summarise(
    ingreso_prom = mean(ingreso, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(ingreso_prom))

por_sector

# (a2) Dos grupos cruzados: ingreso promedio por sector y región
por_sector_region <- casen_mut |>
  group_by(region, sector) |>
  summarise(
    ingreso_prom = mean(ingreso, na.rm = TRUE),
    n = n()
  ) |>
  arrange(desc(ingreso_prom))

por_sector_region

# group_by() + mutate(): conserva las filas, sitúa a cada persona
# respecto del promedio del sector 
casen_brecha <- casen_mut |>
  group_by(sector) |>
  mutate(brecha_sector = ingreso - mean(ingreso, na.rm = TRUE)) |>
  ungroup()

head(select(casen_brecha, region, sector, ingreso, brecha_sector), 5)

# La diferencia con summarise(): por_sector entrega una fila por sector
# (una tabla de grupos, se pierde a las personas). casen_brecha entrega
# las 60 filas originales, cada una con cuánto se aleja su propio
# ingreso del promedio de su sector (una tabla de personas).


# 5. Tratamiento de los NA

mean(casen$ingreso)                  # NA -> los 5 faltantes ensucia el promedio
mean(casen$ingreso, na.rm = TRUE)    # promedio ignorando los faltantes

# Al filtrar con !is.na(ingreso) al principio del script, se pierden
# 5 de las 60 personas (los NA puestos a propósito en la base). Es
# aceptable perderlas porque no hay forma de inventar su ingreso, y
# 5 de 60 no distorsiona demasiado los promedios por grupo,
# aunque sí reduce el tamaño de algunos grupos pequeños.

#6. Interpretación

# Según por_sector, el sector con mayor ingreso promedio en la base es
# Educación, con cerca de $908.857 en promedio (n = 7), seguido de
# Servicios con cerca de $788.600 (n = 15). El sector con menor ingreso
# promedio es Agricultura, con cerca de $447.846 (n = 13): una brecha de
# más de $460.000 entre el sector más alto y el más bajo. Al cruzar
# sector con región (por_sector_region), la combinación con el ingreso
# promedio más alto es Educación en la región de Ñuble, con cerca de
# $1.040.000 (aunque con solo n = 2 personas), lo que sugiere que el
# ingreso de un sector no es uniforme entre regiones. Estos resultados
# se asocian con diferencias de ingreso entre sectores y regiones, pero
# no permiten decir que trabajar en un sector determinado cause un mayor
# ingreso, porque no se controla por otras variables como educación o
# experiencia. Una limitación importante es el tamaño muestral: al cruzar
# sector y región, varios grupos quedan con muy pocas personas (varios
# con n de 1 o 2), lo que hace que sus promedios sean poco confiables.


## Declaración de autoría y uso de IA
# Herramienta utilizada: claude ia y chat gpt
# Para qué la usé: para fortalecer y terminar de entender mediante ejemplos
# la utilización de algunos comandos y verbos necesarios en el taller
# Qué hice yo: le envie mi pregunta y ver si era trabajable de alguna manera,
# al ir avanzando con el codigo le pedia correccion en multiples lineas de codigo
# que ne daba error
# Verificación confirmo que entiendo y puedo explicar todo lo que entrego.