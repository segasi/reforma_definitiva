### Paquetes ----
# install.packages("pacman") En caso de no tener el paquete pacman instalado
library(pacman)
p_load(broom, cowplot, haven, janitor, lubridate, patchwork, readxl, stringr, tidyr, tidyverse, writexl, zoo)


### Importar datos ----
pri <- read_excel("01_datos/cap_04/renuncias_pri_1987_2000.xlsx")
pri_gob <- read_excel("01_datos/cap_04/renuncias_aspirantes_gober_1987_2006.xlsx", sheet = "bd", na = "NA")
efecto_pri <- read_excel("01_datos/cap_04/efecto_renuncias_resultados_pri.xlsx", na = "NA")
efecto_opo <- read_excel("01_datos/cap_04/efecto_renuncias_resultados_oposicion.xlsx", na = "NA")

### Transformar/preparar datos del dataframe pri ----

# Corregir tres problemas con la columna "date" y después convertir esta columna a formato date
pri <- pri %>% 
  mutate(year = as.numeric(year), 
         date = ifelse(day == 30 & month == 9 & year == 1989, "09-30-1989", date),
         date = ifelse(day == 14 & month == 8 & year == 1995, "08-14-1995", date),
         date = ifelse(day == 16 & month == 8 & year == 1995, "08-16-1995", date), 
         date = mdy(date)) 

# Generar variables semestral
pri <- pri %>% 
  mutate(semestre = ifelse(month < 7, 1, 2),
         año_semestre = paste(year, semestre, sep = "-")) 


### Transformar/preparar datos del dataframe pri_gob ----

# Generar versiones logarítmicas de gsp y gsp.pc
pri_gob <- pri_gob %>% 
  mutate(lgsp = log(gsp),
         lgsp.pc = log(gsp.pc))

# Generar dataframe que solo incluye las observaciones entre 1987 y 2000
pri_gob_00 <- pri_gob %>% 
  filter(year <= 2000)


### Transformar/preparar datos de los dataframes efecto_pri y efecto_opo ----

efecto_pri <- clean_names(efecto_pri) 
efecto_opo <- clean_names(efecto_opo)

