### Paquetes ----
# install.packages("pacman") En caso de no tener el paquete pacman instalado
library(pacman)
p_load(Amelia,
       haven,
       readxl,
       stringr,
       survival,
       tidyr,
       tidyverse,
       zoo
       # car, 
       # effects, 
       # foreign, 
       # ggplot2, 
       # KMsurv, 
       # MASS, 
       # splines, 
       # statnet, 
       # survival, 
       # Zelig
       )


### Importar y procesar datos ----


## Datos de Geddes, Wright y Franz (GWF) ----

# Cargar base de datos gwf.xlx. Esta base de datos incluye todos los regíemenes incluidos en la base de datos de GWF para el período 1945-2010
gwf <- read_excel("01_datos/cap_02/gwf.xlsx", 
                  col_types = c("skip", "numeric", "numeric", 
                                "numeric", "text", "text", "text", 
                                "text", "numeric", "text", "text", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "numeric", "numeric", 
                                "numeric", "text", "numeric"))


# Generar gwfsp  

# Este nuevo data frame es un subconjunto de la base de datos original y sólo incluyelas observaciones de los regímenes que ellos clasifican como party, party-military, party-military-personal y party-personal. 

# Además de generar este subconjunto, hago dos cambios a la base de datos original de GWF: 1) reclasifico el régimen de Sudáfrica como "party" (GWF lo clasifican como "oligarchy") para incluirlo en la base de datos. 2) Cambio la fecha del principio del régimen de partido dominante de México a 1929 (originalmente codificado en 1915) porque es entonces cuando se fundó el Partido Nacional Revolucionario (PNR), predecesor del PRI.
gwfsp <- gwf %>% 
  filter(year < 2010) %>%
  mutate(gwf.regimetype = ifelse(gwf.country == "South Africa" & gwf.regimetype == "oligarchy", "party", gwf.regimetype), #  Reclasifciar a Sudáfrica como régimen "party"
         gwf.casename = ifelse(gwf.country == "Mexico" & gwf.regimetype == "party", "Mexico 29-00", gwf.casename), # Cambiar nombre del caso de México
         gwf.spell = ifelse(gwf.country == "Mexico" & gwf.regimetype == "party", 71, gwf.spell),  # Cambiar duración total del régimen priísta
         gwf.duration = ifelse(gwf.country == "Mexico" & gwf.regimetype == "party", gwf.duration-14, gwf.duration)) %>%    # Cambiar contador de años acumulados del régimen priísta)
  filter(str_detect(gwf.regimetype, "party")) # Filtrar datos para solo mantener observaciones de regímenes de partido dominante


# Generar variables dicotómicas para regiones, tipo de régimen y otra para identificar regímenes impuestos desde el extranjero
gwfsp <- gwfsp %>% 
  mutate(gwf.cacar	= ifelse(gwf.country %in% c("Cuba", "El Salvador", "Honduras", "Mexico", "Nicaragua"), 1, 0),
         gwf.casia	= ifelse(gwf.country %in% c("Mongolia", "Afghanistan", "Turkmenistan", "Uzbekistan", "Sri Lanka", "Bangladesh"), 1, 0),
         gwf.ceeurope = ifelse(gwf.country %in% c("Albania" , "Soviet Union", "Bulgaria", "Hungary", "Yugoslavia", "Czechoslovakia", "Poland", "Romania", "Germany East", "Serbia"), 1, 0),
         gwf.easia = ifelse(gwf.country %in% c("China", "Vietnam", "Malaysia", "Taiwan", "Singapore", "Cambodia", "Laos", "Indonesia", "Korea North"), 1, 0),
         gwf.meast = ifelse(gwf.country %in% c("Syria", "Iraq", "Turkey", "South Yemen", "Iran"), 1, 0),
         gwf.nafrica = ifelse(gwf.country %in% c("Egypt", "Tunisia", "Algeria"), 1, 0),
         gwf.samerica = ifelse(gwf.country %in% c("Paraguay", "Bolivia", "Colombia"), 1, 0), 
         gwf.ssafrica = ifelse(gwf.country %in% c("Gabon", "South Africa", "Tanzania", "Botswana", "Senegal", "Ivory Coast", "Kenya", "Zambia", "Angola", "Mozambique", "Liberia", "Gambia","Zimbabwe", "Congo-Brz", "Guinea", "Rwanda", "Sierra Leone",	"Cameroon", "Burundi", "Namibia", "Ethiopia", "Eritrea", "Lesotho", "Chad", "Niger", "Madagascar", "Mali", "Ghana", "Guinea Bissau"), 1, 0), # Generar variable dicotómicas de regiones
         fimposed = ifelse(gwf.country == "Afghanistan" & year > 1978 & year < 1994 | gwf.country == "Bulgaria" & year > 1946 & year < 1991 | gwf.country == "Cambodia" & year > 1978 & year < 1991 | gwf.country == "Czechoslovakia" & year > 1947 & year < 1991 | gwf.country == "Germany East" & year > 1944 & year < 1991 | gwf.country == "Hungary" & year > 1948 & year < 1991  | gwf.country == "Poland" & year > 1946 & year < 1990, 1, 0), # Creación de variable fimposed, para identificar regímenes impuestos por potencias extranjeras
         gwf.party = ifelse(gwf.regimetype == "party", 1, 0), # Dummy partido
         gwf.partymil = ifelse(gwf.regimetype == "party-military", 1, 0), # Dummy partido militar
         gwf.partymilper = ifelse(gwf.regimetype == "party-military-personal", 1, 0), # Dummy partido militar personalist
         gwf.partyper = ifelse(gwf.regimetype == "party-personal", 1, 0)) # Dummy partido personalista


## Datos de Penn World Table (PWT) 7.0 ----
pwt7.0_original <- read.csv("01_datos/cap_02/PWT_7.0.csv", na.strings="NA", header = TRUE, dec = ".", sep =",")

# Renombrar países y calcular diversas variables: 1) Valor rezagado de gdppc; 2) cambio anual de gdppc; 3) cambio anual de gdppc rezagado entre 1 y 11 períodos; 4) Desviación del valor anula del cambio del gdppc respecto al promedio de los últimos 5 a 10 años
pwt7.0 <- pwt7.0_original %>% 
  select(country, year, pwt7.rgdpch)  %>% 
  arrange(country, year) %>% 
  mutate(pwt7.country = as.character(country), 
         pwt7.country = ifelse(pwt7.country == "China Version 1", "China", ifelse(pwt7.country == "Congo, Republic of", "Congo-Brz", ifelse(pwt7.country == "Gambia, The", "Gambia", ifelse(pwt7.country == "Guinea-Bissau", "Guinea Bissau", ifelse(pwt7.country == "Cote d`Ivoire", "Ivory Coast", pwt7.country)))))) %>% # Renombrar países
  group_by(pwt7.country) %>% 
  mutate(pwt7.rgdpchL1 = lag(pwt7.rgdpch), # Valor de pwt7.rgdpch rezagado 1 año
         pwt7.rgdpchL1.log = log(pwt7.rgdpchL1), # Logaritmo natural Valor de pwt7.rgdpch rezagado 1 año
         pwt7.rgdpch.ch = (( pwt7.rgdpch - pwt7.rgdpchL1)/pwt7.rgdpchL1)*100, # Cambio porcentual anual de gdppc
         pwt7.rgdpch.chL1 = lag(pwt7.rgdpch.ch),    # Cambio porcentual anual rezagado 1 año
         pwt7.rgdpch.chL2 = lag(pwt7.rgdpch.ch, 2), # Cambio porcentual anual rezagado 2 años
         pwt7.rgdpch.chL3 = lag(pwt7.rgdpch.ch, 3), # Cambio porcentual anual rezagado 3 años
         pwt7.rgdpch.chL4 = lag(pwt7.rgdpch.ch, 4), # Cambio porcentual anual rezagado 4 años
         pwt7.rgdpch.chL5 = lag(pwt7.rgdpch.ch, 5), # Cambio porcentual anual rezagado 5 años
         pwt7.rgdpch.chL6 = lag(pwt7.rgdpch.ch, 6), # Cambio porcentual anual rezagado 6 años
         pwt7.rgdpch.chL7 = lag(pwt7.rgdpch.ch, 7), # Cambio porcentual anual rezagado 7 años
         pwt7.rgdpch.chL8 = lag(pwt7.rgdpch.ch, 8), # Cambio porcentual anual rezagado 8 años
         pwt7.rgdpch.chL9 = lag(pwt7.rgdpch.ch, 9), # Cambio porcentual anual rezagado 9 años
         pwt7.rgdpch.chL10 = lag(pwt7.rgdpch.ch, 10), # Cambio porcentual anual rezagado 10 años
         pwt7.rgdpch.chL11 = lag(pwt7.rgdpch.ch, 11)) %>%  # Cambio porcentual anual rezagado 11 años
  mutate(pwt7.twoyma	= rollapply(pwt7.rgdpch.chL1, 2, mean, align = "right", fill = NA), # promedio móvil de dos años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.threeyma	= rollapply(pwt7.rgdpch.chL1, 3, mean, align = "right", fill = NA), # promedio móvil de tres años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.fouryma = rollapply(pwt7.rgdpch.chL1, 4, mean, align = "right", fill = NA), # promedio móvil de cuatro años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.fiveyma = rollapply(pwt7.rgdpch.chL1, 5, mean, align = "right", fill = NA), # promedio móvil de cinco años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.sixyma = rollapply(pwt7.rgdpch.chL1, 6, mean, align = "right", fill = NA), # promedio móvil de seis años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.sevenyma = rollapply(pwt7.rgdpch.chL1, 7, mean, align = "right", fill = NA), # promedio móvil de siete años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.eightyma = rollapply(pwt7.rgdpch.chL1, 8, mean, align = "right", fill = NA), # promedio móvil de ocho años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.nineyma = rollapply(pwt7.rgdpch.chL1, 9, mean, align = "right", fill = NA), # promedio móvil de nueve años del cambio porcentual anual rezagado un año (mediante align = "right")
         pwt7.tenyma = rollapply(pwt7.rgdpch.chL1, 10, mean, align = "right", fill = NA)) %>%   # promedio móvil de diez años del cambio porcentual anual rezagado un año (mediante align = "right")
  mutate(pwt7.devfiveyma	= pwt7.rgdpch.ch - pwt7.fiveyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos cinco años, rezagado un año 
         pwt7.devsixyma = pwt7.rgdpch.ch - pwt7.sixyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos seis años, rezagado un año
         pwt7.devsevenyma = pwt7.rgdpch.ch - pwt7.sevenyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos siete años, rezagado un año
         pwt7.deveightyma = pwt7.rgdpch.ch - pwt7.eightyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos ocho años, rezagado un año
         pwt7.devnineyma = pwt7.rgdpch.ch - pwt7.nineyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos nueve años, rezagado un año 
         pwt7.devtenyma = pwt7.rgdpch.ch - pwt7.tenyma) # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos diez años, rezagado un año 


## Datos de Maddison ----
mad_original <- read_excel("01_datos/cap_02/Maddison - Eco Indicators 1500-2008.xls", sheet = "PerCapita GDP", range = "A3:GR194")

# Renombrar variable, eliminar columnas vacías, renombrar países y eliminar datos de categorias reguionales incluidas en la base de datos original
mad <- mad_original %>% 
  rename(mad.country = X__1) %>% # Renombrar columna del país
  select(mad.country, which(sapply(.,class)=="numeric")) %>%  # Eliminar columnas vacías en la base de datos
  mutate(mad.country = ifelse(mad.country == "Total Former USSR", "USSR", ifelse(mad.country == "Indonesia (including Timor until 1999)", "Indonesia", ifelse(mad.country == "Congo 'Brazzaville'", "Congo-Brz", ifelse(mad.country == "Côte d'Ivoire", "Ivory Coast", ifelse(mad.country == "North Korea", "Korea North", ifelse(mad.country == "Serbia/Montenegro/Kosovo", "Serbia", ifelse(mad.country == "USSR", "Soviet Union", mad.country)))))))) %>%  # Renombrar países
  filter(!mad.country %in% c("15 Latin American countries", "15 West Asian countries", "16 East Asian countries", "30 East Asian countries", "57 African countries", "8 Latin American countries", "Western Europe", "Western Offshoots", "Total 12 Western Europe", "Total 14 small west European countries", "Total 15 Latin American countries", "Total 15 West Asian countries", "Total 16 East Asian countries", "Total 21 small Caribbean countries", "Total 24 Small East Asian countries", "Total 3 Small African countries", "Total 30  Western Europe", "Total 30 East Asian countries", "Total 7 East European countries", "Total 8 Latin American countries", "Total 7 East European Countries",  "Total Africa", "Total Asia", "Successor Republics of USSR", "Total Latin America", "Total Western Offshoots", "Former Czechoslovakia", "Former Yugoslavia")) # Filtrar


# Transformar el formato del data frame mad para tener tres columnas: una para el nombre del país, otra para el año y otra para el valor de PIB per cápita 
mad <- mad %>% 
  gather(key = "year",
         value = gdppc, 
         -mad.country)

# Transformar tipo de variable de year de character a numeric; generar variable id concatenando el nombre del país y el año; filtrar data frame para sólo tener datos a partir de 1946 (año en que empieza el data frame de Geddes); 
mad <- mad %>% 
  mutate(id = paste(mad.country, year, sep = ""),
         year = as.numeric(year)) %>% 
  filter(year >= 1945) %>% 
  select(mad.country, year, id, gdppc)

# Calcular diversas variables: 1) Valor rezagado de gdppc; 2) cambio anual de gdppc; 3) cambio anual de gdppc rezagado entre 1 y 11 períodos; 4) Desviación del valor anula del cambio del gdppc respecto al promedio de los últimos 5 a 10 años
mad <- mad %>% 
  arrange(mad.country, year) %>% 
  rename(mad.gdppc = gdppc) %>% 
  group_by(mad.country) %>%
  mutate(mad.gdppcL1 = lag(mad.gdppc), # Valor de gdppc rezagado 1 año
         mad.gdppcL1.log = log(mad.gdppcL1), # Logaritmo natural Valor de mad.gdppcL1 rezagado 1 año
         mad.chgdppc = ((mad.gdppc - mad.gdppcL1)/mad.gdppcL1)*100, # Cambio porcentual anual de gdppc
         mad.chgdppcL1 = lag(mad.chgdppc),    # Cambio porcentual anual rezagado 1 año
         mad.chgdppcL2 = lag(mad.chgdppc, 2), # Cambio porcentual anual rezagado 2 años
         mad.chgdppcL3 = lag(mad.chgdppc, 3), # Cambio porcentual anual rezagado 3 años
         mad.chgdppcL4 = lag(mad.chgdppc, 4), # Cambio porcentual anual rezagado 4 años
         mad.chgdppcL5 = lag(mad.chgdppc, 5), # Cambio porcentual anual rezagado 5 años
         mad.chgdppcL6 = lag(mad.chgdppc, 6), # Cambio porcentual anual rezagado 6 años
         mad.chgdppcL7 = lag(mad.chgdppc, 7), # Cambio porcentual anual rezagado 7 años
         mad.chgdppcL8 = lag(mad.chgdppc, 8), # Cambio porcentual anual rezagado 8 años
         mad.chgdppcL9 = lag(mad.chgdppc, 9), # Cambio porcentual anual rezagado 9 años
         mad.chgdppcL10 = lag(mad.chgdppc, 10), # Cambio porcentual anual rezagado 10 años
         mad.chgdppcL11 = lag(mad.chgdppc, 11)) %>%  # Cambio porcentual anual rezagado 11 años
  mutate(mad.twoyma	= rollapply(mad.chgdppcL1, 2, mean, align = "right", fill = NA), # promedio móvil de dos años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.threeyma	= rollapply(mad.chgdppcL1, 3, mean, align = "right", fill = NA), # promedio móvil de tres años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.fouryma = rollapply(mad.chgdppcL1, 4, mean, align = "right", fill = NA), # promedio móvil de cuatro años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.fiveyma = rollapply(mad.chgdppcL1, 5, mean, align = "right", fill = NA), # promedio móvil de cinco años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.sixyma = rollapply(mad.chgdppcL1, 6, mean, align = "right", fill = NA), # promedio móvil de seis años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.sevenyma = rollapply(mad.chgdppcL1, 7, mean, align = "right", fill = NA), # promedio móvil de siete años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.eightyma = rollapply(mad.chgdppcL1, 8, mean, align = "right", fill = NA), # promedio móvil de ocho años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.nineyma = rollapply(mad.chgdppcL1, 9, mean, align = "right", fill = NA), # promedio móvil de nueve años del cambio porcentual anual rezagado un año (mediante align = "right")
         mad.tenyma = rollapply(mad.chgdppcL1, 10, mean, align = "right", fill = NA)) %>%   # promedio móvil de diez años del cambio porcentual anual rezagado un año (mediante align = "right")
  mutate(mad.devfiveyma	= mad.chgdppc - mad.fiveyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos cinco años, rezagado un año 
         mad.devsixyma = mad.chgdppc - mad.sixyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos seis años, rezagado un año
         mad.devsevenyma = mad.chgdppc - mad.sevenyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos siete años, rezagado un año
         mad.deveightyma = mad.chgdppc - mad.eightyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos ocho años, rezagado un año
         mad.devnineyma = mad.chgdppc - mad.nineyma, # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos nueve años, rezagado un año 
         mad.devtenyma = mad.chgdppc - mad.tenyma) # Desviación del cambio anual de gdppc respecto al promedio móvil del cambio de los últimos diez años, rezagado un año


## Datos de precios de gas y petróleo de Ross ----
ross_original <- read_dta("01_datos/cap_02/Ross Oil & Gas 1932-2009 public.dta")

# Seleccionar variables, renombrar variable y renombrar países
ross <- ross_original %>% 
  select(cty_name, year, oil_gas_valuePOP_2009) %>% 
  rename(ross.country = cty_name, # Renombrar columna del país
         ross.year = year,
         ross.precio = oil_gas_valuePOP_2009) %>%  # Renombrar columna de variable relevante
  mutate(ross.precioL1 = lag(ross.precio), # Rezagar ross.precio un año
         ross.precioL1.log = log(ross.precioL1 + 1), # Logaritmo de la versión rezagada de ross.precio
         ross.country = ifelse(ross.country == "Congo, Rep.", "Congo-Brz", ifelse(ross.country == "Cote d'Ivoire", "Ivory Coast", ifelse(ross.country == "Egypt, Arab Rep.", "Egypt", ifelse(ross.country == "Gambia, The", "Gambia", ifelse(ross.country == "Guinea-Bissau", "Guinea Bissau", ifelse(ross.country == "Iran, Islamic Rep.", "Iran", ifelse(ross.country == "Lao PDR", "Laos", ifelse(ross.country == "Korea, Dem. Rep.", "Korea North", ifelse(ross.country == "Syrian Arab Republic", "Syria", ifelse(ross.country == "Russian Federation", "Soviet Union", ifelse(ross.country == "Yugoslavia, Fed. Rep.", "Yugoslavia", ross.country))))))))))))
### Unir data frames ----

# Unión 1
u1 <- gwfsp %>% 
  left_join(pwt7.0, by= c("gwf.country" = "pwt7.country", "year" = "year"))

# Unión 2
u2 <- u1 %>% 
  left_join(mad, by= c("gwf.country" = "mad.country", "year" = "year"))

# Unión 3 - En esta unión aparece una advertencia porque la base de datos de Ross llega hasta 2009, mientras que la de GWF llega hasta 2010
u3 <- u2 %>% 
  left_join(ross, by= c("gwf.country" = "ross.country", "year" = "ross.year"))

# Comparación de dimensiones
dim(gwfsp)
dim(u1)
dim(u2)
dim(u3)


## Mapas de observaciones faltantes por renglón. Nota: las columnas y renglones están ordendos en el orden inverso al del data frame.

# Mapa de todas las observaciones
missmap(u3, col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)

# Mapas de países seleccionados porque tienen el mayor número de observaciones faltantes
missmap(u3[u3$gwf.country == "Eritrea",], col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)
missmap(u3[u3$gwf.country == "Ethiopia",], col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)
missmap(u3[u3$gwf.country == "Czechoslovakia",], col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)
missmap(u3[u3$gwf.country == "Germany East",], col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)
missmap(u3[u3$gwf.country == "Korea North",], col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)
missmap(u3[u3$gwf.country == "South Yemen",], col = c("#c04d50","#4f81bd"), main= "", x.cex = 0.01, legend=FALSE)

## Guardar data frame u3 como sp para usarla en el análisis y guardar archivo .csv
sp <- u3
write_csv(sp, "03_datos_generados/sp.csv")

# Generar archivo con terminación .dta para llevar a cabo el análisis de los modelos de supervivencia en STATA (ver código incluido en el mismo folder que este archivo)
sp_dta <- sp %>% 
  rename_(.dots=setNames(names(.), tolower(gsub("\\.", "_", names(.)))))

write_dta(sp_dta, "03_datos_generados/sp.dta", version = 12)
