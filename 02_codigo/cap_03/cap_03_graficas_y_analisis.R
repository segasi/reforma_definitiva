### Gráficas y análisis de la frecuencia de renuncias registradas en la base de datos "Renuncias de miembros del PRI, 1986-2006" ----

# Número total de renuncias + número y % de renuncias antes y después de la aprobación de la reforma
pri %>% 
  mutate(periodo = ifelse(date < "1996-08-01", "Antes", "Después")) %>% 
  group_by(periodo) %>% 
  summarise(num_renuncias = n()) %>% 
  ungroup() %>%  
  mutate(total_renuncias = sum(num_renuncias), 
         por = (num_renuncias/total_renuncias)*100) %>% 
  select(periodo, num_renuncias, por, total_renuncias)


# Número total de renuncias + número y % de renuncias entre 1987 - julio 1996 vs. agosto 1996 - diciembre 1998
pri %>% 
  mutate(periodo = ifelse(date < "1996-08-01", "_ene_1987-jul_1996", ifelse(date < "1998-12-31", "ago_1996-dic_1998", NA))) %>% 
  group_by(periodo) %>% 
  summarise(num_renuncias = n()) %>% 
  ungroup() %>%  
  mutate(total_renuncias = sum(num_renuncias), 
         por = (num_renuncias/total_renuncias)*100) %>% 
  select(periodo, num_renuncias, por, total_renuncias)


# Gráfica 3.1 ----
pri %>% 
  group_by(año_semestre) %>% 
  summarise(num_renuncias = n()) %>% 
  ungroup() %>%   
  mutate(total_renuncias = sum(num_renuncias), 
         por = (num_renuncias/total_renuncias)*100) %>% 
  select(año_semestre, num_renuncias, por, total_renuncias) %>% 
  ggplot(aes(año_semestre, num_renuncias)) +
  geom_col(fill = "grey60") +
  geom_vline(xintercept = 19.5, col = "grey20", size = 0.5, linetype = 2) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10), expand = c(0, 0)) +
  annotate("text", x = 17, y = 86.5, label = "Reforma", fontface = "bold", col = "grey30") +
  annotate("text", x = 17, y = 83.5, label = "Electoral", fontface = "bold", col = "grey30") +
  geom_segment(aes(x = 18.2, y = 85, xend = 19.2, yend = 85), alpha = 0.5, size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
  labs(x = NULL,
       y = "Núm. de renuncias") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_text(size = 15, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        panel.grid.major.y = element_line(linetype = 3, colour = "grey80", size = 0.2))

ggsave("fig_3.1.png", path = "04_graficas", width = 10, height = 6.67, dpi = 300)
ggsave("fig_3.1.jpeg", path = "04_graficas", width = 10, height = 6.67, dpi = 300)


# Análisis asociado a los datos de la Gráfica 3.1 ----

# Semestres con más de 40 deserciones
pri %>% 
  group_by(año_semestre) %>% 
  summarise(num_renuncias = n()) %>% 
  ungroup() %>%   
  mutate(total_renuncias = sum(num_renuncias), 
         por = (num_renuncias/total_renuncias)*100) %>% 
  select(año_semestre, num_renuncias, por, total_renuncias) %>% 
  filter(num_renuncias > 40)

# Total por semestre
pri %>% 
  group_by(año_semestre) %>% 
  summarise(num_renuncias = n()) %>% 
  ungroup() %>%   
  mutate(total_renuncias = sum(num_renuncias), 
         por = (num_renuncias/total_renuncias)*100) %>% 
  select(año_semestre, num_renuncias, por, total_renuncias) %>% 
  print(n = nrow(.))


# Renuncias de 1996, después de que se aprobó la reforma el 1 de agosto 
pri %>% 
  filter(year == 1996, 
         date > "1996-07-31") %>% 
  select(date) %>% 
  print(n = nrow(.))

# Renuncias del primer semestre 1998 por estado
pri %>% 
  filter(año_semestre == "1998-1") %>% 
  count(state, sort = T) %>% 
  mutate(total = sum(n),
         por = (n/total)*100)

# Renuncias del primer semestre 1996 por estado
pri %>% 
  filter(año_semestre == "1996-2") %>% 
  count(state, sort = T) %>% 
  mutate(total = sum(n),
         por = (n/total)*100)

# Renuncias del primer semestre 1999 por estado
pri %>% 
  filter(año_semestre == "1999-1") %>% 
  count(state, sort = T) %>% 
  mutate(total = sum(n),
         por = (n/total)*100)

### Heatmap de reuncias por estado (no incluída en el texto---
pri %>% 
  mutate(state = fct_rev(state)) %>% 
  group_by(año_semestre, state) %>% 
  summarise(num_renuncias = n()) %>% 
  ungroup() %>%   
  ggplot(aes(año_semestre, state)) +
  geom_tile(aes(fill = log(num_renuncias)), col = "grey60") +
  geom_text(aes(label = num_renuncias), size = 2.5) +
  scale_fill_gradient(low = "white", high = "salmon") +
  labs(x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none")



### Gráficas y análisis de la frecuencia de renuncias registradas en la base de datos "Deserciones de aspirantes a gobernador del PRI, 1987-2006" ----

# Gráfica 3.2 ----
pri_gob %>% 
  group_by(year) %>% 
  summarise(num_renuncias = sum(pri.res),
            num_no_renuncias = n() - num_renuncias) %>% 
  ungroup() %>%
  gather(key = variable, 
         value = valor, 
         -year) %>% 
  ggplot(aes(year, valor)) +
  geom_col(aes(fill = variable)) +
  geom_vline(xintercept = 1996, col = "grey20", size = 0.5, linetype = 2) +
  annotate("text", x = 1997, y = 10.7, label = "Reforma electoral", fontface = "bold", col = "grey30", hjust = 0) +
  geom_segment(aes(x = 1996.8, y = 10.7, xend = 1996.3, yend = 10.7), alpha = 0.5, size = 1, arrow = arrow(length = unit(0.3, "cm"))) +
  scale_x_continuous(breaks = 1987:2006, expand = c(0, 0), limits = c(1986.5, 2006.5)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, 11, 1), limits = c(0, 11.5)) +
  scale_fill_manual(values = c("grey70", "grey30"), labels = c("Sin renuncia", "Con renuncia")) +
  labs(x = NULL,
       y = "Núm. de procesos de selección de\ncandidatos a gobernador del PRI\n",
       fill = NULL) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 3, colour = "grey80", size = 0.2),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(hjust = 1, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.text = element_text(size = 12),
        legend.position = c(0.1, 0.92))

ggsave("fig_3.2.png", path = "04_graficas", width = 10, height = 6.67, dpi = 300)
ggsave("fig_3.2.jpeg", path = "04_graficas", width = 10, height = 6.67, dpi = 300)

# Número total de procesos de selección con al menos renuncia ----
pri_gob %>% 
  summarise(num_renuncias = sum(pri.res),
            num_elecciones = n()) %>% 
  mutate(por_renuncias = round((num_renuncias/num_elecciones)*100, 1))

# Número total de procesos de selección por año con al menos renuncia ----
pri_gob %>% 
  group_by(year) %>% 
  summarise(num_renuncias = sum(pri.res),
            num_elecciones = n()) %>% 
  ungroup() %>% 
  mutate(por_renuncias = round((num_renuncias/num_elecciones)*100, 1))


# Número de procesos de selección en los que hubo una, dos y tres renuncias ----
pri_gob %>% 
  mutate(una_renuncia = ifelse(!is.na(res.name1) & is.na(res.name2) & is.na(res.name3), 1, 0),
         dos_renuncias = ifelse(!is.na(res.name1) & !is.na(res.name2) & is.na(res.name3), 1, 0),
         tres_renuncias = ifelse(!is.na(res.name1) & !is.na(res.name2) & !is.na(res.name3), 1, 0)) %>% 
  summarise(sum_una_renuncia = sum(una_renuncia),
            sum_dos_renuncias = sum(dos_renuncias),
            sum_tres_renuncias = sum(tres_renuncias))

# Número de procesos de selección entre 1987 y 1995 en los que hubo al menos una renuncia ----
pri_gob %>% 
  filter(year < 1996) %>% 
  summarise(num_renuncias = sum(pri.res),
            num_elecciones = n()) %>%
  mutate(por_renuncias = round((num_renuncias/num_elecciones)*100, 1))

# Número de procesos de selección entre 1996 y 2006 en los que hubo al menos una renuncia ----
pri_gob %>% 
  filter(year >= 1996) %>% 
  summarise(num_renuncias = sum(pri.res),
            num_elecciones = n()) %>%
  mutate(por_renuncias = round((num_renuncias/num_elecciones)*100, 1))

# Número de procesos de selección entre 1996 y 2000 en los que hubo al menos una renuncia ----
pri_gob %>% 
  filter(year >= 1996 & year < 2001) %>% 
  summarise(num_renuncias = sum(pri.res),
            num_elecciones = n()) %>%
  mutate(por_renuncias = round((num_renuncias/num_elecciones)*100, 1))




### Modelos del Cuadro 3.2 ----

# Modelo 1 - Porcentaje del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada 

c4.2_m1 <- glm(pri.res ~ pri.pfshr + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1, family = binomial(link = "logit"), data = pri_gob)
summary(c4.2_m1)


# Modelo 2 - Porcentaje del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada + Efectos fijos regionales

c4.2_m2 <- glm(pri.res ~ pri.pfshr + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1 + north + bajio + center + south, family = binomial(link = "logit"), data = pri_gob)
summary(c4.2_m2)


# Modelo 3 - Margen del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + PResultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada

c4.2_m3 <- glm(pri.res ~ pri.pfmarg + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1, family = binomial(link = "logit"), data = pri_gob)
summary(c4.2_m3)

# Modelo 4 - Margen del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada + Efectos fijos regionales

c4.2_m4 <- glm(pri.res ~ pri.pfmarg + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1 + north + bajio + center + south, family = binomial(link = "logit"), data = pri_gob)
summary(c4.2_m4)

# Modelo 5 - Reforma electoral de 1996 + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada 

c4.2_m5 <- glm(pri.res ~ reform + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1, family = binomial(link = "logit"), data = pri_gob)
summary(c4.2_m5)


# Modelo 6 - Reforma electoral de 1996 + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada + Region fixed  effects
c4.2_m6 <- glm(pri.res ~ reform + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1 + north + bajio + center + south, family = binomial(link = "logit"), data = pri_gob)
summary(c4.2_m6)

##  Crear cuadro con todos los resultados

# Generar una función para transformar los resultados de los modelos de diferentes formas
fun_limpieza <- function(df) {
  
  tidy(df) %>% # Broomear los resultados de cada modelo
    as_data_frame() %>% 
    select(-statistic) %>% # Eliminar columna "statistic"
    
    # Transformar estructura de la base de datos
    gather(key = dimension, 
           value = valor, 
           -term, -p.value) %>% 
    arrange((term)) %>% 
    
    # Modificar cómo se presentan los valores y las etiquetas de los términos 
    mutate(valor = round(valor, 3),
           valor = as.character(valor),
           
           # Valor del error estándar entre ()
           valor = ifelse(dimension == "std.error", paste("(", valor, ")", sep = ""), valor), 
           
           # Agregar estrellas
           valor = ifelse(dimension == "estimate" & p.value < 0.01, paste(valor, "***", sep = ""), valor),
           valor = ifelse(dimension == "estimate" & p.value >= 0.01 & p.value < 0.05, paste(valor, "**", sep = ""), valor),
           valor = ifelse(dimension == "estimate" & p.value >= 0.05 & p.value < 0.1, paste(valor, "*", sep = ""), valor),
           
           # Recodificar nombres de términos 
           term = ifelse(term == "(Intercept)", "Constante", term),
           term = ifelse(term == "ch.gdppc.l1", "Crecimiento económico (rezagado)", term),
           term = ifelse(term == "fd.pre.shr", "Resultados electorales previos del PRI", term),
           term = ifelse(term == "marg", "Marginalización", term), 
           term = ifelse(term == "pri.pfshr", "Porcentaje del financiamiento público recibido por el PRI", term),
           term = ifelse(term == "pri.pfmarg", "Margen del financiamiento público recibido por el PRI", term),
           term = ifelse(term == "reform", "Reforma electoral de 1996", term),
           term = ifelse(term == "pri.res.l1", "Deserción del PRI rezagada", term)) %>% 
    select(-p.value)
}  

# Aplicar la función a todos los modelos
mod_1 <- fun_limpieza(c4.2_m1)
mod_2 <- fun_limpieza(c4.2_m2)
mod_3 <- fun_limpieza(c4.2_m3)
mod_4 <- fun_limpieza(c4.2_m4)
mod_5 <- fun_limpieza(c4.2_m5)
mod_6 <- fun_limpieza(c4.2_m6)

# Juntar todos los modelos
cuadro_3.2 <- mod_1 %>% 
  full_join(mod_2, by = c("term", "dimension")) %>% 
  rename(`Modelo 1 (91-06)` = valor.x,
         `Modelo 2 (91-06)` = valor.y) 

cuadro_3.2 <- cuadro_3.2 %>% 
  full_join(mod_3, by = c("term", "dimension")) %>% 
  rename(`Modelo 3 (91-06)` = valor)

cuadro_3.2 <- cuadro_3.2 %>% 
  full_join(mod_4, by = c("term", "dimension")) %>% 
  rename(`Modelo 4 (91-06)` = valor)

cuadro_3.2 <- cuadro_3.2 %>% 
  full_join(mod_5, by = c("term", "dimension")) %>% 
  rename(`Modelo 5 (87-06)` = valor)

cuadro_3.2 <- cuadro_3.2 %>% 
  full_join(mod_6, by = c("term", "dimension")) %>% 
  rename(`Modelo 6 (87-06)` = valor) 

# Eliminar variables regionales
cuadro_3.2 <- cuadro_3.2 %>% 
  filter(!term %in% c("bajio", "center", "north")) %>% 
  select(-dimension)


## Generar dataframe con las estadísticas generales de cada modelo

# Función para reestructurar los datos
fun_estadisticas <- function(modelo) {
  glance(modelo) %>% 
    select(df.null, logLik, AIC) %>% 
    rename(N = df.null,
           `Log likelihood` = logLik) %>% 
    mutate(N = round(N + 1, 0),
           `Log likelihood` = round(`Log likelihood`, 3),
           AIC = round(AIC, 3)) %>% 
    gather(key = term,
           value = modelo) 
}

# Aplicar la función a todos los modelos
est_c4.2_m1 <- fun_estadisticas(c4.2_m1)
est_c4.2_m2 <- fun_estadisticas(c4.2_m2)
est_c4.2_m3 <- fun_estadisticas(c4.2_m3)
est_c4.2_m4 <- fun_estadisticas(c4.2_m4)
est_c4.2_m5 <- fun_estadisticas(c4.2_m5)
est_c4.2_m6 <- fun_estadisticas(c4.2_m6)

# Agregar nombres de los modelos
est_c4.2_m1 <- est_c4.2_m1 %>% rename(`Modelo 1 (91-06)` = modelo)
est_c4.2_m2 <- est_c4.2_m2 %>% rename(`Modelo 2 (91-06)` = modelo)
est_c4.2_m3 <- est_c4.2_m3 %>% rename(`Modelo 3 (91-06)` = modelo)
est_c4.2_m4 <- est_c4.2_m4 %>% rename(`Modelo 4 (91-06)` = modelo)
est_c4.2_m5 <- est_c4.2_m5 %>% rename(`Modelo 5 (87-06)` = modelo)
est_c4.2_m6 <- est_c4.2_m6 %>% rename(`Modelo 6 (87-06)` = modelo)

# Unir todas las partes
est_generales <- est_c4.2_m1 %>% left_join(est_c4.2_m2, by = "term")
est_generales <- est_generales %>% left_join(est_c4.2_m3, by = "term")
est_generales <- est_generales %>% left_join(est_c4.2_m4, by = "term")
est_generales <- est_generales %>% left_join(est_c4.2_m5, by = "term")
est_generales <- est_generales %>% left_join(est_c4.2_m6, by = "term")

# Convertir el data.frame en un tibble 
est_gral_4.2 <- est_generales %>% as_data_frame()

# Unirlo al cuadro 4.2
cuadro_3.2 <- rbind(cuadro_3.2, est_gral_4.2)

# Ordenar términos
cuadro_3.2 <- cuadro_3.2 %>% 
  mutate(term = fct_relevel(term, "Porcentaje del financiamiento público recibido por el PRI", 
                            "Margen del financiamiento público recibido por el PRI", 
                            "Reforma electoral de 1996", 
                            "Crecimiento económico (rezagado)", 
                            "Resultados electorales previos del PRI", 
                            "Marginalización",
                            "Deserción del PRI rezagada",
                            "Constante",
                            "N",
                            "Log likelihood",
                            "AIC")) %>% 
  arrange(term)

# Guardar cuadro como archivo .xlsx
write_xlsx(cuadro_3.2, path = "03_datos_generados/cap_3/cap_3_tabla_3.2.xlsx", col_names = TRUE)


### Modelos del Cuadro 4.3 ----

# Modelo 1 - Porcentaje del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada 

c4.3_m1 <- glm(pri.res ~ pri.pfshr + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1, family = binomial(link = "logit"), data = pri_gob_00)
summary(c4.3_m1)


# Modelo 2 - Porcentaje del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada + Efectos fijos regionales

c4.3_m2 <- glm(pri.res ~ pri.pfshr + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1 + north + bajio + center + south, family = binomial(link = "logit"), data = pri_gob_00)
summary(c4.3_m2)


# Modelo 3 - Margen del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + PResultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada

c4.3_m3 <- glm(pri.res ~ pri.pfmarg + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1, family = binomial(link = "logit"), data = pri_gob_00)
summary(c4.3_m3)

# Modelo 4 - Margen del financiamiento público recibido por el PRI + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada + Efectos fijos regionales

c4.3_m4 <- glm(pri.res ~ pri.pfmarg + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1 + north + bajio + center + south, family = binomial(link = "logit"), data = pri_gob_00)
summary(c4.3_m4)

# Modelo 5 - Reforma electoral de 1996 + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada 

c4.3_m5 <- glm(pri.res ~ reform + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1, family = binomial(link = "logit"), data = pri_gob_00)
summary(c4.3_m5)


# Modelo 6 - Reforma electoral de 1996 + Crecimiento económico (rezagado) + Resultados electorales previos del PRI + Marginalización + Deserción del PRI rezagada + Region fixed  effects
c4.3_m6 <- glm(pri.res ~ reform + ch.gdppc.l1 + fd.pre.shr + marg + pri.res.l1 + north + bajio + center + south, family = binomial(link = "logit"), data = pri_gob_00)
summary(c4.3_m6)

##  Crear cuadro con todos los resultados

# Generar una función para transformar los resultados de los modelos de diferentes formas
fun_limpieza <- function(df) {
  
  tidy(df) %>% # Broomear los resultados de cada modelo
    as_data_frame() %>% 
    select(-statistic) %>% # Eliminar columna "statistic"
    
    # Transformar estructura de la base de datos
    gather(key = dimension, 
           value = valor, 
           -term, -p.value) %>% 
    arrange((term)) %>% 
    mutate(valor = round(valor, 3),
           valor = as.character(valor),
           
           # Valor del error estándar entre ()
           valor = ifelse(dimension == "std.error", paste("(", valor, ")", sep = ""), valor), 
           
           # Agregar estrellas
           valor = ifelse(dimension == "estimate" & p.value < 0.01, paste(valor, "***", sep = ""), valor),
           valor = ifelse(dimension == "estimate" & p.value >= 0.01 & p.value < 0.05, paste(valor, "**", sep = ""), valor),
           valor = ifelse(dimension == "estimate" & p.value >= 0.05 & p.value < 0.1, paste(valor, "*", sep = ""), valor),
           
           # Recodificar nombres de términos 
           term = ifelse(term == "(Intercept)", "Constante", term),
           term = ifelse(term == "ch.gdppc.l1", "Crecimiento económico (rezagado)", term),
           term = ifelse(term == "fd.pre.shr", "Resultados electorales previos del PRI", term),
           term = ifelse(term == "marg", "Marginalización", term), 
           term = ifelse(term == "pri.pfshr", "Porcentaje del financiamiento público recibido por el PRI", term),
           term = ifelse(term == "pri.pfmarg", "Margen del financiamiento público recibido por el PRI", term),
           term = ifelse(term == "reform", "Reforma electoral de 1996", term),
           term = ifelse(term == "pri.res.l1", "Deserción del PRI rezagada", term)) %>% 
    select(-p.value)
}  

# Aplicar la función a todos los modelos
mod_1 <- fun_limpieza(c4.3_m1)
mod_2 <- fun_limpieza(c4.3_m2)
mod_3 <- fun_limpieza(c4.3_m3)
mod_4 <- fun_limpieza(c4.3_m4)
mod_5 <- fun_limpieza(c4.3_m5)
mod_6 <- fun_limpieza(c4.3_m6)

# Juntar todos los modelos
cuadro_3.3 <- mod_1 %>% 
  full_join(mod_2, by = c("term", "dimension")) %>% 
  rename(`Modelo 1 (91-00)` = valor.x,
         `Modelo 2 (91-00)` = valor.y) 

cuadro_3.3 <- cuadro_3.3 %>% 
  full_join(mod_3, by = c("term", "dimension")) %>% 
  rename(`Modelo 3 (91-00)` = valor)

cuadro_3.3 <- cuadro_3.3 %>% 
  full_join(mod_4, by = c("term", "dimension")) %>% 
  rename(`Modelo 4 (91-00)` = valor)

cuadro_3.3 <- cuadro_3.3 %>% 
  full_join(mod_5, by = c("term", "dimension")) %>% 
  rename(`Modelo 5 (87-00)` = valor)

cuadro_3.3 <- cuadro_3.3 %>% 
  full_join(mod_6, by = c("term", "dimension")) %>% 
  rename(`Modelo 6 (87-00)` = valor) 

# Eliminar variables regionales
cuadro_3.3 <- cuadro_3.3 %>% 
  filter(!term %in% c("bajio", "center", "north")) %>% 
  select(-dimension)

## Generar dataframe con las estadísticas generales de cada modelo

# Función para reestructurar los datos
fun_estadisticas <- function(modelo) {
  glance(modelo) %>% 
    select(df.null, logLik, AIC) %>% 
    rename(N = df.null,
           `Log likelihood` = logLik) %>% 
    mutate(N = round(N + 1, 0),
           `Log likelihood` = round(`Log likelihood`, 3),
           AIC = round(AIC, 3)) %>% 
    gather(key = term,
           value = modelo) 
}

# Aplicar la función a todos los modelos
est_c4.3_m1 <- fun_estadisticas(c4.3_m1)
est_c4.3_m2 <- fun_estadisticas(c4.3_m2)
est_c4.3_m3 <- fun_estadisticas(c4.3_m3)
est_c4.3_m4 <- fun_estadisticas(c4.3_m4)
est_c4.3_m5 <- fun_estadisticas(c4.3_m5)
est_c4.3_m6 <- fun_estadisticas(c4.3_m6)

# Agregar nombres de los modelos
est_c4.3_m1 <- est_c4.3_m1 %>% rename(`Modelo 1 (91-00)` = modelo)
est_c4.3_m2 <- est_c4.3_m2 %>% rename(`Modelo 2 (91-00)` = modelo)
est_c4.3_m3 <- est_c4.3_m3 %>% rename(`Modelo 3 (91-00)` = modelo)
est_c4.3_m4 <- est_c4.3_m4 %>% rename(`Modelo 4 (91-00)` = modelo)
est_c4.3_m5 <- est_c4.3_m5 %>% rename(`Modelo 5 (87-00)` = modelo)
est_c4.3_m6 <- est_c4.3_m6 %>% rename(`Modelo 6 (87-00)` = modelo)

# Unir todas las partes
est_generales <- est_c4.3_m1 %>% left_join(est_c4.3_m2, by = "term")
est_generales <- est_generales %>% left_join(est_c4.3_m3, by = "term")
est_generales <- est_generales %>% left_join(est_c4.3_m4, by = "term")
est_generales <- est_generales %>% left_join(est_c4.3_m5, by = "term")
est_generales <- est_generales %>% left_join(est_c4.3_m6, by = "term")

# Convertir el data.frame en un tibble 
est_gral_4.3 <- est_generales %>% as_data_frame()

# Unirlo al cuadro 4.3
cuadro_3.3 <- rbind(cuadro_3.3, est_gral_4.3)

# Ordenar términos
cuadro_3.3 <- cuadro_3.3 %>% 
  mutate(term = fct_relevel(term, "Porcentaje del financiamiento público recibido por el PRI", 
                            "Margen del financiamiento público recibido por el PRI", 
                            "Reforma electoral de 1996", 
                            "Crecimiento económico (rezagado)", 
                            "Resultados electorales previos del PRI", 
                            "Marginalización",
                            "Deserción del PRI rezagada",
                            "Constante",
                            "N",
                            "Log likelihood",
                            "AIC")) %>% 
  arrange(term)


# Guardar cuadro como archivo .xlsx
write_xlsx(cuadro_3.3, path = "03_datos_generados/cap_3/cap_3_tabla_3.3.xlsx", col_names = TRUE)




### Gráfica 3.3: gráficas de los modelos 1 y 5 de los cuadros 4.2 y 4.3 ----

# Nota: Buena parte del código de esta sección se basa en esta hilo en Stack Overflow: https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression

## Modelo 1, Cuadro 4.2 ----

# Analizar valores sustantivos para construir la probabilidad predicha
range(pri_gob$pri.pfshr, na.rm = T)
median(pri_gob$ch.gdppc.l1)
median(pri_gob$fd.pre.shr)
median(pri_gob$marg)
mean(pri_gob$pri.res.l1)

# Genera nuevo data frame con valores fijos para todas las variables excepto pri.pfshr, misma que varía a lo largo de su rango (de 30% a 52%)
datos_nuevos <- data_frame(ch.gdppc.l1 = rep(median(pri_gob$ch.gdppc.l1), 23), fd.pre.shr = rep(median(pri_gob$fd.pre.shr), 23), marg = rep(median(pri_gob$marg), 23), pri.res.l1 = rep(0, 23), pri.pfshr = seq(30, 52, 1))

# Generar valores predichos
preds <- predict(c4.2_m1, datos_nuevos, type = "link", se.fit = TRUE)

# Calcular intervalo de confianza
critval <- 1.959964 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# Transformar valores usando el inverso del link
fit2 <- c4.2_m1$family$linkinv(fit)
upr2 <- c4.2_m1$family$linkinv(upr)
lwr2 <- c4.2_m1$family$linkinv(lwr)

# Guardar valores en el dataframe datos_nuevos
datos_nuevos$lwr <- lwr2 
datos_nuevos$upr <- upr2 
datos_nuevos$fit <- fit2

# Graficar
g_mod_1_4.2 <- datos_nuevos %>%
  ggplot() +
  geom_line(aes(pri.pfshr, y = fit), col = "black", size = 1) +
  geom_line(data = datos_nuevos, aes(pri.pfshr, y = upr), col= "grey70", size = 1, linetype = 2) + 
  geom_line(data = datos_nuevos, aes(pri.pfshr, y = lwr), col= "grey70", size = 1, linetype = 2) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(title = "Modelo 1 - Cuadro 4.2", 
       subtitle = "Datos de 1987-2006", 
       
       x = "Porcentaje del financiamiento\npúblico recibido por el PRI",
       y = "Probabilidad predicha") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, colour = "grey40"), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 11, hjust = 1,  margin = margin(10, 35, 0, 0)),
        axis.title.y = element_text(size = 11, hjust = 1, margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        panel.grid.major.y = element_line(linetype = 3, colour = "grey80", size = 0.3))


## Modelo 5, Cuadro 4.2 ----

# Analizar valores sustantivos para construir la probabilidad predicha
range(pri_gob$pri.pfshr, na.rm = T)
median(pri_gob$ch.gdppc.l1)
median(pri_gob$fd.pre.shr)
median(pri_gob$marg)
mean(pri_gob$pri.res.l1)

# Genera nuevo data frame con valores fijos para todas las variables excepto pri.pfshr, misma que varía a lo largo de su rango (de 30% a 52%)
datos_nuevos <- data_frame(ch.gdppc.l1 = rep(median(pri_gob$ch.gdppc.l1), 2), fd.pre.shr = rep(median(pri_gob$fd.pre.shr), 2), marg = rep(median(pri_gob$marg), 2), pri.res.l1 = rep(0, 2), reform = c(0, 1))

# Generar valores predichos
preds <- predict(c4.2_m5, datos_nuevos, type = "response")

# Gráfica 
g_mod_5_4.2 <- data_frame(preds, 
                          situacion = c("Antes de\nla reforma", 
                                        "Después de\nla reforma")) %>%
  ggplot(aes(situacion, preds)) +
  geom_col(fill = "grey60") +
  geom_text(aes(label = round(preds, 2)), fontface = "bold", vjust = -0.8) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(title = "Modelo 5 - Cuadro 4.2", 
       subtitle = "Datos de 1987-2006", 
       x = NULL,
       y = "Probabilidad predicha") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, colour = "grey40"), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 11, hjust = 1,  margin = margin(15, 15, 0, 0)),
        axis.title.y = element_text(size = 11, hjust = 1, margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        panel.grid.major.y = element_line(linetype = 3, colour = "grey80", size = 0.3))


## Modelo 1, Cuadro 4.3 ----

# Analizar valores sustantivos para construir la probabilidad predicha
range(pri_gob_00$pri.pfshr, na.rm = T)
median(pri_gob_00$ch.gdppc.l1)
median(pri_gob_00$fd.pre.shr)
median(pri_gob_00$marg)
mean(pri_gob_00$pri.res.l1)

# Genera nuevo data frame con valores fijos para todas las variables excepto pri.pfshr, misma que varía a lo largo de su rango (de 30% a 52%)
datos_nuevos <- data_frame(ch.gdppc.l1 = rep(median(pri_gob_00$ch.gdppc.l1), 23), fd.pre.shr = rep(median(pri_gob_00$fd.pre.shr), 23), marg = rep(median(pri_gob_00$marg), 23), pri.res.l1 = rep(0, 23), pri.pfshr = seq(30, 52, 1))

# Generar valores predichos
preds <- predict(c4.3_m1, datos_nuevos, type = "link", se.fit = TRUE)

# Calcular intervalo de confianza
critval <- 1.959964 ## approx 95% CI
upr <- preds$fit + (critval * preds$se.fit)
lwr <- preds$fit - (critval * preds$se.fit)
fit <- preds$fit

# Transformar valores usando el inverso del link
fit2 <- c4.3_m1$family$linkinv(fit)
upr2 <- c4.3_m1$family$linkinv(upr)
lwr2 <- c4.3_m1$family$linkinv(lwr)

# Guardar valores en el dataframe datos_nuevos
datos_nuevos$lwr <- lwr2 
datos_nuevos$upr <- upr2 
datos_nuevos$fit <- fit2

# Graficar
g_mod_1_4.3 <- datos_nuevos %>%
  ggplot() +
  geom_line(aes(pri.pfshr, y = fit), col = "black", size = 1) +
  geom_line(data = datos_nuevos, aes(pri.pfshr, y = upr), col= "grey70", size = 1, linetype = 2) + 
  geom_line(data = datos_nuevos, aes(pri.pfshr, y = lwr), col= "grey70", size = 1, linetype = 2) + 
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  labs(title = "Modelo 1 - Cuadro 4.3", 
       subtitle = "Datos de 1987-2000", 
       x = "Porcentaje del financiamiento\npúblico recibido por el PRI",
       y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"), 
        plot.subtitle = element_text(size = 13, hjust = 0.5, colour = "grey40"), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 11, hjust = 1,  margin = margin(10, 35, 0, 0)),
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        panel.grid.major.y = element_line(linetype = 3, colour = "grey80", size = 0.3))

# Cálculo de probabilidades descritas en el texto para ejemplicar el modelo 
datos_nuevos %>% 
  print(n = nrow(.))



## Modelo 5, Cuadro 4.3 ----

# Analizar valores sustantivos para construir la probabilidad predicha
range(pri_gob$pri.pfshr, na.rm = T)
median(pri_gob$ch.gdppc.l1)
median(pri_gob$fd.pre.shr)
median(pri_gob$marg)
mean(pri_gob$pri.res.l1)

# Genera nuevo data frame con valores fijos para todas las variables excepto pri.pfshr, misma que varía a lo largo de su rango (de 30% a 52%)
datos_nuevos <- data_frame(ch.gdppc.l1 = rep(median(pri_gob$ch.gdppc.l1), 2), fd.pre.shr = rep(median(pri_gob$fd.pre.shr), 2), marg = rep(median(pri_gob$marg), 2), pri.res.l1 = rep(0, 2), reform = c(0, 1))

# Generar valores predichos
preds <- predict(c4.3_m5, datos_nuevos, type = "response")

# Gráfica 
g_mod_5_4.3 <- data_frame(preds, 
                          situacion = c("Antes de\nla reforma", 
                                        "Después de\nla reforma")) %>%
  ggplot(aes(situacion, preds)) +
  geom_col(fill = "grey60") +
  geom_text(aes(label = round(preds, 2)), fontface = "bold", vjust = -0.8) +
  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +
  labs(title = "Modelo 5 - Cuadro 4.3", 
       subtitle = "Datos de 1987-2000", 
       x = NULL,
       y = "") +
  theme_classic() +
  theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 13, hjust = 0.5, colour = "grey40"), 
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 11, hjust = 1,  margin = margin(15, 15, 0, 0)),
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        panel.grid.major.y = element_line(linetype = 3, colour = "grey80", size = 0.3))


# Juntar gáficas
plot_grid(g_mod_1_4.2, g_mod_1_4.3, g_mod_5_4.2, g_mod_5_4.3, nrow = 2)

# Guardar las gráficas
ggsave("fig_3.3.png", path = "04_graficas", width = 10, height = 8, dpi = 300)
ggsave("fig_3.3.jpeg", path = "04_graficas", width = 10, height = 8, dpi = 300)


### Gráficas y análisis del efecto de las renuncias de priístas en los resultados electorales de ese partido ----

# Cambio promedio del % de votos del PRI en elecciones de goberndor en aquellas entidades en las que experimentó una renuncia (1) y donde no  (0)
efecto_pri %>% 
  group_by(defection) %>% 
  summarise(promedio = mean(gov_diff, na.rm = T)) %>% 
  ungroup()


# Cambio promedio del % de votos del PRI en elecciones de goberndor en aquellas entidades en las que experimentó una renuncia (1) y donde no  (0), antes de la reforma de 1996
efecto_pri %>% 
  filter(year < 1996) %>% 
  group_by(defection) %>% 
  summarise(promedio = mean(gov_diff, na.rm = T)) %>% 
  ungroup()


# Cambio promedio del % de votos del PRI en elecciones de goberndor en aquellas entidades en las que experimentó una renuncia (1) y donde no  (0), después de la reforma de 1996
efecto_pri %>% 
  filter(year > 1996) %>% 
  group_by(defection) %>% 
  summarise(promedio = mean(gov_diff, na.rm = T)) %>% 
  ungroup()


### Gráficas y análisis del efecto de las renuncias de priístas en los resultados electorales de la oposición ----


# Cambio promedio del % de votos del PRI en elecciones de goberndor en aquellas entidades en las que experimentó una renuncia (1) y donde no  (0)
efecto_opo %>% 
  group_by(defection) %>% 
  summarise(promedio = mean(v_shr_diff, na.rm = T)) %>% 
  ungroup()


# Cambio promedio del % de votos del PRI en elecciones de goberndor en aquellas entidades en las que experimentó una renuncia (1) y donde no (0), entre 1997 y 2000
efecto_opo %>% 
  filter(yr >= 1997 & yr <= 2000) %>% 
  group_by(defection) %>% 
  summarise(promedio = mean(v_shr_diff, na.rm = T)) %>% 
  ungroup()

