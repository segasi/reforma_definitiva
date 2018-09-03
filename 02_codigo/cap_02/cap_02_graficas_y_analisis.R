### Paquetes ----
library(pacman)
p_load(cowplot, flexsurv, readr, readxl, tidyverse, 
       stringr, survival, zeligverse)

### Importar y procesar datos ----
source("02_codigo/cap_02/cap_02_cargar_unir_datos.R") # Corre el script 'cap_02_cargar_unir_datos.R' (incluido en el mismo folder que este archivo) para importar y transformar los datos


### Gráficas y análisis de universo de regímenes de partido dominante ----

### Figura 2.1 ----
sp %>% 
  group_by(year) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual)) %>%
  ggplot() +
  geom_line(aes(year, num_acumulado), size = 1.2) +
  geom_line(aes(year, num_anual), size = 1.2, col = "#66666680", linetype = 3) +
  scale_x_continuous(breaks = c(1946, seq(1950, 2000, 10), 2009), 
                     limits = c(1946, 2009)) +
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0, 60)) +
  annotate("segment", x = 1946, xend = 1949, y = 58, yend = 58, size = 1.2) +
  annotate("segment", x = 1946, xend = 1949, y = 55, yend = 55, size = 1.2, col = "#66666680", linetype = 3) +
  annotate("text", x = 1950.5, y = 58, label = "Frecuencia acumulada", size = 5, hjust = 0) +
  annotate("text", x = 1950.5, y = 55, label = "Frecuencia anual", size = 5, hjust = 0) +
  labs(x = "",
       y = "Número") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 15, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"))

ggsave("fig_2.1.png", path = "04_graficas", width = 10, height = 6.67, dpi = 300)
ggsave("fig_2.1.jpeg", path = "04_graficas", width = 10, height = 6.67, dpi = 300)

# Análisis asociado a Figura 2.1 ----

# Número de regímens que terminaron cada año y número acumulado
sp %>% 
  group_by(year) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por = round((num_anual/last(num_acumulado))*100, 2),
         por_acumulado =cumsum(por)) %>% 
  print(n = nrow(.))

# Número de años en los que terminó un régimen de partido dominante
sp %>% 
  group_by(year) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  summarise(sum(num_anual != 0)) 


# Número de regímenes que concluyeron entre 1946 y 1959
sp %>% 
  group_by(year) %>% 
  filter(year >= 1946, 
         year <= 1959) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%
  print(n = nrow(.))

# Número de regímenes que concluyeron entre 1960 y 1969
sp %>% 
  group_by(year) %>% 
  filter(year >= 1960, 
         year <= 1969) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%  
  print(n = nrow(.))

# Número de regímenes que concluyeron entre 1970 y 1979
sp %>% 
  group_by(year) %>% 
  filter(year >= 1970, 
         year <= 1979) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%  
  print(n = nrow(.))

  
# Número de regímenes que concluyeron entre 1980 y 1989 
sp %>% 
  group_by(year) %>% 
  filter(year >= 1980, 
         year <= 1989) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%  
  print(n = nrow(.))

# Número de regímenes que concluyeron entre 1990 y 1999
sp %>% 
  group_by(year) %>% 
  filter(year >= 1990, 
         year <= 1999) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%  
  print(n = nrow(.))

# Número de regímenes que concluyeron entre 2000 y 2009 
sp %>% 
  group_by(year) %>% 
  filter(year >= 2000, 
         year <= 2009) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%
  print(n = nrow(.))


# Número de regímenes que concluyeron entre 1989 y 1994
sp %>% 
  group_by(year) %>% 
  filter(year >= 1989, 
         year <= 1994) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%  
  print(n = nrow(.))

# Número de regímenes que concluyeron entre 1989 y 1994
sp %>% 
  group_by(year) %>% 
  filter(year >= 1999, 
         year <= 2000) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  mutate(num_acumulado = cumsum(num_anual),
         por_acumulado = round((num_acumulado/52)*100, 2)) %>%  
  print(n = nrow(.))


# Promedio de regímenes que concluyeron cada año entre 1946 y 1952
sp %>% 
  group_by(year) %>% 
  filter(year >= 1946, 
         year <= 1952) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  ungroup() %>% 
  summarise(promedio = sum(num_anual)/length(num_anual))


# Promedio de regímenes que concluyeron cada año entre 1953 y 1988
sp %>% 
  group_by(year) %>% 
  filter(year >= 1953, 
         year <= 1988) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  ungroup() %>% 
  summarise(promedio = sum(num_anual)/length(num_anual))


# Promedio de regímenes que concluyeron cada año entre 1989 y 1994
sp %>% 
  group_by(year) %>% 
  filter(year >= 1989, 
         year <= 1994) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  ungroup() %>% 
  summarise(promedio = sum(num_anual)/length(num_anual))

# Promedio de regímenes que concluyeron cada año entre 1989 y 2000
sp %>% 
  group_by(year) %>% 
  filter(year >= 1989, 
         year <= 2000) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  ungroup() %>% 
  summarise(promedio = sum(num_anual)/length(num_anual))

# Promedio de regímenes que concluyeron cada año entre 2000 y 2010
sp %>% 
  group_by(year) %>% 
  filter(year >= 2003, 
         year <= 2010) %>% 
  summarise(num_anual = sum(gwf.fail)) %>% 
  ungroup() %>% 
  summarise(promedio = sum(num_anual)/length(num_anual))



### Figura 2.2 ----
sp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration), # Variable que registra el último valor de gwf.duration 
            termino = last(gwf.fail)) %>%  # Variable que registra si el régimen terminó o no
  ungroup() %>% 
  ggplot() +
  geom_density(aes(duracion), size = 1.2) +
  geom_density(aes(duracion, col = factor(termino), linetype = factor(termino)), size = 1.2) +
  scale_color_manual(values = c("grey", "#666666")) +
  scale_linetype_manual(values=c(2, 3)) +
  scale_y_continuous(limits = c(0, 0.025)) +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  annotate("segment", x = 58, xend = 61, y = 0.025, yend = 0.025, size = 1.2) +
  annotate("segment", x = 58, xend = 61, y = 0.0235, yend = 0.0235, size = 1.2, colour = "#666666", linetype = 3) +
  annotate("segment", x = 58, xend = 61, y = 0.022, yend = 0.022, size = 1.2, colour = "grey", linetype = 2) +
  annotate("text", x = 63, y = 0.025, label = "Todos los regímenes (n = 76)", size = 5, hjust = 0) +
  annotate("text", x = 63, y = 0.0235, label = "Concluidos (n = 52)", size = 5, hjust = 0) +
  annotate("text", x = 63, y = 0.022, label = "Continuaban en 2010 (n = 24)", size = 5, hjust = 0) +
  labs(x = "Años en el poder",
       y = "Densidad") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 15, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 15, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")

# Guardar gráfica
ggsave("fig_2.2.png", path = "04_graficas", width = 10, height = 6.67, dpi = 300)
ggsave("fig_2.2.jpeg", path = "04_graficas", width = 10, height = 6.67, dpi = 300)

# Análisis asociado a figura 2.2 ----

# Duración individual de todos los regímenes considerados
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail)) %>% 
  arrange(-duracion) %>% 
  print(n = nrow(.))

# Duración individual de los regímenes que terminaron
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail)) %>% 
  filter(termino == 1) %>% 
  arrange(-duracion) %>% 
  print(n = nrow(.))


# Duración individual de los regímenes que continuaban en 2010
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail)) %>% 
  filter(termino == 0) %>% 
  arrange(-duracion) %>% 
  print(n = nrow(.))


# Duración promedio de todos los regímenes considerados
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail),
            fimposed = last(fimposed)) %>%
  ungroup() %>% 
  summarise(promedio = mean(duracion, na.rm = T),
            mediana = median(duracion, na.rm = T))

# Duración promedio de todos los regímenes considerados, distinguiendo si fueron impuestos o no por una potencia extranjera
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail),
            fimposed = last(fimposed)) %>%
  ungroup() %>% 
  group_by(fimposed) %>% 
  summarise(promedio = mean(duracion, na.rm = T),
            mediana = median(duracion, na.rm = T)) %>% 
  ungroup()


# Duración promedio de los regímenes, distinguiendo si terminaron o continuaban en el poder en 2010
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail),
            fimposed = last(fimposed)) %>%
  ungroup() %>% 
  group_by(termino) %>% 
  summarise(promedio = mean(duracion, na.rm = T),
            mediana = median(duracion, na.rm = T))  %>% 
  ungroup()


# Duración promedio de los regímenes, distinguiendo si fueron impuestos o no por una potencia extranjera, así como si terminaron o continuaban en el poder en 2010
gwfsp %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration),
            termino = last(gwf.fail),
            fimposed = last(fimposed)) %>%
  ungroup() %>% 
  group_by(termino, fimposed) %>% 
  summarise(promedio = mean(duracion, na.rm = T),
            mediana = median(duracion, na.rm = T))  %>% 
  ungroup()



### Intro sección 2.1.2 ----

# Datos mencionados en la intro de la sección 2.1.2
sp %>%
  filter(gwf.casename == "Cameroon 60-83") %>% 
  mutate(prom_mov_siete = rollapply(pwt7.rgdpch.ch, 7, mean, align = "right", fill = NA),
         dif_prom_mov_siete = pwt7.rgdpch.ch - prom_mov_siete) %>%
  select(year, gwf.casename,  pwt7.rgdpch.ch, dif_prom_mov_siete) %>% 
  print(n = nrow(.))

sp %>%
  filter(gwf.casename == "Senegal 60-00") %>% 
  mutate(prom_mov_siete = rollapply(pwt7.rgdpch.ch, 7, mean, align = "right", fill = NA),
         dif_prom_mov_siete = pwt7.rgdpch.ch - prom_mov_siete) %>%
  select(year, gwf.casename,  pwt7.rgdpch.ch, dif_prom_mov_siete) %>% 
  print(n = nrow(.))

### Figura 2.3  ----
g1 <- sp %>%
  filter(!str_detect(gwf.casename, "NA")) %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  ggplot(aes(años_previos_transicion, pwt7.rgdpch.ch, group = años_previos_transicion)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "grey70", linetype = 2) +
  scale_x_reverse(breaks = seq(55, 0, -5), labels = seq(-55, 0, 5), limits = c(50, -1)) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(title = "Panel (a) - Cambio en PIB per capita (46 regímenes)", 
       x = "Número de años antes de la transición",
       y = "Cambio porcentual") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(0, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")

g2 <- sp %>%
  filter(!str_detect(gwf.casename, "NA")) %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  ggplot(aes(años_previos_transicion, pwt7.fiveyma, group = años_previos_transicion)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "grey70", linetype = 2) +
  scale_x_reverse(breaks = seq(55, 0, -5), labels = seq(-55, 0, 5), limits = c(50, -1)) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(title = "Panel (b) - Promedio móvil de cinco años (44 regímenes)", 
       x = "Número de años antes de la transición",
       y = "Cambio porcentual") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(15, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")


g3 <- sp %>%
  filter(!str_detect(gwf.casename, "NA")) %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  ggplot(aes(años_previos_transicion, pwt7.devsevenyma, group = años_previos_transicion)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "grey70", linetype = 2) +
  scale_x_reverse(breaks = seq(55, 0, -5), labels = seq(-55, 0, 5), limits = c(50, -1)) +
  scale_y_continuous(limits = c(-20, 20)) +
  labs(title = "Panel (c) - Desviación del promedio móvil de siete años (43 regímenes)", 
       x = "Número de años antes de la transición",
       y = "Cambio porcentual") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(15, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")
  
# Unir gráficas
plot_grid(g1, g2, g3, nrow = 3, ncol = 1, align = 'h')
ggsave("fig_2.3.png", path = "04_graficas", width = 10, height = 12, dpi = 300)
ggsave("fig_2.3.jpeg", path = "04_graficas", width = 10, height = 12, dpi = 300)

## Misma gráfica pero sin restringir rangos de cada una. Esta segunda versión NO se incluye en el libro

g1a <- sp %>%
  filter(!str_detect(gwf.casename, "NA"), 
         gwf.duration < 51) %>%
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ggplot(aes(años_previos_transicion, pwt7.rgdpch.ch, group = años_previos_transicion)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "grey70", linetype = 2) +
  scale_x_reverse(breaks = seq(50, 0, -5), labels = seq(-50, 0, 5)) +
  labs(title = "Cambio en PIB per capita (46 regímenes)", 
       x = "Número de años antes de la transición",
       y = "Cambio porcentual") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 20, hjust = 0, face = "bold", margin = margin(0, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")

g2a <- sp %>%
  filter(!str_detect(gwf.casename, "NA"), 
         gwf.duration < 51) %>%
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ggplot(aes(años_previos_transicion, pwt7.fiveyma, group = años_previos_transicion)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "grey70", linetype = 2) +
  scale_x_reverse(breaks = seq(50, 0, -5), labels = seq(-50, 0, 5)) +
  labs(title = "Promedio móvil de cinco años (44 regímenes)", 
       x = "Número de años antes de la transición",
       y = "Cambio porcentual") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 20, hjust = 0, face = "bold", margin = margin(15, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")


g3a <- sp %>%
  filter(!str_detect(gwf.casename, "NA"), 
         gwf.duration < 51) %>%
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ggplot(aes(años_previos_transicion, pwt7.devsevenyma, group = años_previos_transicion)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, col = "grey70", linetype = 2) +
  scale_x_reverse(breaks = seq(50, 0, -5), labels = seq(-50, 0, 5)) +
  labs(title = "Desviación del promedio móvil de siete años (43 regímenes)", 
       x = "Número de años antes de la transición",
       y = "Cambio porcentual") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 20, hjust = 0, face = "bold", margin = margin(15, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")

plot_grid(g1a, g2a, g3a, nrow = 3, ncol = 1, align = 'h')

# Análisis asociado a la figura 2.4 ----

## Cálculo del número de regímenes incluidos en cada úno de las gráficas de la figura 2.3 

# Gráfica superior, utilizando pwt7.rgdpch.ch
sp %>%
  filter(gwf.fail == 1, !is.na(pwt7.rgdpch.ch))  %>% 
  distinct(gwf.casename) %>% 
  print(n = nrow(.))

# Gráfica intermedia, utilizando pwt7.fiveyma
sp %>%
  filter(gwf.fail == 1, !is.na(pwt7.fiveyma))  %>% 
  distinct(gwf.casename) %>% 
  print(n = nrow(.))

# Gráfica inferior, utilizando pwt7.devsevenyma
sp %>%
  filter(gwf.fail == 1, !is.na(pwt7.devsevenyma))  %>% 
  distinct(gwf.casename) %>% 
  print(n = nrow(.))

## Cálculos relacionados con Crecimiento de corto plazo 
# Mediana de todos los regímenes que cayeron
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  summarise(mediana = median(pwt7.rgdpch.ch)) 

# Mediana de todos los regímenes que cayeron, distinguiendo si eran apoyados o no por potencia extranjera
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(fimposed) %>% 
  summarise(mediana = median(pwt7.rgdpch.ch)) %>% 
  ungroup()

# Mediana de todos los regimenes, por año previo a la transición 

# Datos ordenados por el valor de la mediana
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  group_by(años_previos_transicion) %>% 
  summarise(mediana = median(pwt7.rgdpch.ch)) %>% 
  ungroup() %>% 
  arrange(mediana) %>% 
  print(n = nrow(.))

# Datos sin ordenar
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  group_by(años_previos_transicion) %>% 
  summarise(mediana = median(pwt7.rgdpch.ch)) %>% 
  ungroup() %>% 
  print(n = nrow(.))


# Valor en el año -1
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(años_previos_transicion == 1) %>% 
  select(gwf.casename, pwt7.rgdpch.ch) %>% 
  arrange(-pwt7.rgdpch.ch) %>% 
  mutate(positivo = ifelse(pwt7.rgdpch.ch > 0, 1, 0),
         suma_acumu_positivo = cumsum(positivo),
         por_acumu = round((suma_acumu_positivo/length(positivo))*100, 1)) %>% 
  print(n = nrow(.))


## Valor en el año 0
# Mediana de todos los regímenes 
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(años_previos_transicion == 0) %>% 
  summarise(mediana = median(pwt7.rgdpch.ch)) 

# Mediana de todos los regímenes, distinguiendo si son regímenes impuestos desde el extranjero o no
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(años_previos_transicion == 0) %>% 
  group_by(fimposed) %>% 
  summarise(mediana = median(pwt7.rgdpch.ch)) %>% 
  ungroup() 

# Cálculo del valor en el año 0
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.rgdpch.ch))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(años_previos_transicion == 0) %>% 
  select(gwf.casename, pwt7.rgdpch.ch) %>% 
  arrange(-pwt7.rgdpch.ch) %>% 
  mutate(positivo = ifelse(pwt7.rgdpch.ch > 0, 1, 0),
         suma_acumu_positivo = cumsum(positivo),
         por_acumu = round((suma_acumu_positivo/length(positivo))*100, 1)) %>% 
  print(n = nrow(.))


## Cálculos relacionados con Promedio de cinco años

# Mediana de todos los regímenes que cayeron
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.fiveyma))  %>% 
  summarise(mediana = median(pwt7.fiveyma)) 

# Mediana de todos los regímenes que cayeron, distinguiendo si eran apoyados o no por potencia extranjera
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.fiveyma))  %>% 
  group_by(fimposed) %>% 
  summarise(mediana = median(pwt7.fiveyma)) %>% 
  ungroup()

# Mediana de todos los regimenes, por año previo a la transición 
# Datos ordenados por el valor de la mediana
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.fiveyma))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  group_by(años_previos_transicion) %>% 
  summarise(mediana = median(pwt7.fiveyma)) %>% 
  ungroup() %>% 
  arrange(mediana) %>% 
  print(n = nrow(.))

# Datos sin ordenar
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.fiveyma))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  group_by(años_previos_transicion) %>% 
  summarise(mediana = median(pwt7.fiveyma)) %>% 
  ungroup() %>% 
  print(n = nrow(.))


## Valor en el año -1
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.fiveyma))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(años_previos_transicion == 1) %>% 
  select(gwf.casename, pwt7.fiveyma) %>% 
  arrange(-pwt7.fiveyma) %>% 
  mutate(positivo = ifelse(pwt7.fiveyma > 0, 1, 0),
         suma_acumu_positivo = cumsum(positivo),
         por_acumu = round((suma_acumu_positivo/44)*100, 1)) %>% 
  print(n = nrow(.))

## Cálculos relacionados con Desviación del promedio móvil de siete años 

# Mediana de todos los regímenes que cayeron
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.devsevenyma))  %>% 
  summarise(mediana = median(pwt7.devsevenyma)) 

# Mediana de todos los regímenes que cayeron, distinguiendo si eran apoyados o no por potencia extranjera
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.devsevenyma))  %>% 
  group_by(fimposed) %>% 
  summarise(mediana = median(pwt7.devsevenyma)) %>% 
  ungroup()

# Mediana de todos los regimenes, por año previo a la transición 
# Datos ordenados por el valor de la mediana
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.devsevenyma))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  group_by(años_previos_transicion) %>% 
  summarise(mediana = median(pwt7.devsevenyma)) %>% 
  ungroup() %>% 
  arrange(mediana) %>% 
  print(n = nrow(.))

# Datos sin ordenar
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.devsevenyma))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  group_by(años_previos_transicion) %>% 
  summarise(mediana = median(pwt7.devsevenyma)) %>% 
  ungroup() %>% 
  print(n = nrow(.))


# Valor en el año -1
sp %>%
  filter(!str_detect(gwf.casename, "NA"), !is.na(pwt7.devsevenyma))  %>% 
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(años_previos_transicion == 1) %>% 
  select(gwf.casename, pwt7.devsevenyma) %>% 
  arrange(-pwt7.devsevenyma) %>% 
  mutate(positivo = ifelse(pwt7.devsevenyma > 0, 1, 0),
         suma_acumu_positivo = cumsum(positivo),
         por_acumu = round((suma_acumu_positivo/43)*100, 1)) %>% 
  print(n = nrow(.))


### Figura 2.4  ----
# Boxplots del Panel (a) - Cambio en PIB per capita
g1 <- sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  mutate(tipo = ifelse(años_previos_transicion == 1 & !str_detect(gwf.casename, "NA"), "Concluidos, año\nprevio a la transición", ifelse(años_previos_transicion > 1 & !str_detect(gwf.casename, "NA"), "Concluidos, todos los\naños previos al año -1", ifelse(str_detect(gwf.casename, "NA"), "Continuaban en 2009,\ntodos los años", "Otros")))) %>% 
  filter(tipo != "Otros") %>% 
  ggplot() +
  geom_boxplot(aes(tipo, pwt7.rgdpch.ch, group = tipo)) +
  labs(title = "Panel (a) - Cambio en PIB per capita", 
       x = "",
      y = "Cambio porcentual") +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  geom_hline(yintercept = 0, color = "grey50", linetype = 3) +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(0, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")


# Boxplots del Panel (b) - Promedio móvil de cinco años
g2 <- sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  mutate(tipo = ifelse(años_previos_transicion == 1 & !str_detect(gwf.casename, "NA"), "Concluidos, año\nprevio a la transición", ifelse(años_previos_transicion > 1 & !str_detect(gwf.casename, "NA"), "Concluidos, todos los\naños previos al año -1", ifelse(str_detect(gwf.casename, "NA"), "Continuaban en 2009,\ntodos los años", "Otros")))) %>% 
  filter(tipo != "Otros") %>% 
  ggplot() +
  geom_boxplot(aes(tipo, pwt7.fiveyma, group = tipo)) +
  labs(title = "Panel (b) - Promedio móvil de cinco años", 
       x = "",
       y = "Cambio porcentual") +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  geom_hline(yintercept = 0, color = "grey50", linetype = 3) +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(0, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")


# Boxplots del Panel (c) - Desviación del promedio móvil de siete años
g3 <- sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  mutate(tipo = ifelse(años_previos_transicion == 1 & !str_detect(gwf.casename, "NA"), "Concluidos, año\nprevio a la transición", ifelse(años_previos_transicion > 1 & !str_detect(gwf.casename, "NA"), "Concluidos, todos los\naños previos al año -1", ifelse(str_detect(gwf.casename, "NA"), "Continuaban en 2009,\ntodos los años", "Otros")))) %>% 
  filter(tipo != "Otros") %>% 
  ggplot() +
  geom_boxplot(aes(tipo, pwt7.devsevenyma, group = tipo)) +
  labs(title = "Panel (c) - Desviación del promedio móvil de siete años", 
       x = "",
       y = "Cambio porcentual") +
  scale_y_continuous(limits = c(-15, 15), breaks = seq(-15, 15, 5)) +
  geom_hline(yintercept = 0, color = "grey50", linetype = 3) +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(0, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 13, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 13, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")


# Unir gráficas
plot_grid(g1, g2, g3, nrow = 3, ncol = 1, align = 'h')
ggsave("fig_2.4.png", path = "04_graficas", width = 10, height = 12, dpi = 300)
ggsave("fig_2.4.jpeg", path = "04_graficas", width = 10, height = 12, dpi = 300)

# Análisis asociado a la figura 2.4 -----

# Datos de duración de China, Malasia, Singapur y Vietnam 
sp %>%
  filter(gwf.casename %in% c("China 49-NA", "Malaysia 57-NA", "Singapore 65-NA", "Vietnam 54-NA")) %>% 
  group_by(gwf.casename) %>% 
  summarise(duracion = last(gwf.duration)) %>% 
  ungroup()


# Medianas del Panel (a) - Cambio en PIB per capita
sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  mutate(tipo = ifelse(años_previos_transicion == 1 & !str_detect(gwf.casename, "NA"), "Concluidos, año\nprevio a la transición", ifelse(años_previos_transicion > 1 & !str_detect(gwf.casename, "NA"), "Concluidos, todos los\naños previos al año -1", ifelse(str_detect(gwf.casename, "NA"), "Continuaban en 2009,\ntodos los años", "Otros")))) %>% 
  filter(tipo != "Otros") %>% 
  group_by(tipo) %>% 
  summarise(mediana = median(pwt7.rgdpch.ch, na.rm = T)) %>% 
  ungroup()

# Medianas del Panel (b) - Promedio móvil de cinco años
sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  mutate(tipo = ifelse(años_previos_transicion == 1 & !str_detect(gwf.casename, "NA"), "Concluidos, año\nprevio a la transición", ifelse(años_previos_transicion > 1 & !str_detect(gwf.casename, "NA"), "Concluidos, todos los\naños previos al año -1", ifelse(str_detect(gwf.casename, "NA"), "Continuaban en 2009,\ntodos los años", "Otros")))) %>% 
  filter(tipo != "Otros") %>% 
  group_by(tipo) %>% 
  summarise(mediana = median(pwt7.fiveyma, na.rm = T)) %>% 
  ungroup()


# Medianas del Panel (c) - Desviación del promedio móvil de siete años
sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  mutate(tipo = ifelse(años_previos_transicion == 1 & !str_detect(gwf.casename, "NA"), "Concluidos, año\nprevio a la transición", ifelse(años_previos_transicion > 1 & !str_detect(gwf.casename, "NA"), "Concluidos, todos los\naños previos al año -1", ifelse(str_detect(gwf.casename, "NA"), "Continuaban en 2009,\ntodos los años", "Otros")))) %>% 
  filter(tipo != "Otros") %>% 
  group_by(tipo) %>% 
  summarise(mediana = median(pwt7.devsevenyma, na.rm = T)) %>% 
  ungroup()

# Datos de México
sp %>%
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(gwf.casename == "Mexico 29-00") %>% 
  select(year, gwf.casename, años_previos_transicion, pwt7.rgdpch.ch, pwt7.fiveyma, pwt7.devsevenyma) %>% 
  print(n = nrow(.))


# Datos de Angola, Cuba, Siria, Senegal, Sri Lanka, Taiwán 
sp %>%
  # count(gwf.casename) %>% print(n = nrow(.))
  group_by(gwf.casename) %>% 
  mutate(años_previos_transicion = gwf.spell - gwf.duration) %>% 
  ungroup() %>% 
  filter(gwf.casename %in% c("Angola 75-NA", "Cuba 59-NA", "Iraq 68-79", "Senegal 60-00", "Syria 63-NA", "Sri Lanka 78-94", "Taiwan 49-00")) %>% 
  select(year, gwf.casename, años_previos_transicion, pwt7.fiveyma) %>% 
  print(n = nrow(.))


### Modelos ----

# Todos los modelos presentados en el capítulo 2 fueron calculados en STATA, usando (i) la base de datos "sp.dta",  generada en R con el script "cap_02_cargar_unir_datos.R"; y (ii) el código que está disponible en los do files "cap_02_modelos_logit.do" y "cap_02_modelos_survival.do", incluidos en el folder "02_codigo" de este proyecto.

# En esta sección únicamente calculo el número de regímenes en cada modelo

# Número de regímenes en los modelos en cuadros 2.1 y 2.2
sp %>% filter(!is.na(pwt7.rgdpch.chL1)) %>% distinct(gwf.casename) # Modelo 1
sp %>% filter(!is.na(pwt7.fiveyma)) %>% distinct(gwf.casename) # Modelo 2
sp %>% filter(!is.na(pwt7.devsevenyma)) %>% distinct(gwf.casename) # Modelo 3
sp %>% filter(!is.na(pwt7.rgdpch.chL1), # Modelos 4 y 7
              !is.na(ross.precioL1.log)) %>% 
  distinct(gwf.casename) 
sp %>% filter(!is.na(pwt7.fiveyma),     # Modelos 5 y 8
              !is.na(ross.precioL1.log)) %>% 
  distinct(gwf.casename)
sp %>% filter(!is.na(pwt7.devsevenyma), # Modelos 6 y 9
              !is.na(ross.precioL1.log)) %>% 
  distinct(gwf.casename)


# Número de regímenes en los modelos en los Apéndices III y IV 
sp %>% filter(!is.na(mad.chgdppcL1)) %>% distinct(gwf.casename)   # Modelo 1
sp %>% filter(!is.na(mad.fiveyma)) %>%  distinct(gwf.casename)    # Modelo 2
sp %>% filter(!is.na(mad.devsevenyma)) %>% distinct(gwf.casename) # Modelo 3
sp %>% filter(!is.na(mad.chgdppcL1),         # Modelos 4 y 7
              !is.na(ross.precioL1.log)) %>% 
  distinct(gwf.casename)
sp %>% filter(!is.na(mad.fiveyma),           # Modelos 5 y 8 
              !is.na(ross.precioL1.log)) %>% 
  distinct(gwf.casename)
sp %>% filter(!is.na(mad.devsevenyma),       # Modelos 6 y 9 
              !is.na(ross.precioL1.log)) %>% 
  distinct(gwf.casename)


### Figura 2.6 ----

# Esta gráfica es construida con datos previamente calculados en STATA usando el archivo
# cap_02_modelos_logit.do, disponible en el folder 02_codigo de este proyecto.

# Cargar datos
datos <- read_csv("01_datos/cap_02/datos_grafica_2.6.csv")

# Hacer la gráfica
datos %>% 
  ggplot() +
  geom_line(aes(pwt7.fiveyma, pred_y), size = 1) +
  geom_line(aes(pwt7.fiveyma, lower), size = 1, col = "grey80", linetype = 2) +
  geom_line(aes(pwt7.fiveyma, upper), size = 1, col = "grey80", linetype = 2) +
  geom_segment(aes(x = 1.5, y = 0.24, xend = 2.7, yend = 0.24), size = 1) +
  geom_segment(aes(x = 1.5, y = 0.225, xend = 2.7, yend = 0.225), size = 1, col = "grey80", linetype = 2) +
  annotate("text", label = "Probabilidad predicha", x = 3, y = 0.24, size = 5, hjust = 0) +
  annotate("text", label = "Intervalo de confianza de 95%", x = 3, y = 0.225, size = 5, hjust = 0) +
  scale_x_continuous(breaks = seq(-9, 9, 1)) +
  scale_y_continuous(breaks = seq(0, 0.25, 0.025)) +
  labs(x = "Cambio porcentual",
       y = "Probabilidad de caída") +
  theme_classic() +
  theme(plot.caption = element_text(hjust = 0), 
        plot.title = element_text(size = 18, hjust = 0, face = "bold", margin = margin(0, 0, 15, 0), color = "#666666"),
        axis.text = element_text(size = 14, colour = "#666666", face = "bold"),
        axis.title.x = element_text(size = 15, hjust = 1, face = "bold", margin = margin(15, 0, 0, 0)), 
        axis.title.y = element_text(size = 15, hjust = 1, face = "bold", margin = margin(0, 15, 0, 0)),
        axis.line = element_line(size = 0.5, colour = "#666666"),
        legend.position = "none")

ggsave("fig_2.6.png", path = "04_graficas", width = 10, height = 6.67, dpi = 300)
ggsave("fig_2.6.jpeg", path = "04_graficas", width = 10, height = 6.67, dpi = 300)


### Análisis de cifras relacionadas con la figura 2.6 ----

# Número de observaciones país año en el que el Promedio móvil de cinco años es menor a -3%
sp %>% 
  group_by(gwf.casename) %>% 
  summarise(num = sum(pwt7.fiveyma <= -3)) %>% 
  ungroup() %>% 
  filter(!is.na(num)) %>% 
  mutate(total_yr = sum(num), 
         por_yr = (total_yr/2184)*100) %>% 
  print(n = nrow(.))



