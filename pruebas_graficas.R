library(dplyr)
library(ggplot2)
library(GGally)

# Carga de datos
Partidos <- read.csv("wcmatches.csv")

# Ajustes de variables para q las graficas se vean mas wonitas
# Este pa convertir la fecha a un formato de fecha que pueda leer correctamente R
Partidos$date <- as.Date(Partidos$date, format = "%Y/%m/%d")

# Este para ordenar los días de la semana cronológicamente y no alfabéticamente (cómo están ahorita)
dias_ordenados <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
Partidos$dayofweek <- factor(Partidos$dayofweek, levels = dias_ordenados)

# Una variable d los goles totales por partido
Partidos$goles_totales <- Partidos$home_score + Partidos$away_score

# ALGUNAS GRÁFICAS GENERALES
# Gráfica A: Histograma de goles totales
grafica_goles <- ggplot(Partidos, aes(x = goles_totales)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "white", alpha = 0.9) +
  labs(
    title = "Distribución de Goles Totales por Partido (1930 - 2018)",
    x = "Cantidad de Goles en un Partido",
    y = "Número de Partidos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.minor = element_blank()
  )

grafica_goles

# Grafica de cómo ha cambiado el número de partidos por año
grafica_partidos <- Partidos %>%
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line(color = "turquoise", linewidth = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Número de Partidos Jugados por edición",
    x = "Año del Mundial",
    y = "Cantidad de Partidos"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid.minor = element_blank()
  )
grafica_partidos

# Distribución de datos, matriz de correlación

# Filtracion de los datos y renombramiento pa q salgan en español al imprimir
datos_numericos <- Partidos[, c("year", "home_score", "away_score")]
colnames(datos_numericos) <- c("Año del Mundial", "Goles Local", "Goles Visitante")

# impresión de la matriz de correlación
matriz_correlacion <- ggpairs(
  datos_numericos,
  # Escribirmos lo mismo arriba y abajo de la matriz para ver el comportamiento en el cruce de variables
  upper = list(continuous = wrap("smooth", alpha = 0.3, color = "#2c3e50")), 
  lower = list(continuous = wrap("smooth", alpha = 0.3, color = "#2c3e50")), 
  # curvas de densidad en la diagonal
  diag = list(continuous = wrap("densityDiag", fill = "#16a085", alpha = 0.6)),
  
  title = "Distribución de datos y relación entre año y goles en las ediciones de los mundiales (1930-2018)"
) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    strip.background = element_rect(fill = "#ecf0f1", color = "white"),
    strip.text = element_text(face = "bold", size = 10)
  )
matriz_correlacion

# TABLA DE CORRELACIÓN DE PEARSON
# usamos la funcion y calculamos la matriz matemática y redondeamos a 3 decimales
matriz_pearson <- round(cor(datos_numericos), 3)
# Imprimimos la tabla en la consola
print("Coeficientes de Correlación de Pearson:")
matriz_pearson
