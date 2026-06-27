library(dplyr)
library(ggplot2)

# Carga de datos
Partidos <- read.csv("wcmatches.csv")

# Ajustes de variables para q las graficas se vean mas wonitas
# Este pa convertir la fecha a un formato de fecha que pueda leer correctamente R
Partidos$date <- as.Date(Partidos$date, format = "%Y/%m/%d")

# Este para ordenar los días de la semana cronológicamente y no alfabéticamente (cómo están ahorita)
dias_ordenados <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
Partidos$dayofweek <- factor(Partidos$dayofweek, levels = dias_ordenados)

# Una variable d los goles totales por partido
Partidos$total_goals <- Partidos$home_score + Partidos$away_score

# ALGUNAS GRÁFICAS GENERALES
# Gráfica A: Histograma de goles totales
grafica_goles <- ggplot(Partidos, aes(x = total_goals)) +
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
