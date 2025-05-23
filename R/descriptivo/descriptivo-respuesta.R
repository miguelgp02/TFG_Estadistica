# Cargar librerías
library(readr)
library(dplyr)
library(ggplot2)
library(moments)
library(tidyr)


# Cargar los datos
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-TODOS.csv")

# 📊 Estadísticas descriptivas de goles

cat("📊 Estadísticas de goles locales y visitantes:\n")

local_stats <- df$resultado_local
visitante_stats <- df$resultado_visitante

cat("Local - ", "\n","Media:", mean(local_stats), "\n", 
    "Mediana:", median(local_stats), "\n",
    "Max:", max(local_stats), "\n",
    "Min:", min(local_stats), "\n",
    "Q1:", quantile(local_stats, 0.25), "\n",
    "Q3:", quantile(local_stats, 0.75), "\n",
    "SD:", sd(local_stats), "\n",
    "Asimetría:", skewness(local_stats), "\n",
    "Curtosis:", kurtosis(local_stats), "\n")

cat("Visitante - ", "\n","Media:", mean(visitante_stats), "\n", 
    "Mediana:", median(visitante_stats), "\n",
    "Max:", max(visitante_stats), "\n",
    "Min:", min(visitante_stats), "\n",
    "Q1:", quantile(visitante_stats, 0.25), "\n",
    "Q3:", quantile(visitante_stats, 0.75), "\n",
    "SD:", sd(visitante_stats), "\n",
    "Asimetría:", skewness(visitante_stats), "\n",
    "Curtosis:", kurtosis(visitante_stats), "\n")

cat("Correlación goles locales y visitantes:\n")
cor(df$resultado_local, df$resultado_visitante)


# Histograma goles equipo local
ggplot(df, aes(x = resultado_local)) +
  geom_bar(binwidth = 1, fill = "steelblue", color = "black") +
  scale_x_continuous(breaks = 0:10, limits = c(0, 7)) +
  labs(title = "Distribución de goles del equipo local",
       x = "Goles", y = "Frecuencia") +
  theme_minimal()

# Histograma goles equipo visitante
ggplot(df, aes(x = resultado_visitante)) +
  geom_bar(binwidth = 1, fill = "tomato", color = "black") +
  scale_x_continuous(breaks = 0:10, limits = c(0, 7)) +
  labs(title = "Distribución de goles del equipo visitante",
       x = "Goles", y = "Frecuencia") +
  theme_minimal()


df_long <- df %>%
  dplyr::select(resultado_local, resultado_visitante) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Tipo", values_to = "Goles")


# Densidad comparativa de goles
ggplot(df_long, aes(x = Goles, fill = Tipo)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("resultado_local" = "steelblue",
                               "resultado_visitante" = "tomato")) +
  labs(title = "Densidad de goles locales y visitantes",
       x = "Goles", y = "Densidad") +
  xlim(0, 7) +
  theme_minimal()

# Boxplot coherente con el estilo de los histogramas
ggplot(df_long, aes(x = Tipo, y = Goles, fill = Tipo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("resultado_local" = "steelblue",
                               "resultado_visitante" = "tomato")) +
  scale_x_discrete(labels = c("resultado_local" = "Local", "resultado_visitante" = "Visitante")) +
  labs(title = "Boxplot: comparación de goles local vs visitante",
       x = "Equipo", y = "Goles") +
  theme_minimal() +
  theme(legend.position = "none")


df %>%
  group_by(temporada) %>%
  summarise(media_local = mean(resultado_local),
            media_visitante = mean(resultado_visitante)) %>%
  ggplot(aes(x = temporada)) +
  geom_line(aes(y = media_local, color = "Local"), linewidth = 1) +
  geom_line(aes(y = media_visitante, color = "Visitante"), linewidth = 1) +
  scale_color_manual(values = c("Local" = "steelblue", "Visitante" = "tomato")) +
  labs(title = "Media de goles por temporada",
       x = "Temporada", y = "Media de goles", color = "Equipo") +
  theme_minimal()


ggplot(df, aes(x = jornada, y = resultado_local)) +
  geom_smooth(se = FALSE, color = "steelblue") +
  labs(title = "Tendencia de goles del equipo local por jornada",
       x = "Jornada", y = "Goles") +
  theme_minimal()


ggplot(df, aes(x = jornada, y = resultado_visitante)) +
  geom_smooth(se = FALSE, color = "tomato") +
  labs(title = "Tendencia de goles del equipo visitante por jornada",
       x = "Jornada", y = "Goles") +
  theme_minimal()







# 📋 Distribución de resultado_partido
conteo <- df %>%
  count(resultado_partido) %>%
  arrange(resultado_partido)

conteo <- conteo %>%
  mutate(Proporcion = n / sum(n))

cat("\n📋 Distribución de resultado_partido:\n")
print(conteo)

# Gráfico de barras para resultado_partido
ggplot(df, aes(x = factor(resultado_partido, levels = c("1", "X", "2")), fill = resultado_partido)) +
  geom_bar() +
  scale_fill_manual(values = c("1" = "steelblue", "X" = "orange", "2" = "firebrick")) +
  labs(title = "Distribución de resultado del partido",
       x = "Resultado del partido", y = "Número de partidos") +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta la leyenda si no la necesitas


df %>%
  count(resultado_local, resultado_visitante, sort = TRUE) %>%
  top_n(5)

df %>%
  count(resultado_local, resultado_visitante, sort = TRUE) %>%
  slice_min(n, n = 5)



library(reshape2)

tabla_goles <- df %>% count(resultado_local, resultado_visitante)
ggplot(tabla_goles, aes(x = resultado_local, y = resultado_visitante, fill = n)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Frecuencia de combinaciones de goles")








