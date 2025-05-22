# Cargar librerías necesarias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(moments)

# Cargar los datos
df3 <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-3-cambios.csv") %>%
  mutate(reglamento = "3 cambios")
df5 <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-5-cambios.csv") %>%
  mutate(reglamento = "5 cambios")

# Unir ambos dataframes
df_completo <- bind_rows(df3, df5)

# Seleccionar proporciones de cambios por equipo
df_cambios <- df_completo %>%
  dplyr::select(
    reglamento,
    Local = `proporcion local cambios en general`,
    Visitante = `proporcion visitante cambios en general`
  ) %>%
  pivot_longer(cols = c(Local, Visitante), names_to = "Equipo", values_to = "Proporcion")

# 📈 Estadísticas descriptivas detalladas por reglamento y equipo
cat("\n📊 Estadísticas descriptivas de proporciones de cambios por equipo y reglamento:\n")

df_cambios %>%
  group_by(reglamento, Equipo) %>%
  summarise(
    Media = mean(Proporcion),
    Mediana = median(Proporcion),
    Q1 = quantile(Proporcion, 0.25),
    Q3 = quantile(Proporcion, 0.75),
    SD = sd(Proporcion),
    Asimetría = skewness(Proporcion),
    Curtosis = kurtosis(Proporcion),
    .groups = "drop"
  ) %>%
  print()

# 📊 Boxplot comparativo por equipo y reglamento
ggplot(df_cambios, aes(x = reglamento, y = Proporcion, fill = Equipo)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Local" = "steelblue", "Visitante" = "tomato")) +
  labs(title = "Comparación de proporciones de cambios por equipo y reglamento",
       x = "Reglamento", y = "Proporción de cambios") +
  theme_minimal()


# =======================================
# 📊 2. Cambios en alineación (defensas, medios, delanteros, sitio y general)
# =======================================


# Calcular totales de cambios por alineación para local y visitante
df_completo <- df_completo %>%
  mutate(
    cambios_local_total = rowSums(across(c(
      "proporcion local cambios alineacion defensa en general",
      "proporcion local cambios alineacion centrocampista en general",
      "proporcion local cambios alineacion delantero en general"
    ))),
    cambios_visitante_total = rowSums(across(c(
      "proporcion visitante cambios alineacion defensa en general",
      "proporcion visitante cambios alineacion centrocampista en general",
      "proporcion visitante cambios alineacion delantero en general"
    ))),
  )

# Preparar dataframe largo
df_alineacion <- df_completo %>%
  dplyr::select(reglamento,
         `Local` = cambios_local_total,
         `Visitante` = cambios_visitante_total) %>%
  pivot_longer(-reglamento, names_to = "Tipo", values_to = "Proporcion")

# Estadísticas descriptivas por grupo
df_alineacion %>%
  group_by(reglamento, Tipo) %>%
  summarise(
    Media = mean(Proporcion),
    Mediana = median(Proporcion),
    Q1 = quantile(Proporcion, 0.25),
    Q3 = quantile(Proporcion, 0.75),
    SD = sd(Proporcion),
    Asimetría = skewness(Proporcion),
    Curtosis = kurtosis(Proporcion),
    .groups = "drop"
  ) %>%
  print()

# Boxplot comparativo
ggplot(df_alineacion, aes(x = reglamento, y = Proporcion, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Local" = "steelblue", "Visitante" = "tomato")) +
  labs(title = "Comparación de cambios en alineación por reglamento y tipo",
       x = "Reglamento", y = "Proporción de cambios") +
  theme_minimal()


# =======================================
# 📊 3. Minutos promedio de cambios
# =======================================
df_completo <- bind_rows(df3, df5)

# Seleccionar y pivotar datos de minutos
df_minutos <- df_completo %>%
  dplyr::select(
    reglamento,
    `Local` = `media local cambios minutos en general`,
    `Visitante` = `media visitante cambios minutos en general`
  ) %>%
  pivot_longer(-reglamento, names_to = "Tipo", values_to = "Minuto")

# 📋 Estadísticas descriptivas detalladas
cat("\n📊 Estadísticas del minuto promedio de cambios:\n")

df_minutos %>%
  group_by(reglamento, Tipo) %>%
  summarise(
    Media = mean(Minuto),
    Mediana = median(Minuto),
    Q1 = quantile(Minuto, 0.25),
    Q3 = quantile(Minuto, 0.75),
    SD = sd(Minuto),
    Asimetría = skewness(Minuto),
    Curtosis = kurtosis(Minuto),
    .groups = "drop"
  ) %>%
  print()

# 📊 Boxplot comparativo
ggplot(df_minutos, aes(x = reglamento, y = Minuto, fill = Tipo)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  scale_fill_manual(values = c("Local" = "steelblue", "Visitante" = "tomato")) +
  labs(title = "Comparación del minuto promedio de cambios",
       x = "Reglamento", y = "Minuto") +
  theme_minimal()

