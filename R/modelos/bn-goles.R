# ======================================
# 📦 1. Cargar librerías
# ======================================
library(readr)
library(dplyr)
library(caret)
library(MASS)  # glm.nb
library(ggplot2)

# ======================================
# 📁 2. Cargar y preparar datos
# ======================================
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-5-cambios.csv")

# Quitar columnas no predictivas ni necesarias
vars_a_quitar <- c("resultado_partido", "jornada", "id_indicadores_equipo_prepartido", "id_partido", "temporada")
df_modelo <- df[, !(names(df) %in% vars_a_quitar)] %>% na.omit()

# ======================================
# 🧪 3. Separar en train y test
# ======================================
set.seed(123)

part_local <- createDataPartition(df_modelo$resultado_local, p = 0.8, list = FALSE)
train_data_local <- df_modelo[part_local, ]
test_data_local  <- df_modelo[-part_local, ]

part_visitante <- createDataPartition(df_modelo$resultado_visitante, p = 0.8, list = FALSE)
train_data_visitante <- df_modelo[part_visitante, ]
test_data_visitante  <- df_modelo[-part_visitante, ]

# ======================================
# 🤖 4. Binomial Negativa para goles LOCAL
# ======================================
modelo_local_nb <- glm.nb(resultado_local ~ ., data = train_data_local)

pred_local_nb <- predict(modelo_local_nb, newdata = test_data_local, type = "response")

mae_local_nb <- mean(abs(pred_local_nb - test_data_local$resultado_local))
rmse_local_nb <- sqrt(mean((pred_local_nb - test_data_local$resultado_local)^2))
cat("⚽ Goles Local (NB) - MAE:", round(mae_local_nb, 4), " | RMSE:", round(rmse_local_nb, 4), "\n")

# ======================================
# 🤖 5. Binomial Negativa para goles VISITANTE
# ======================================
modelo_visit_nb <- glm.nb(resultado_visitante ~ ., data = train_data_visitante)

pred_visit_nb <- predict(modelo_visit_nb, newdata = test_data_visitante, type = "response")

mae_visit_nb <- mean(abs(pred_visit_nb - test_data_visitante$resultado_visitante))
rmse_visit_nb <- sqrt(mean((pred_visit_nb - test_data_visitante$resultado_visitante)^2))
cat("⚽ Goles Visitante (NB) - MAE:", round(mae_visit_nb, 4), " | RMSE:", round(rmse_visit_nb, 4), "\n")

# ======================================
# 🏁 6. Inferir resultado del partido (NB con redondeo a 1 decimal)
# ======================================
goles_local_red <- round(pred_local_nb, 1)
goles_visit_red <- round(pred_visit_nb, 1)

resultado_predicho_nb <- case_when(
  abs(goles_local_red - goles_visit_red) < 0.05 ~ "X",
  goles_local_red > goles_visit_red ~ "1",
  TRUE ~ "2"
)

n_filas <- min(nrow(test_data_local), nrow(test_data_visitante))
resultado_real <- case_when(
  test_data_local$resultado_local[1:n_filas] > test_data_visitante$resultado_visitante[1:n_filas] ~ "1",
  test_data_local$resultado_local[1:n_filas] < test_data_visitante$resultado_visitante[1:n_filas] ~ "2",
  TRUE ~ "X"
)

# ======================================
# 📊 Matriz de confusión y Accuracy
# ======================================
conf_mat_nb <- table(Predicho = resultado_predicho_nb[1:n_filas], Real = resultado_real)

cat("\n📊 Matriz de confusión (resultado partido derivado de NB):\n")
print(conf_mat_nb)

accuracy_nb <- mean(resultado_predicho_nb[1:n_filas] == resultado_real)
cat("\n🎯 Accuracy resultado partido (NB):", round(accuracy_nb, 4), "\n")
