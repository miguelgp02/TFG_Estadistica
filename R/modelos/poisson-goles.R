# ======================================
# 📦 1. Cargar librerías
# ======================================
library(readr)
library(dplyr)
library(ggplot2)
library(caret)

# ======================================
# 📁 2. Cargar y preparar datos
# ======================================
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-TODOS.csv")

# Eliminar columnas no predictivas e identificadores
vars_a_quitar <- c("resultado_partido", "jornada", "id_indicadores_equipo_prepartido", "id_partido", "temporada")
df_modelo <- df[, !(names(df) %in% vars_a_quitar)] %>% na.omit()

# ======================================
# 🧪 3. Separar en train y test
# ======================================
set.seed(123)

# Separar para modelo de goles local
part_local <- createDataPartition(df_modelo$resultado_local, p = 0.8, list = FALSE)
train_data_local <- df_modelo[part_local, ]
test_data_local  <- df_modelo[-part_local, ]

# Separar para modelo de goles visitante
part_visitante <- createDataPartition(df_modelo$resultado_visitante, p = 0.8, list = FALSE)
train_data_visitante <- df_modelo[part_visitante, ]
test_data_visitante  <- df_modelo[-part_visitante, ]

# ======================================
# 🤖 4. Ajustar modelo Poisson para goles LOCAL
# ======================================
modelo_local <- glm(resultado_local ~ ., data = train_data_local, family = poisson)

pred_local <- predict(modelo_local, newdata = test_data_local, type = "response")

mae_local <- mean(abs(pred_local - test_data_local$resultado_local))
rmse_local <- sqrt(mean((pred_local - test_data_local$resultado_local)^2))
cat("⚽ Goles Local - MAE:", round(mae_local, 4), " | RMSE:", round(rmse_local, 4), "\n")

# ======================================
# 🤖 5. Ajustar modelo Poisson para goles VISITANTE
# ======================================
modelo_visit <- glm(resultado_visitante ~ ., data = train_data_visitante, family = poisson)

pred_visit <- predict(modelo_visit, newdata = test_data_visitante, type = "response")

mae_visit <- mean(abs(pred_visit - test_data_visitante$resultado_visitante))
rmse_visit <- sqrt(mean((pred_visit - test_data_visitante$resultado_visitante)^2))
cat("⚽ Goles Visitante - MAE:", round(mae_visit, 4), " | RMSE:", round(rmse_visit, 4), "\n")

# ======================================
# 🏁 6. Inferir resultado del partido (con redondeo a 1 decimal)
# ======================================
goles_local_red <- round(pred_local, 1)
goles_visit_red <- round(pred_visit, 1)

resultado_predicho <- case_when(
  abs(goles_local_red - goles_visit_red) < 0.05 ~ "X",
  goles_local_red > goles_visit_red ~ "1",
  TRUE ~ "2"
)

# Resultado real
n_filas <- min(nrow(test_data_local), nrow(test_data_visitante))
resultado_real <- case_when(
  test_data_local$resultado_local[1:n_filas] > test_data_visitante$resultado_visitante[1:n_filas] ~ "1",
  test_data_local$resultado_local[1:n_filas] < test_data_visitante$resultado_visitante[1:n_filas] ~ "2",
  TRUE ~ "X"
)

# ======================================
# 📊 Matriz de confusión y Accuracy
# ======================================
conf_mat <- table(Predicho = resultado_predicho[1:n_filas], Real = resultado_real)

cat("\n📊 Matriz de confusión (resultado partido con redondeo a 1 decimal):\n")
print(conf_mat)

accuracy_resultado <- mean(resultado_predicho[1:n_filas] == resultado_real)
cat("\n🎯 Accuracy resultado partido (derivado de Poisson):", round(accuracy_resultado, 4), "\n")
