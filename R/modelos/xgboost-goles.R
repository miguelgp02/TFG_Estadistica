# ======================================
# 📦 1. Cargar librerías
# ======================================
library(readr)
library(dplyr)
library(caret)
library(xgboost)
library(Matrix)

# ======================================
# 📁 2. Cargar y preparar datos
# ======================================
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-5-cambios.csv")
vars_a_quitar <- c("resultado_partido", "jornada", "id_indicadores_equipo_prepartido", "id_partido", "temporada")
df_modelo <- df[, !(names(df) %in% vars_a_quitar)] %>% na.omit()

# ======================================
# ⚽ 3. Goles LOCAL - Grid Search, evaluación y predicción
# ======================================
set.seed(123)
part_local <- createDataPartition(df_modelo$resultado_local, p = 0.8, list = FALSE)
train_local <- df_modelo[part_local, ]
test_local  <- df_modelo[-part_local, ]

X_train_local <- model.matrix(resultado_local ~ . - resultado_visitante, data = train_local)[, -1]
y_train_local <- train_local$resultado_local
X_test_local  <- model.matrix(resultado_local ~ . - resultado_visitante, data = test_local)[, -1]
y_test_local  <- test_local$resultado_local

dtrain_local <- xgb.DMatrix(data = X_train_local, label = y_train_local)
dtest_local  <- xgb.DMatrix(data = X_test_local, label = y_test_local)

# Grid search
etas <- c(0.01, 0.05, 0.1)
depths <- c(3, 5, 7)
subs <- c(0.8, 1)
cols <- c(0.8, 1)

mejor_rmse_local <- Inf
modelo_mejor_local <- NULL

for (md in depths) {
  for (eta in etas) {
    for (sub in subs) {
      for (col in cols) {
        params <- list(
          objective = "reg:squarederror",
          eval_metric = "rmse",
          max_depth = md,
          eta = eta,
          subsample = sub,
          colsample_bytree = col
        )
        
        set.seed(123)
        modelo <- xgb.train(params = params, data = dtrain_local, nrounds = 100, verbose = 0)
        pred <- predict(modelo, newdata = dtest_local)
        rmse <- sqrt(mean((pred - y_test_local)^2))
        
        if (rmse < mejor_rmse_local) {
          mejor_rmse_local <- rmse
          modelo_mejor_local <- modelo
          mejor_params_local <- params
        }
      }
    }
  }
}

# Evaluación final goles local
pred_final_local <- predict(modelo_mejor_local, newdata = dtest_local)
rmse_final_local <- sqrt(mean((pred_final_local - y_test_local)^2))
mae_final_local <- mean(abs(pred_final_local - y_test_local))
cat(sprintf("🔧 Mejor modelo → max_depth = %d | eta = %.2f | subsample = %.2f | colsample_bytree = %.2f\n",
            mejor_params_local$max_depth, mejor_params_local$eta,
            mejor_params_local$subsample, mejor_params_local$colsample_bytree))
cat("\n📈 Evaluación del mejor modelo XGBoost (goles locales):\n")
cat("🎯 MAE:", round(mae_final_local, 4), " | RMSE:", round(rmse_final_local, 4), "\n")

# ======================================
# ⚽ 4. Goles VISITANTE - Grid Search, evaluación y predicción
# ======================================
set.seed(123)
part_visit <- createDataPartition(df_modelo$resultado_visitante, p = 0.8, list = FALSE)
train_visit <- df_modelo[part_visit, ]
test_visit  <- df_modelo[-part_visit, ]

X_train_visit <- model.matrix(resultado_visitante ~ . - resultado_local, data = train_visit)[, -1]
y_train_visit <- train_visit$resultado_visitante
X_test_visit  <- model.matrix(resultado_visitante ~ . - resultado_local, data = test_visit)[, -1]
y_test_visit  <- test_visit$resultado_visitante

dtrain_visit <- xgb.DMatrix(data = X_train_visit, label = y_train_visit)
dtest_visit  <- xgb.DMatrix(data = X_test_visit, label = y_test_visit)

mejor_rmse_visit <- Inf
modelo_mejor_visit <- NULL

for (md in depths) {
  for (eta in etas) {
    for (sub in subs) {
      for (col in cols) {
        params <- list(
          objective = "reg:squarederror",
          eval_metric = "rmse",
          max_depth = md,
          eta = eta,
          subsample = sub,
          colsample_bytree = col
        )
        
        set.seed(123)
        modelo <- xgb.train(params = params, data = dtrain_visit, nrounds = 100, verbose = 0)
        pred <- predict(modelo, newdata = dtest_visit)
        rmse <- sqrt(mean((pred - y_test_visit)^2))
        
        if (rmse < mejor_rmse_visit) {
          mejor_rmse_visit <- rmse
          modelo_mejor_visit <- modelo
          mejor_params_visit <- params
        }
      }
    }
  }
}

# Evaluación final goles visitante
pred_final_visit <- predict(modelo_mejor_visit, newdata = dtest_visit)
rmse_final_visit <- sqrt(mean((pred_final_visit - y_test_visit)^2))
mae_final_visit <- mean(abs(pred_final_visit - y_test_visit))
cat(sprintf("🔧 Mejor modelo → max_depth = %d | eta = %.2f | subsample = %.2f | colsample_bytree = %.2f\n",
            mejor_params_visit$max_depth, mejor_params_visit$eta,
            mejor_params_visit$subsample, mejor_params_visit$colsample_bytree))
cat("\n📈 Evaluación del mejor modelo XGBoost (goles visitantes):\n")
cat("🎯 MAE:", round(mae_final_visit, 4), " | RMSE:", round(rmse_final_visit, 4), "\n")

# ======================================
# 🏁 5. Inferencia de resultado del partido a partir de predicciones
# ======================================
goles_local_red <- round(pred_final_local, 1)
goles_visit_red <- round(pred_final_visit, 1)

resultado_predicho_xgb <- case_when(
  abs(goles_local_red - goles_visit_red) < 0.05 ~ "X",
  goles_local_red > goles_visit_red ~ "1",
  TRUE ~ "2"
)

n_filas <- min(length(goles_local_red), length(goles_visit_red))
resultado_real <- case_when(
  test_local$resultado_local[1:n_filas] > test_visit$resultado_visitante[1:n_filas] ~ "1",
  test_local$resultado_local[1:n_filas] < test_visit$resultado_visitante[1:n_filas] ~ "2",
  TRUE ~ "X"
)

# ======================================
# 📊 6. Matriz de confusión y Accuracy
# ======================================
conf_mat_xgb <- table(Predicho = resultado_predicho_xgb[1:n_filas], Real = resultado_real)

cat("\n📊 Matriz de confusión (resultado derivado de XGBoost):\n")
print(conf_mat_xgb)

accuracy_xgb <- mean(resultado_predicho_xgb[1:n_filas] == resultado_real)
cat("\n🎯 Accuracy resultado partido (XGBoost):", round(accuracy_xgb, 4), "\n")
