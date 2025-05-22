# ======================================
# 📦 1. Cargar librerías
# ======================================
library(readr)
library(dplyr)
library(Matrix)
library(caret)
library(xgboost)
library(tidyr)

# ======================================
# 📁 2. Cargar y preparar datos
# ======================================
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-5-cambios.csv") %>%
  mutate(resultado_partido = factor(resultado_partido, levels = c("1", "X", "2")))

# Eliminar columnas no relevantes
cols_quitar <- c("resultado_local", "resultado_visitante", "jornada",
                 "id_indicadores_equipo_prepartido", "id_partido", "temporada")
df_modelo <- df[, !(names(df) %in% cols_quitar)] %>% na.omit()

# Codificar variable objetivo como enteros (0, 1, 2)
df_modelo$label <- as.integer(df_modelo$resultado_partido) - 1

# ======================================
# 🔄 3. Separar train y test
# ======================================
set.seed(123)
part <- createDataPartition(df_modelo$label, p = 0.8, list = FALSE)
train_data <- df_modelo[part, ]
test_data  <- df_modelo[-part, ]

# ======================================
# 🔡 4. Preparar matrices para XGBoost
# ======================================
X_train <- model.matrix(label ~ . - resultado_partido, data = train_data)[, -1]
X_test  <- model.matrix(label ~ . - resultado_partido, data = test_data)[, -1]

y_train <- train_data$label
y_test  <- test_data$label

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test, label = y_test)
num_clases <- length(unique(y_train))

# ======================================
# 🔁 5. Grid Search con almacenamiento del mejor modelo
# ======================================
etas <- c(0.01, 0.05, 0.1)
depths <- c(3, 5, 7)
subs <- c(0.8, 1)
cols <- c(0.8, 1)

resultados <- data.frame()
mejor_accuracy <- -Inf
modelo_mejor <- NULL
parametros_mejor <- NULL

for (md in depths) {
  for (eta in etas) {
    for (sub in subs) {
      for (col in cols) {
        
        params <- list(
          objective = "multi:softmax",
          num_class = num_clases,
          eval_metric = "merror",
          max_depth = md,
          eta = eta,
          subsample = sub,
          colsample_bytree = col
        )
        
        set.seed(123)
        modelo <- xgb.train(
          params = params,
          data = dtrain,
          nrounds = 100,
          verbose = 0
        )
        
        pred <- predict(modelo, newdata = dtest)
        acc <- mean(pred == y_test)
        
        resultados <- rbind(resultados, data.frame(
          max_depth = md, eta = eta, subsample = sub, colsample_bytree = col, accuracy = round(acc, 4)
        ))
        
        cat(sprintf("🔎 max_depth=%d, eta=%.2f, subsample=%.2f, colsample=%.2f => acc=%.4f\n", md, eta, sub, col, acc))
        
        if (acc > mejor_accuracy) {
          mejor_accuracy <- acc
          modelo_mejor <- modelo
          parametros_mejor <- params
          cat("✅ Nuevo mejor modelo guardado\n")
        }
      }
    }
  }
}

# ======================================
# 🏁 6. Evaluación final con el mejor modelo
# ======================================
pred_final <- predict(modelo_mejor, newdata = dtest)

niveles <- levels(df_modelo$resultado_partido)
matriz_final <- table(
  Predicho = factor(pred_final, levels = 0:2, labels = niveles),
  Real = factor(y_test, levels = 0:2, labels = niveles)
)

cat("\n📊 Matriz de confusión del mejor modelo XGBoost:\n")
print(matriz_final)

cat("\n🎯 Accuracy final del mejor modelo:", round(mejor_accuracy, 4), "\n")
