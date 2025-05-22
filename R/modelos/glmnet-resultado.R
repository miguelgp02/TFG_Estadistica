# ======================================
# 📦 1. Cargar librerías
# ======================================
library(readr)
library(dplyr)
library(glmnet)
library(caret)
library(Matrix)
library(ggplot2)

# ======================================
# 📁 2. Cargar datos
# ======================================
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-5-cambios.csv")

# ======================================
# 🧹 3. Preprocesamiento
# ======================================
df <- df %>%
  mutate(resultado_partido = factor(resultado_partido, levels = c("1", "X", "2")))

# Eliminar columnas no deseadas
cols_quitar <- c("jornada", "id_indicadores_equipo_prepartido", "id_partido", "temporada")
df_modelo <- df[, !(names(df) %in% cols_quitar)]
df_modelo <- na.omit(df_modelo)

# Crear X e y explícitamente
X <- df_modelo[, !(names(df_modelo) %in% c("resultado_local", "resultado_visitante","resultado_partido"))]
y <- df_modelo$resultado_partido

# ======================================
# 🔄 4. Separar en train y test
# ======================================
set.seed(123)
part <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[part, ]
X_test  <- X[-part, ]
y_train <- y[part]
y_test  <- y[-part]

# Preparar matrices para glmnet
X_train_mat <- model.matrix(~ ., data = X_train)[, -1]
X_test_mat  <- model.matrix(~ ., data = X_test)[, -1]

# ======================================
# 🔁 5. Probar varios valores de alpha
# ======================================
alphas <- c(0, 0.25, 0.5, 0.75, 1)
resultados <- data.frame(alpha = numeric(), lambda = numeric(), accuracy = numeric(), stringsAsFactors = FALSE)

for (a in alphas) {
  set.seed(123)
  cv_model <- cv.glmnet(
    x = X_train_mat,
    y = y_train,
    family = "multinomial",
    type.measure = "class",
    alpha = a
  )
  
  pred <- predict(cv_model, newx = X_test_mat, s = "lambda.min", type = "class")
  acc <- mean(pred == y_test)
  
  resultados <- rbind(resultados, data.frame(alpha = a, lambda = cv_model$lambda.min, accuracy = acc))
  
  cat("\n✅ Alpha:", a, " | Lambda:", round(cv_model$lambda.min, 5), " | Accuracy:", round(acc, 4))
}

# ======================================
# 🏆 6. Mostrar mejores resultados
# ======================================
mejor <- resultados[which.max(resultados$accuracy), ]
cat("\n\n🏁 Mejor modelo:\n")
print(mejor)

# ======================================
# 📊 7. Visualizar accuracies
# ======================================
ggplot(resultados, aes(x = alpha, y = accuracy)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Accuracy en test según alpha (glmnet multinomial)",
       x = "Alpha (penalización)", y = "Accuracy") +
  theme_minimal()
