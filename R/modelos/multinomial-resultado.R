# ======================================
# 📦 1. Cargar librerías
# ======================================
library(readr)
library(dplyr)
library(nnet)       # Para modelo multinomial
library(caret)      # Para partición y métricas
library(ggplot2)

# ======================================
# 📁 2. Cargar datos
# ======================================
df <- read_csv("D:/TFG/codigo/datos/indicadores-filtrados-primera-5-cambios.csv")

# ======================================
# 🧹 3. Preprocesamiento
# ======================================
# Convertir resultado a factor con niveles ordenados
df <- df %>% mutate(resultado_partido = factor(resultado_partido, levels = c("1", "X", "2")))

# Eliminar columnas irrelevantes (ID, jornada, etc.)
vars_a_quitar <- c("jornada", "id_indicadores_equipo_prepartido", "id_partido", "temporada")
df_modelo <- df[, !(names(df) %in% vars_a_quitar)]

# Crear X (solo predictores) y y (variable objetivo)
X <- df_modelo[, !(names(df_modelo) %in% c("resultado_partido", "resultado_local", "resultado_visitante"))]
y <- df_modelo$resultado_partido

# ======================================
# 🧪 4. Separar entrenamiento y test
# ======================================
set.seed(123)
part <- createDataPartition(y, p = 0.8, list = FALSE)

X_train <- X[part, ]
X_test  <- X[-part, ]
y_train <- y[part]
y_test  <- y[-part]

# Combinar para usar en el modelo
train_data <- cbind(resultado_partido = y_train, X_train)
test_data  <- cbind(resultado_partido = y_test, X_test)

# ======================================
# 🤖 5. Ajustar modelo multinomial
# ======================================
modelo_multinom <- multinom(resultado_partido ~ ., data = train_data)

# ======================================
# 📈 6. Evaluar el modelo
# ======================================
# Predicciones
predicciones <- predict(modelo_multinom, newdata = test_data)

# Matriz de confusión
matriz <- confusionMatrix(predicciones, test_data$resultado_partido)
cat("\n📊 Matriz de confusión:\n")
print(matriz$table)
cat("\n🎯 Accuracy del modelo multinomial:", round(matriz$overall["Accuracy"], 4), "\n")

