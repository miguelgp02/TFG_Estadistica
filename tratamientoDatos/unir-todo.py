import pandas as pd
import os

# Ruta base donde están los CSVs
ruta_base = "D:/TFG/codigo/datos/"

# Diccionario con los archivos por liga
ligas = {
    "primera": "indicadores-filtrados-primera-TODOS.csv",
    "bundesliga": "indicadores-filtrados-bundesliga-TODOS.csv",
    "premier": "indicadores-filtrados-premier-TODOS.csv"
}

# Lista para almacenar los DataFrames
dfs = []

# Cargar, añadir columna 'liga' y acumular
for liga, archivo in ligas.items():
    ruta = os.path.join(ruta_base, archivo)
    df = pd.read_csv(ruta)
    df["liga"] = liga
    dfs.append(df)

# Concatenar todos los DataFrames
df_total = pd.concat(dfs, ignore_index=True)

# Guardar archivo combinado
output_path = os.path.join(ruta_base, "indicadores-filtrados-TODAS-LIGAS.csv")
df_total.to_csv(output_path, index=False)

print("✅ Archivo unificado generado: indicadores-filtrados-TODAS-LIGAS.csv")
