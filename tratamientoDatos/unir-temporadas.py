import pandas as pd
import os

# Ruta base donde están guardados los CSVs
ruta_base = "D:/TFG/codigo/datos/"

# Años que vamos a unir
años = [2019, 2020, 2021, 2022, 2023, 2024]

# Lista para guardar los DataFrames
dfs = []

# Leer y añadir columna 'temporada'
for año in años:
    ruta = os.path.join(ruta_base, f"premier-{año}", f"indicadores-filtrados-premier-{año}.csv")
    df = pd.read_csv(ruta)
    df["temporada"] = año
    dfs.append(df)

# Concatenar todos los DataFrames
df_consolidado = pd.concat(dfs, ignore_index=True)

# Guardar CSV final
df_consolidado.to_csv("D:/TFG/codigo/datos/indicadores-filtrados-premier-TODOS.csv", index=False)

print("✅ CSV conjunto creado: indicadores-filtrados-premier-TODOS.csv")
