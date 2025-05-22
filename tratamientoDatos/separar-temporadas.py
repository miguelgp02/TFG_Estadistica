import pandas as pd

# Cargar CSV completo Premier
df_premier = pd.read_csv("D:/TFG/codigo/datos/indicadores-filtrados-premier-TODOS.csv")

# Separar por tipo de temporada
df_premier_3 = df_premier[df_premier["temporada"].isin([2019, 2021, 2022])]
df_premier_5 = df_premier[df_premier["temporada"].isin([2023, 2024])]
df_premier_trans = df_premier[df_premier["temporada"] == 2020]

# Guardar CSVs
df_premier_3.to_csv("D:/TFG/codigo/datos/indicadores-premier-3-cambios.csv", index=False)
df_premier_5.to_csv("D:/TFG/codigo/datos/indicadores-premier-5-cambios.csv", index=False)
df_premier_trans.to_csv("D:/TFG/codigo/datos/indicadores-premier-transicion.csv", index=False)

print("✅ Premier League separada en 3 archivos.")
