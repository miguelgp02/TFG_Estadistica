import pandas as pd
from scipy.stats import zscore

# Leer archivo CSV
ruta = r'D:\TFG\codigo\datos\premier-2024\valores-premier-2024.csv'
df = pd.read_csv(ruta)

# Calcular z-scores
df['z_score'] = zscore(df['Valor_mercado_total_millones'])
print(df['z_score'])
# Filtrar equipos con z-score entre -1 y +1
df_filtrado = df[(df['z_score'] >= -1) & (df['z_score'] <= 1)]

# Eliminar columna auxiliar
df_filtrado = df_filtrado.drop(columns='z_score')

# Mostrar resultados
print("Equipos con valor de mercado moderado (z-score entre -1 y +1):")
print(df_filtrado)

# (Opcional) Guardar
df_filtrado.to_csv(r'D:\TFG\codigo\datos\premier-2024\valores-filtrados-premier-2024.csv', index=False)
