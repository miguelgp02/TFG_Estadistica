import pandas as pd

# Cargar los archivos para la temporada 2024 (Premier League)
equipos_todos = pd.read_csv("D:/TFG/codigo/datos/premier-2024/equipos-premier-2024.csv")
equipos_filtrados = pd.read_csv("D:/TFG/codigo/datos/premier-2024/equipos-filtrados-premier-2024.csv")
partidos = pd.read_csv("D:/TFG/codigo/datos/premier-2024/partidos-premier-2024.csv")
indicadores = pd.read_csv("D:/TFG/codigo/datos/premier-2024/indicadoresEquipoHistoricoModelo-premier-2024.csv")

# Obtener los ID de todos los equipos y los filtrados
ids_todos = set(equipos_todos["id_equipo"])
ids_filtrados = set(equipos_filtrados["id_equipo"])

# Calcular los equipos eliminados
ids_eliminados = ids_todos - ids_filtrados

# Filtrar partidos a partir de la jornada 10
partidos_j10_plus = partidos[partidos["jornada"] >= 10]

# Seleccionar partidos donde participe al menos un equipo eliminado
partidos_afectados = partidos_j10_plus[
    partidos_j10_plus["id_equipo_local"].isin(ids_eliminados) |
    partidos_j10_plus["id_equipo_visitante"].isin(ids_eliminados)
]

# Extraer la lista de IDs de partidos afectados
ids_partidos_a_eliminar = set(partidos_afectados["id_partido"])

# Filtrar los indicadores para conservar solo los partidos NO afectados
indicadores_filtrados = indicadores[~indicadores["id_partido"].isin(ids_partidos_a_eliminar)]

# Guardar el nuevo CSV limpio
indicadores_filtrados.to_csv("D:/TFG/codigo/datos/premier-2024/indicadores-filtrados-premier-2024.csv", index=False)

print("✅ Indicadores filtrados correctamente para Premier League 2024.")
