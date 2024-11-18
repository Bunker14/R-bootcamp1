# Paso 1: configuración inicial
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))
consumo <- c(120, 150, 180, NA, 170, 190, NA, 220, 160, 180, 90, NA, 130, 140, 100, NA, 120, 125, 105, 95)
costo_kwh <- c(rep(0.15, 10), rep(0.20, 10))

# Paso 2: limpieza de datos
# Calcular la mediana del consumo para cada tipo de energía
mediana_renovable <- median(consumo[energia == "Renovable"], na.rm = TRUE)
mediana_no_renovable <- median(consumo[energia == "No Renovable"], na.rm = TRUE)

# Reemplazar valores NA con la mediana correspondiente
consumo[is.na(consumo) & energia == "Renovable"] <- mediana_renovable
consumo[is.na(consumo) & energia == "No Renovable"] <- mediana_no_renovable

# Paso 3: creación del dataframe
df_consumo <- data.frame(energia, consumo, costo_kwh)

# Paso 4: cálculos
# Agregar la columna costo_total
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Calcular totales y medias por tipo de energía
total_consumo <- aggregate(consumo ~ energia, data = df_consumo, sum)
total_costo <- aggregate(costo_total ~ energia, data = df_consumo, sum)
media_consumo <- aggregate(consumo ~ energia, data = df_consumo, mean)

# Agregar la columna ganancia con un aumento del 10%
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: resumen
# Ordenar el dataframe por costo_total en orden descendente
df_ordenado <- df_consumo[order(-df_consumo$costo_total), ]

# Extraer las tres filas con el mayor costo_total
top_3_costos <- head(df_ordenado, 3)

# Crear la lista resumen_energia
resumen_energia <- list(
  DataframeOrdenado = df_ordenado,
  TotalConsumo = total_consumo,
  TotalCosto = total_costo,
  Top3Costos = top_3_costos
)

# Mostrar la lista resumen_energia
print(resumen_energia)

