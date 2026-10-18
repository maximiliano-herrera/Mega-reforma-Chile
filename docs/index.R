# index — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- index-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

# Indicadores breves que se muestran en la portada. Se calculan desde los mismos
# CSV que alimentan las páginas temáticas para evitar cifras escritas a mano.
vivienda_portada <- leer('vivienda_nacional_abreviada')
empleo_portada <- leer('empleo_ene') |> filter(anio == 2025)
tributacion_portada <- leer('tributacion') |> filter(anio == 2025)
finanzas_portada <- leer('finanzas_publicas') |> filter(anio == 2025)

reduccion_deficit_vivienda <- 100 * (
  vivienda_portada$deficit[vivienda_portada$anio == 2024] /
    vivienda_portada$deficit[vivienda_portada$anio == 2002] - 1
)
brecha_ocupacion_sexo <-
  empleo_portada$ocupacion_pct[empleo_portada$sexo == 'Hombres'] -
  empleo_portada$ocupacion_pct[empleo_portada$sexo == 'Mujeres']
primera_categoria_total <- 100 *
  tributacion_portada$primera_categoria / tributacion_portada$total
