# metodologia — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- metodologia-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

# Esta página es principalmente texto; no contiene gráficos propios.
