# Ejecutar al comenzar una sesión de trabajo.
# Requiere internet si hay paquetes que todavía no están instalados.

# Instalar pacman solo si falta.
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman", repos = "https://cloud.r-project.org")
}

# Instalar los paquetes faltantes y cargar todos en la sesión actual.
pacman::p_load(
  readxl, dplyr, tidyr, readr, jsonlite,
  rvest, ggplot2, scales, knitr, rmarkdown
)
