# Procesamiento reproducible de la hoja abreviada MINVU.
# Desde Proyecto.Rproj: source("scripts/procesar_vivienda.R"); procesar_vivienda()
procesar_vivienda <- function(
  archivo = "06_vivienda/datos_originales/minvu_censos.xlsx",
  salida = "datos"
) {
  x <- readxl::read_excel(archivo, sheet = "2024 2017 2002 Abreviada",
                         col_names = FALSE, .name_repair = "minimal")
  stopifnot(trimws(x[[1]][8]) == "Total País",
            x[[4]][6] == "VIVIENDAS", x[[7]][6] == "HOGARES",
            x[[28]][6] == "TOTAL DÉFICIT HABITACIONAL")
  regiones <- trimws(as.character(x[[1]][10:25]))
  stopifnot(length(regiones) == 16, !anyDuplicated(regiones))
  comunas <- which(!is.na(suppressWarnings(as.numeric(x[[3]]))))
  stopifnot(length(comunas) > 300, !anyDuplicated(x[[3]][comunas]))
  tablas <- lapply(seq_along(c(2002, 2017, 2024)), function(i) {
    anio <- c(2002, 2017, 2024)[i]
    cols <- c(viviendas = 3+i, hogares = 6+i, deficit = 27+i)
    stopifnot(all(vapply(cols, function(j) as.numeric(x[[j]][7]) == anio, logical(1))))
    d <- data.frame(anio = anio, region = c("Total País", regiones))
    for (nombre in names(cols)) {
      col <- cols[[nombre]]
      d[[nombre]] <- as.numeric(x[[col]][c(8,10:25)])
      stopifnot(all(is.finite(d[[nombre]])), all(d[[nombre]] >= 0),
                sum(d[[nombre]][-1]) == d[[nombre]][1])
      # Verificar las agrupaciones territoriales publicadas contra sus comunas.
      for (region in regiones) {
        filas <- comunas[trimws(as.character(x[[1]][comunas])) == region]
        stopifnot(length(filas) > 0,
                  sum(as.numeric(x[[col]][filas])) == d[[nombre]][d$region == region])
      }
    }
    # Reconciliar los cuatro componentes con el déficit del cuadro.
    componentes <- lapply(c(9+i,12+i,21+i,24+i), function(j) as.numeric(x[[j]][c(8,10:25)]))
    stopifnot(all(Reduce(`+`, componentes) == d$deficit), all(d$hogares > 0))
    d$metodologia <- "Abreviada"
    d$deficit_por_100_hogares <- 100*d$deficit/d$hogares
    d$participacion_deficit_nacional_pct <- 100*d$deficit/d$deficit[1]
    d
  })
  serie <- dplyr::bind_rows(tablas) |>
    dplyr::group_by(region) |>
    dplyr::arrange(anio, .by_group = TRUE) |>
    dplyr::mutate(
      variacion_viviendas_pct = 100*(viviendas/dplyr::lag(viviendas)-1),
      variacion_hogares_pct = 100*(hogares/dplyr::lag(hogares)-1),
      variacion_deficit_pct = 100*(deficit/dplyr::lag(deficit)-1)
    ) |> dplyr::ungroup()
  nacional <- dplyr::filter(serie, region == "Total País")
  regional <- dplyr::filter(serie, region != "Total País")

  # Mantener también el nivel comunal para análisis territoriales.
  # La misma metodología abreviada permite comparar 2002, 2017 y 2024.
  tablas_comunales <- lapply(seq_along(c(2002, 2017, 2024)), function(i) {
    anio <- c(2002, 2017, 2024)[i]
    cols <- c(viviendas = 3 + i, hogares = 6 + i, deficit = 27 + i)
    d <- data.frame(
      anio = anio,
      region = trimws(as.character(x[[1]][comunas])),
      comuna = trimws(as.character(x[[2]][comunas])),
      codigo_comuna_ine = as.integer(x[[3]][comunas])
    )
    for (nombre in names(cols)) {
      d[[nombre]] <- as.numeric(x[[cols[[nombre]]]][comunas])
    }
    stopifnot(
      all(is.finite(d$viviendas)), all(is.finite(d$hogares)),
      all(is.finite(d$deficit)), all(d$hogares >= 0), all(d$deficit >= 0),
      all(d$deficit[d$hogares == 0] == 0),
      !anyDuplicated(d$codigo_comuna_ine)
    )
    d$metodologia <- "Abreviada"
    # Antártica figura con 0 hogares y 0 déficit en 2024: la tasa no se define.
    d$deficit_por_100_hogares <- ifelse(
      d$hogares > 0, 100 * d$deficit / d$hogares, NA_real_
    )
    d
  })
  comunal <- dplyr::bind_rows(tablas_comunales) |>
    dplyr::group_by(codigo_comuna_ine) |>
    dplyr::arrange(anio, .by_group = TRUE) |>
    dplyr::mutate(
      variacion_deficit_pct = 100 * (deficit / dplyr::lag(deficit) - 1)
    ) |>
    dplyr::ungroup()
  stopifnot(
    nrow(nacional) == 3, nrow(regional) == 48,
    !anyDuplicated(serie[c("region", "anio")]),
    !anyDuplicated(comunal[c("codigo_comuna_ine", "anio")]),
    all(vapply(c(2002, 2017, 2024), function(anio) {
      sum(comunal$deficit[comunal$anio == anio]) ==
        nacional$deficit[nacional$anio == anio]
    }, logical(1)))
  )
  dir.create(salida, showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(nacional, file.path(salida,"vivienda_nacional_abreviada.csv"), na = "")
  readr::write_csv(regional, file.path(salida,"vivienda_regional_abreviada.csv"), na = "")
  readr::write_csv(comunal, file.path(salida, "vivienda_comunal_abreviada.csv"), na = "")
  message("Vivienda: 3 años, 16 regiones y ",
          dplyr::n_distinct(comunal$codigo_comuna_ine),
          " comunas; totales nacionales, componentes y comunas conciliados.")
  invisible(list(nacional = nacional, regional = regional, comunal = comunal))
}
