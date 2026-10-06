# VIVIENDA: evolución censal y diagnóstico regional
# Abra Proyecto.Rproj y pulse Source. No necesita hacer Render.
# Datos en Environment: nacional, regional y comunal (metodología abreviada).
# View(nacional) / View(regional) / View(comunal) permiten revisar las tablas.
# Los gráficos aparecen en Plots; use las flechas para recorrerlos.
# Conserve los separadores ## ----: conectan este código con el QMD.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- vivienda-preparacion
source("scripts/graficos.R", encoding = "UTF-8")

## ---- vivienda-datos
# El gráfico lee tablas procesadas. Para reconstruirlas desde el Excel original:
# source("scripts/procesar_vivienda.R", encoding = "UTF-8")
# procesar_vivienda()
nacional <- leer("vivienda_nacional_abreviada")
regional <- leer("vivienda_regional_abreviada")
comunal <- leer("vivienda_comunal_abreviada") |>
  dplyr::group_by(anio) |>
  dplyr::mutate(participacion_deficit_nacional_pct = 100 * deficit / sum(deficit)) |>
  dplyr::ungroup()

stopifnot(nrow(nacional) == 3, nrow(regional) == 48, nrow(comunal) > 900)

porcentaje <- function(x) paste0(n(x, 1), "%")
fuente_abreviada <- "Fuente: MINVU · Censos 2002, 2017 y 2024 · Metodología abreviada"

## ---- vivienda-nacional-deficit
etiquetas_deficit <- paste0(
  n(nacional$deficit), "\n",
  ifelse(
    is.na(nacional$variacion_deficit_pct),
    "Base de comparación",
    paste0(ifelse(nacional$variacion_deficit_pct > 0, "+", ""),
           porcentaje(nacional$variacion_deficit_pct), " vs. censo anterior")
  )
)
grafico_deficit_nacional <- ggplot(nacional, aes(x = anio, y = deficit)) +
  geom_line(colour = paleta[2], linewidth = .8, linetype = "dashed") +
  geom_point(colour = paleta[2], size = 3) +
  geom_text(aes(label = etiquetas_deficit), vjust = -.65, size = 3.5,
            lineheight = 1.05) +
  scale_x_continuous(breaks = c(2002, 2017, 2024),
                     expand = expansion(mult = c(.18, .2))) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA), expand = expansion(mult = c(.02, .25))) +
  labs(
    title = "Déficit habitacional total · metodología abreviada",
    subtitle = "Cantidad y variación porcentual frente al censo anterior",
    x = NULL, y = "Cantidad", caption = fuente_abreviada
  ) +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 9))
print(grafico_deficit_nacional)

## ---- vivienda-regional-2017-2024
# Comparación legible entre los dos censos más recientes.
# El orden se fija según el total de 2024. La etiqueta muestra la participación
# de cada región en el déficit nacional del mismo censo.
regional_17_24 <- regional |>
  dplyr::filter(anio %in% c(2017, 2024)) |>
  dplyr::group_by(region) |>
  dplyr::mutate(deficit_2024 = deficit[anio == 2024]) |>
  dplyr::ungroup()

orden_regional <- regional_17_24 |>
  dplyr::distinct(region, deficit_2024) |>
  dplyr::arrange(deficit_2024) |>
  dplyr::pull(region)

regional_17_24 <- regional_17_24 |>
  dplyr::mutate(
    anio = factor(anio, levels = c(2017, 2024)),
    region = factor(region, levels = orden_regional),
    etiqueta = paste0(n(deficit), " · ",
                       porcentaje(participacion_deficit_nacional_pct),
                       " del déficit nacional")
  )

grafico_regional_17_24 <- ggplot(
  regional_17_24, aes(x = deficit, y = region, fill = anio)
) +
  geom_col(position = position_dodge(width = .72), width = .64) +
  geom_text(
    aes(label = etiqueta), position = position_dodge(width = .72),
    hjust = -.08, size = 3
  ) +
  scale_fill_manual(values = c("2017" = paleta[1], "2024" = paleta[2])) +
  scale_x_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, .36))
  ) +
  labs(
    title = "Déficit habitacional por región, 2017 y 2024",
    subtitle = "Etiqueta: requerimientos · porcentaje del déficit nacional del mismo año",
    x = "Requerimientos de nuevas viviendas", y = NULL, fill = "Censo",
    caption = fuente_abreviada
  ) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
print(grafico_regional_17_24)






## ---- vivienda-comunal-top-aumentos
# Comparar la misma comuna en ambos censos usando el código INE.
# Seleccionamos aumentos en cantidad absoluta, no aumentos porcentuales.
top_aumentos_comunales <- comunal |>
  dplyr::filter(anio %in% c(2017, 2024)) |>
  dplyr::select(codigo_comuna_ine, region, comuna, anio, deficit,
                participacion_deficit_nacional_pct) |>
  tidyr::pivot_wider(
    names_from = anio,
    values_from = c(deficit, participacion_deficit_nacional_pct),
    names_glue = "{.value}_{anio}"
  ) |>
  dplyr::mutate(
    aumento_absoluto = deficit_2024 - deficit_2017,
    aumento_porcentual = 100 * (deficit_2024 / deficit_2017 - 1)
  ) |>
  dplyr::filter(aumento_absoluto > 0) |>
  dplyr::arrange(dplyr::desc(aumento_absoluto), comuna) |>
  dplyr::slice_head(n = 10)

top_aumentos_largo <- top_aumentos_comunales |>
  dplyr::select(comuna, region, aumento_absoluto, aumento_porcentual,
                deficit_2017, deficit_2024,
                participacion_deficit_nacional_pct_2017,
                participacion_deficit_nacional_pct_2024) |>
  tidyr::pivot_longer(
    cols = c(deficit_2017, deficit_2024,
             participacion_deficit_nacional_pct_2017,
             participacion_deficit_nacional_pct_2024),
    names_to = c(".value", "anio"),
    names_pattern = "(.*)_(2017|2024)"
  ) |>
  dplyr::mutate(
    anio = factor(anio, levels = c("2017", "2024")),
    comuna = factor(comuna, levels = rev(top_aumentos_comunales$comuna)),
    etiqueta = paste0(n(deficit), " · ",
                       porcentaje(participacion_deficit_nacional_pct),
                       " del déficit nacional")
  )

grafico_top_aumentos_comunales <- ggplot(
  top_aumentos_largo,
  aes(x = deficit, y = comuna, fill = anio)
) +
  geom_col(position = position_dodge(width = .72), width = .64) +
  geom_text(
    aes(label = etiqueta),
    position = position_dodge(width = .72), hjust = -.08, size = 3
  ) +
  scale_fill_manual(values = c("2017" = paleta[1], "2024" = paleta[2])) +
  scale_x_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, .35))
  ) +
  labs(
    title = "Las 10 comunas con mayor aumento del déficit, 2017–2024",
    subtitle = "Selección por aumento absoluto; etiqueta: requerimientos · porcentaje del déficit nacional",
    x = "Requerimientos de nuevas viviendas", y = NULL, fill = "Censo",
    caption = fuente_abreviada
  ) +
  theme(panel.grid.major.y = element_blank(), legend.position = "top")
print(grafico_top_aumentos_comunales)




