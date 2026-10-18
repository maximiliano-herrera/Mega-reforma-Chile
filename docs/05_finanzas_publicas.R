# 05_finanzas_publicas — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- 05-finanzas-publicas-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

## ---- 05-finanzas-publicas-paso-2
# Cargar datos y calcular indicadores
d <- leer('finanzas_publicas') |>
  mutate(
    periodo_presidencial = case_when(
      anio <= 1993 ~ 'Aylwin (1990–1993)',
      anio <= 1999 ~ 'Frei Ruiz-Tagle (1994–1999)',
      anio <= 2005 ~ 'Lagos (2000–2005)',
      anio <= 2009 ~ 'Bachelet I (2006–2009)',
      anio <= 2013 ~ 'Piñera I (2010–2013)',
      anio <= 2017 ~ 'Bachelet II (2014–2017)',
      anio <= 2021 ~ 'Piñera II (2018–2021)',
      TRUE ~ 'Boric (2022–2025)'
    )
  )
u <- filter(d, anio == 2025)

periodos <- tibble(
  inicio = c(1990, 1994, 2000, 2006, 2010, 2014, 2018, 2022),
  fin = c(1993, 1999, 2005, 2009, 2013, 2017, 2021, 2025),
  periodo = c('Aylwin', 'Frei', 'Lagos', 'Bachelet I', 'Piñera I',
              'Bachelet II', 'Piñera II', 'Boric'),
  tono = rep(c('#e5e7eb', '#ffffff'), 4)
) |>
  mutate(etiqueta = paste0(periodo, '\n', inicio, '–', fin))

marcas_periodos <- c(1990, 1994, 2000, 2006, 2010, 2014, 2018, 2022, 2025)

tabla_anual <- d |>
  transmute(
    periodo = periodo_presidencial,
    anio = as.character(anio),
    ingresos_pct_pib,
    ingresos_millones_pesos_2025,
    gastos_pct_pib,
    gastos_millones_pesos_2025,
    balance_pct_pib,
    balance_millones_pesos_2025
  )

## ---- 05-finanzas-publicas-paso-3
# Ingresos y gastos como porcentaje del PIB. Las franjas distinguen períodos
# presidenciales, pero no atribuyen a cada gobierno los cambios observados.
g_pct <- d |>
  select(anio, Ingresos = ingresos_pct_pib, Gastos = gastos_pct_pib) |>
  pivot_longer(-anio, names_to = 'serie', values_to = 'valor')
periodos_pct <- periodos |>
  mutate(etiqueta_y = max(g_pct$valor) * 1.07)

grafico_pct <- ggplot(g_pct, aes(anio, valor, colour = serie, linetype = serie)) +
  geom_rect(data = periodos_pct,
            aes(xmin = inicio - .5, xmax = fin + .5, ymin = -Inf, ymax = Inf,
                fill = tono), inherit.aes = FALSE, alpha = .45) +
  geom_text(data = periodos_pct,
            aes(x = (inicio + fin) / 2, y = etiqueta_y, label = etiqueta),
            inherit.aes = FALSE, size = 2.35, colour = '#374151') +
  geom_line(linewidth = .7) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c(Ingresos = paleta[2], Gastos = paleta[1])) +
  scale_linetype_manual(values = c(Ingresos = 'dashed', Gastos = 'solid')) +
  scale_fill_identity() +
  scale_x_continuous(breaks = marcas_periodos) +
  scale_y_continuous(labels = label_number(suffix = '%', decimal.mark = ','),
                     expand = expansion(mult = c(.04, .10))) +
  labs(title = 'Ingresos y gastos del Gobierno Central Total',
       subtitle = 'Porcentaje del PIB · franjas alternadas: períodos presidenciales',
       x = NULL, y = 'Porcentaje del PIB', colour = NULL, linetype = NULL,
       caption = 'Fuente: DIPRES · Estado de Operaciones del Gobierno Central Total') +
  theme(legend.position = 'top', panel.grid.minor = element_blank())
print(grafico_pct)

## ---- 05-finanzas-publicas-paso-4
# La misma serie se expresa en pesos reales: millones de pesos de 2025.
g_pesos <- d |>
  select(anio,
         Ingresos = ingresos_millones_pesos_2025,
         Gastos = gastos_millones_pesos_2025) |>
  pivot_longer(-anio, names_to = 'serie', values_to = 'valor')
periodos_pesos <- periodos |>
  mutate(etiqueta_y = max(g_pesos$valor) * 1.08)

grafico_pesos <- ggplot(g_pesos, aes(anio, valor, colour = serie, linetype = serie)) +
  geom_rect(data = periodos_pesos,
            aes(xmin = inicio - .5, xmax = fin + .5, ymin = -Inf, ymax = Inf,
                fill = tono), inherit.aes = FALSE, alpha = .45) +
  geom_text(data = periodos_pesos,
            aes(x = (inicio + fin) / 2, y = etiqueta_y, label = etiqueta),
            inherit.aes = FALSE, size = 2.35, colour = '#374151') +
  geom_line(linewidth = .7) +
  geom_point(size = 1.6) +
  geom_text(data = filter(g_pesos, anio == 2025), aes(label = n(valor)),
            hjust = -.12, show.legend = FALSE, size = 3) +
  scale_colour_manual(values = c(Ingresos = paleta[2], Gastos = paleta[1])) +
  scale_linetype_manual(values = c(Ingresos = 'dashed', Gastos = 'solid')) +
  scale_fill_identity() +
  scale_x_continuous(breaks = marcas_periodos,
                     expand = expansion(mult = c(.01, .10))) +
  scale_y_continuous(labels = label_number(big.mark = '.', decimal.mark = ','),
                     expand = expansion(mult = c(.04, .12))) +
  labs(title = 'Ingresos y gastos en valores comparables',
       subtitle = 'Millones de pesos de 2025 · las etiquetas identifican el último año',
       x = NULL, y = 'Millones de pesos de 2025', colour = NULL, linetype = NULL,
       caption = 'Fuente: DIPRES · Estado de Operaciones del Gobierno Central Total') +
  theme(legend.position = 'top', panel.grid.minor = element_blank())
print(grafico_pesos)

## ---- 05-finanzas-publicas-paso-5
# Balance efectivo: la etiqueta incluye porcentaje y monto real de cada año.
# Las franjas asignan el año de cambio de mando al gobierno que inicia en marzo.
periodos_balance <- periodos |>
  mutate(etiqueta_y = max(d$balance_pct_pib) * 1.25)

grafico_balance <- ggplot(d, aes(anio, balance_pct_pib)) +
  geom_rect(data = periodos_balance,
            aes(xmin = inicio - .5, xmax = fin + .5, ymin = -Inf, ymax = Inf,
                fill = tono), inherit.aes = FALSE, alpha = .45) +
  geom_text(data = periodos_balance,
            aes(x = (inicio + fin) / 2, y = etiqueta_y, label = etiqueta),
            inherit.aes = FALSE, size = 2.15, lineheight = .9, colour = '#374151') +
  geom_col(fill = paleta[1], width = .72) +
  geom_hline(yintercept = 0, colour = '#4b5563') +
  geom_text(aes(label = paste0(
    ifelse(balance_pct_pib > 0, '+', ''), n(balance_pct_pib, 1), '%\n',
    ifelse(balance_millones_pesos_2025 > 0, '+', ''),
    n(balance_millones_pesos_2025 / 1e6, 1), ' billones'
  )), vjust = ifelse(d$balance_pct_pib >= 0, -.25, 1.15), size = 2.25, lineheight = .9) +
  scale_fill_identity() +
  scale_x_continuous(breaks = 1990:2025) +
  scale_y_continuous(labels = label_number(suffix = '%', decimal.mark = ','),
                     expand = expansion(mult = c(.16, .30))) +
  labs(title = 'Balance efectivo del Gobierno Central Total',
       subtitle = 'Préstamo neto / endeudamiento neto · etiquetas en billones de pesos de 2025',
       x = NULL, y = 'Balance efectivo (% del PIB)',
       caption = 'Fuente: DIPRES · barras negativas: déficit; barra positiva: superávit') +
  theme(panel.grid.minor = element_blank())
print(grafico_balance)

## ---- 05-finanzas-publicas-paso-6
knitr::kable(
  tabla_anual,
  digits = c(NA, 0, 1, 0, 1, 0, 1, 0),
  format.args = list(big.mark = '.', decimal.mark = ','),
  col.names = c(
    'Per\u00edodo', 'A\u00f1o', 'Ingresos (% PIB)',
    'Ingresos (millones de $ de 2025)', 'Gastos (% PIB)',
    'Gastos (millones de $ de 2025)', 'Balance (% PIB)',
    'Balance (millones de $ de 2025)'
  ),
  caption = 'Serie anual completa. Montos expresados en millones de pesos de 2025.'
)

