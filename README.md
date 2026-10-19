# Política para no políticos

Proyecto de Maximiliano: análisis reproducible de políticas públicas con datos institucionales. El primer dossier examina el Plan de Reconstrucción Nacional anunciado en marzo–abril de 2026; el sitio se ampliará gradualmente con temas de salud, Estado, economía, vivienda y relaciones internacionales.

Sitio: https://maximiliano-herrera.github.io/Mega-reforma-Chile/

## Reproducir

1. Abrir Proyecto.Rproj. Instalar R y Quarto.
2. Ejecutar `source("scripts/instalar_paquetes.R")`.
3. Con los originales disponibles: `source("scripts/procesar_datos.R", encoding="UTF-8")`.
4. Ejecutar `quarto render` desde la raíz. El sitio se genera en `docs/`.

Los CSV depurados incluidos permiten renderizar sin ejecutar de nuevo el paso 3. Los originales voluminosos de Fonasa se conservan localmente y no se publican. El inventario `fuentes/manifest.csv` documenta los insumos. Los nuevos originales pequeños se incluyen en las carpetas temáticas. No se publican datos inventados ni se estiman efectos causales.

Las páginas y las limitaciones metodológicas están en español. `metodologia.qmd` documenta fuentes, transformaciones y límites; `glosario.qmd` explica conceptos en lenguaje simple. Las series previas a 2026 no son evidencia de efectos del plan.


## Revisar los análisis en RStudio

Abra Proyecto.Rproj y luego el archivo R del tema, por ejemplo 06_vivienda.R.
Pulse Source para ejecutarlo completo: carga paquetes y datos y muestra los gráficos
(en Plots, use las flechas para recorrerlos). Puede ejecutar también cada sección.
Los datos quedan en Environment. En vivienda, View(d) abre la base utilizada.

Cada archivo QMD utiliza los bloques de su archivo R mediante knitr::read_chunk.
Modifique cálculos y gráficos en el R; edite textos y pies de figuras en el QMD.
Conserve los separadores `## ---- ...`, que conectan ambos archivos.
Para una nueva sección de código, hay que añadir su bloque correspondiente al QMD.
Después pulse Render en el QMD para revisar el artículo completo. No publica en GitHub.
Index y metodología tienen scripts de preparación pero no gráficos propios.
