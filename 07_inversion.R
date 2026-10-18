# 07_inversion — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- 07-inversion-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

## ---- 07-inversion-paso-2
# Cargar datos y calcular indicadores
d<-leer('inversion') |> filter(anio>=2018)
cruce <- leer('impuesto_fbcf')
stopifnot(nrow(cruce)==13,!anyDuplicated(cruce$anio),
          all(cruce$fbcf_pct_pib>0),all(cruce$tasa_general_pct>0))

## ---- 07-inversion-paso-3
# FBCF anual: cada etiqueta muestra el porcentaje del PIB.
g_fbcf <- d |> mutate(etiqueta=paste0(n(fbcf_pct_pib,1),'%'))
print(ggplot(g_fbcf,aes(anio,fbcf_pct_pib))+
  geom_line(colour=paleta[1],linewidth=.9)+geom_point(colour=paleta[1],size=2.5)+
  geom_text(aes(label=etiqueta),vjust=-.7,size=3)+
  scale_x_continuous(breaks=2018:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
    expand=expansion(mult=c(.05,.15)))+
  labs(title='Formación bruta de capital fijo',
    subtitle='Cada etiqueta muestra el porcentaje del PIB',
    x=NULL,y='FBCF como porcentaje del PIB'))

## ---- 07-inversion-paso-4
# Comparación de series: dos paneles para no confundir porcentajes de distinta naturaleza.
serie_cruce <- cruce |>
  select(anio,`FBCF (% del PIB)`=fbcf_pct_pib,`Tasa general de Primera Categoría`=tasa_general_pct) |>
  pivot_longer(-anio,names_to='indicador',values_to='porcentaje') |>
  mutate(etiqueta=paste0(n(porcentaje,1),'%'))
print(ggplot(serie_cruce,aes(anio,porcentaje))+
  geom_line(colour=paleta[1],linewidth=.8)+geom_point(colour=paleta[1],size=2)+
  geom_text(aes(label=etiqueta),vjust=-.7,size=3)+
  facet_wrap(~indicador,ncol=1,scales='free_y')+
  scale_x_continuous(breaks=2013:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
    expand=expansion(mult=c(.06,.16)))+
  labs(title='FBCF y tasa legal de Primera Categoría',
    subtitle='Dos porcentajes con denominadores distintos; lectura descriptiva',
    x=NULL,y='Porcentaje'))

