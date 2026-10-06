# 04_empleo_formal — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- 04-empleo-formal-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

## ---- 04-empleo-formal-paso-2
# Cargar datos y calcular indicadores
d <- leer('empleo_ene') |> filter(anio>=2018)
t <- filter(d,sexo=='Total');u <- filter(t,anio==2025)
regional <- leer('empleo_ene_regional') |> filter(anio>=2018)
stopifnot(nrow(regional)==128,!anyDuplicated(regional[c('anio','region')]))

## ---- 04-empleo-formal-paso-3
# Ocupación nacional: Total, Hombres y Mujeres. Cada etiqueta es porcentaje de 15+.
g_ocupacion <- d |> mutate(etiqueta=paste0(n(ocupacion_pct,1),'%'),
  ajuste=case_when(sexo=='Mujeres' ~ 1.25, TRUE ~ -.7))
print(ggplot(g_ocupacion,aes(anio,ocupacion_pct,colour=sexo,linetype=sexo))+
  geom_line(linewidth=.8)+geom_point(size=2)+
  geom_text(aes(label=etiqueta,vjust=ajuste),show.legend=FALSE,size=3)+
  scale_colour_manual(values=c(Total=paleta[1],Hombres=paleta[2],Mujeres=paleta[3]))+
  scale_linetype_manual(values=c(Total='solid',Hombres='dashed',Mujeres='longdash'))+
  scale_x_continuous(breaks=2018:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
    expand=expansion(mult=c(.05,.13)))+
  labs(title='Tasa de ocupación nacional por sexo',
    subtitle='Cada etiqueta es porcentaje de la población de 15 años o más',
    x=NULL,y='Porcentaje de población de 15 años o más',colour=NULL,linetype=NULL))

## ---- 04-empleo-formal-paso-4
# Desocupación nacional por sexo. Cada etiqueta es porcentaje de la fuerza de trabajo.
g_desocupacion <- d |> mutate(etiqueta=paste0(n(desocupacion_pct,1),'%'),
  ajuste=case_when(sexo=='Mujeres' ~ 1.25, TRUE ~ -.7))
print(ggplot(g_desocupacion,aes(anio,desocupacion_pct,colour=sexo,linetype=sexo))+
  geom_line(linewidth=.8)+geom_point(size=2)+
  geom_text(aes(label=etiqueta,vjust=ajuste),show.legend=FALSE,size=3)+
  scale_colour_manual(values=c(Total=paleta[1],Hombres=paleta[2],Mujeres=paleta[3]))+
  scale_linetype_manual(values=c(Total='solid',Hombres='dashed',Mujeres='longdash'))+
  scale_x_continuous(breaks=2018:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
    expand=expansion(mult=c(.08,.17)))+
  labs(title='Tasa de desocupación nacional por sexo',
    subtitle='Cada etiqueta es porcentaje de la fuerza de trabajo',
    x=NULL,y='Porcentaje de la fuerza de trabajo',colour=NULL,linetype=NULL))

## ---- 04-empleo-formal-paso-12
# Inactividad = 100% menos participación laboral. Incluye quienes no buscan empleo activamente.
g_inactividad <- d |>
  mutate(inactividad_pct=100-participacion_pct,
    etiqueta=paste0(n(inactividad_pct,1),'%'),
    ajuste=case_when(sexo=='Hombres' ~ 1.25, TRUE ~ -.7))
print(ggplot(g_inactividad,aes(anio,inactividad_pct,colour=sexo,linetype=sexo))+
  geom_line(linewidth=.8)+geom_point(size=2)+
  geom_text(aes(label=etiqueta,vjust=ajuste),show.legend=FALSE,size=3)+
  scale_colour_manual(values=c(Total=paleta[1],Hombres=paleta[2],Mujeres=paleta[3]))+
  scale_linetype_manual(values=c(Total='solid',Hombres='dashed',Mujeres='longdash'))+
  scale_x_continuous(breaks=2018:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
    expand=expansion(mult=c(.05,.13)))+
  labs(title='Tasa de inactividad nacional por sexo',
    subtitle='Cada etiqueta es porcentaje de la población de 15 años o más',
    x=NULL,y='Porcentaje de población de 15 años o más',colour=NULL,linetype=NULL))

## ---- 04-empleo-formal-paso-5
# Un gráfico por indicador para que las regiones se lean con claridad.
grafico_macrozona <- function(nombre, indicador) {
  x <- regional |>
    filter(macrozona==nombre) |>
    transmute(anio,region,pct=.data[[indicador]],
      etiqueta=paste0(n(.data[[indicador]],1),'%'))
  titulo <- if (indicador=='ocupacion_pct') 'Tasa de ocupación' else 'Tasa de desocupación'
  subtitulo <- if (indicador=='ocupacion_pct') {
    'Cada valor es porcentaje de la población de 15 años o más'
  } else {
    'Cada valor es porcentaje de la fuerza de trabajo'
  }
  ggplot(x,aes(anio,pct,colour=region))+
    geom_line(linewidth=.75)+geom_point(size=1.6)+
    geom_text(data=filter(x,anio==max(anio)),aes(label=etiqueta),
      hjust=-.15,show.legend=FALSE,size=2.8)+
    scale_x_continuous(breaks=2018:2025)+
    scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
      expand=expansion(mult=c(.06,.16)))+
    labs(title=paste(titulo,'regional:',nombre),subtitle=subtitulo,
      x=NULL,y='Porcentaje',colour=NULL)
}

## ---- 04-empleo-formal-paso-6
print(grafico_macrozona('Norte','ocupacion_pct'))

## ---- 04-empleo-formal-paso-7
print(grafico_macrozona('Norte','desocupacion_pct'))

## ---- 04-empleo-formal-paso-8
print(grafico_macrozona('Centro','ocupacion_pct'))

## ---- 04-empleo-formal-paso-9
print(grafico_macrozona('Centro','desocupacion_pct'))

## ---- 04-empleo-formal-paso-10
print(grafico_macrozona('Sur','ocupacion_pct'))

## ---- 04-empleo-formal-paso-11
print(grafico_macrozona('Sur','desocupacion_pct'))

