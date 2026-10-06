# 03_tributacion — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- 03-tributacion-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

## ---- 03-tributacion-paso-2
# Cargar datos y calcular indicadores
serie_completa <- leer('tributacion')
d <- serie_completa |> filter(anio>=2018)
u <- filter(d,anio==2025)
stopifnot(all(serie_completa$primera_categoria >= 0),
          all(serie_completa$otros_renta >= 0),
          all(serie_completa$otros_impuestos >= 0))

## ---- 03-tributacion-paso-3
# Mantener la vista agregada reciente. Cada etiqueta es porcentaje del PIB.
g_actual <- d |>
  select(anio, Total=total, Renta=renta, IVA=iva) |>
  pivot_longer(-anio, names_to='tipo', values_to='pct') |>
  mutate(etiqueta=paste0(n(pct,1),'%'),
         ajuste=case_when(tipo=='IVA' ~ 1.25, TRUE ~ -0.7))
print(ggplot(g_actual,aes(anio,pct,colour=tipo,linetype=tipo))+
  geom_line(linewidth=.8)+geom_point(size=2)+
  geom_text(aes(label=etiqueta,vjust=ajuste),show.legend=FALSE,size=3)+
  scale_colour_manual(values=c(IVA=paleta[1],Renta=paleta[2],Total=paleta[3]))+
  scale_linetype_manual(values=c(IVA='solid',Renta='dashed',Total='longdash'))+
  scale_x_continuous(breaks=2018:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
                     expand=expansion(mult=c(.05,.13)))+
  labs(title='Ingresos tributarios como porcentaje del PIB',
       subtitle='Cada etiqueta muestra el porcentaje del PIB',x=NULL,
       y='Porcentaje del PIB',colour=NULL,linetype=NULL))

## ---- 03-tributacion-paso-4
# Primera Categoría es la referencia más cercana al impuesto corporativo.
g_primera <- serie_completa |>
  transmute(anio, pct=primera_categoria, etiqueta=paste0(n(primera_categoria,1),'%'))
print(ggplot(g_primera,aes(anio,pct))+
  geom_line(colour=paleta[2],linewidth=.8)+geom_point(colour=paleta[2],size=2)+
  geom_text(aes(label=etiqueta),vjust=-.7,size=3)+
  scale_x_continuous(breaks=2009:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
                     expand=expansion(mult=c(.05,.16)))+
  labs(title='Impuesto de Primera Categoría',
       subtitle='Cada etiqueta muestra el porcentaje del PIB',x=NULL,
       y='Porcentaje del PIB'))

## ---- 03-tributacion-paso-5
# Dentro de cada barra, los componentes suman 100% del total tributario de ese año.
composicion <- serie_completa |>
  transmute(anio,
    IVA=100*iva/total,
    `Primera categoría`=100*primera_categoria/total,
    `Otros impuestos a la renta`=100*otros_renta/total,
    `Otros impuestos`=100*otros_impuestos/total) |>
  pivot_longer(-anio,names_to='componente',values_to='participacion_pct') |>
  mutate(componente=factor(componente,levels=c('IVA','Primera categoría',
    'Otros impuestos a la renta','Otros impuestos')),
    etiqueta=paste0(n(participacion_pct,1),'%'))
stopifnot(all(abs(tapply(composicion$participacion_pct,composicion$anio,sum)-100) < 1e-8))
print(ggplot(composicion,aes(anio,participacion_pct,fill=componente))+
  geom_col(width=.82)+
  geom_text(aes(label=etiqueta),position=position_stack(vjust=.5),size=2.45)+
  scale_fill_manual(values=c(IVA=paleta[1],`Primera categoría`=paleta[2],
    `Otros impuestos a la renta`=paleta[3],`Otros impuestos`=paleta[4]))+
  scale_x_continuous(breaks=2009:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),limits=c(0,100))+
  labs(title='Composición de los ingresos tributarios',
       subtitle='Cada barra suma 100% del total tributario del año',x=NULL,
       y='Porcentaje del total tributario',fill=NULL))

## ---- 03-tributacion-paso-6
# Cuánto representa Primera Categoría dentro de la recaudación de renta.
peso_primera_renta <- serie_completa |>
  transmute(anio,pct=primera_categoria_pct_renta,
            etiqueta=paste0(n(primera_categoria_pct_renta,1),'%'))
print(ggplot(peso_primera_renta,aes(anio,pct))+
  geom_line(colour=paleta[1],linewidth=.8)+geom_point(colour=paleta[1],size=2)+
  geom_text(aes(label=etiqueta),vjust=-.7,size=3)+
  scale_x_continuous(breaks=2009:2025)+
  scale_y_continuous(labels=scales::label_number(suffix='%',decimal.mark=','),
                     expand=expansion(mult=c(.05,.16)))+
  labs(title='Peso de Primera Categoría dentro de Renta',
       subtitle='Cada etiqueta es el porcentaje de la recaudación de renta',x=NULL,
       y='Porcentaje de Renta'))

