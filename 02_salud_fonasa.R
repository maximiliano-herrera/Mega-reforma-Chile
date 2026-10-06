# 02_salud_fonasa — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- 02-salud-fonasa-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

## ---- 02-salud-fonasa-paso-2
# Cargar datos y calcular indicadores
f <- leer('fonasa_tramos')
t <- f |> group_by(anio) |> summarise(beneficiarios=sum(beneficiarios),.groups='drop')
movimientos_isapres <- leer('isapres_movimientos')
stopifnot(all(movimientos_isapres$incremento_neto==movimientos_isapres$suscripciones-movimientos_isapres$desahucios))

## ---- 02-salud-fonasa-paso-3
# Total anual: cada etiqueta muestra el número de beneficiarios registrados.
g_total <- t |> mutate(etiqueta=n(beneficiarios))
print(ggplot(g_total,aes(anio,beneficiarios))+
  geom_line(colour=paleta[1],linewidth=.9)+geom_point(colour=paleta[1],size=2.5)+
  geom_text(aes(label=etiqueta),vjust=-.7,size=3)+
  scale_x_continuous(breaks=2021:2025)+
  scale_y_continuous(labels=label_number(big.mark='.',decimal.mark=','),
    expand=expansion(mult=c(.03,.12)))+
  labs(title='Beneficiarios registrados de Fonasa',
    subtitle='Cada etiqueta muestra el número de beneficiarios',
    x=NULL,y='Beneficiarios registrados'))

## ---- 02-salud-fonasa-paso-4
# Barras agrupadas y horizontales: un panel por tramo, cinco cortes anuales comparables.
g_tramos <- f |>
  mutate(anio=factor(anio),grupo='Beneficiarios',etiqueta=n(beneficiarios))
print(ggplot(g_tramos,aes(grupo,beneficiarios,fill=anio))+
  geom_col(position=position_dodge(width=.78),width=.68)+
  geom_text(aes(label=etiqueta),position=position_dodge(width=.78),
    hjust=-.08,size=2.6,show.legend=FALSE)+
  facet_wrap(~tramo,ncol=1)+coord_flip()+
  scale_fill_manual(values=c(paleta,'#4b5563'))+
  scale_y_continuous(labels=label_number(big.mark='.',decimal.mark=','),
    expand=expansion(mult=c(0,.18)))+
  labs(title='Beneficiarios de Fonasa por tramo y año',
    subtitle='Cada barra y etiqueta muestran un conteo al cierre de diciembre',
    x=NULL,y='Beneficiarios registrados',fill='Año'))

## ---- 02-salud-fonasa-paso-6
# Suscripciones, términos y saldo neto de contratos, sin inferir el destino ni la causa de cada término.
g_movimientos <- movimientos_isapres |>
  filter(anio>=2018) |>
  select(anio,Suscripciones=suscripciones,`Términos de contratos`=desahucios) |>
  pivot_longer(-anio,names_to='tipo',values_to='contratos')
print(ggplot(g_movimientos,aes(anio,contratos,colour=tipo,linetype=tipo))+
  geom_line(linewidth=.8)+geom_point(size=2)+
  geom_text(data=filter(g_movimientos,anio==2025),aes(label=n(contratos)),
    vjust=-.7,show.legend=FALSE,size=3)+
  scale_colour_manual(values=c(Suscripciones=paleta[3],`Términos de contratos`=paleta[2]))+
  scale_linetype_manual(values=c(Suscripciones='solid',`Términos de contratos`='dashed'))+
  scale_x_continuous(breaks=2018:2025)+scale_y_continuous(labels=label_number(big.mark='.',decimal.mark=','),
    expand=expansion(mult=c(.05,.14)))+
  labs(title='Suscripciones y términos de contratos Isapre',
    subtitle='Cada línea muestra contratos durante el año; la etiqueta corresponde a 2025',
    x=NULL,y='Contratos',colour=NULL,linetype=NULL))

## ---- 02-salud-fonasa-paso-7
g_neto <- movimientos_isapres |> filter(anio>=2018) |>
  mutate(etiqueta=n(incremento_neto),color=if_else(incremento_neto>=0,'Positivo','Negativo'))
print(ggplot(g_neto,aes(factor(anio),incremento_neto,fill=color))+
  geom_col(width=.7)+geom_hline(yintercept=0)+
  geom_text(aes(label=etiqueta),vjust=if_else(g_neto$incremento_neto>=0,-.5,1.3),size=3)+
  scale_fill_manual(values=c(Positivo=paleta[3],Negativo=paleta[2]))+
  scale_y_continuous(labels=label_number(big.mark='.',decimal.mark=','),expand=expansion(mult=c(.12,.15)))+
  labs(title='Saldo neto anual de contratos Isapre',
    subtitle='Suscripciones menos términos de contratos; negativo implica más términos que suscripciones',
    x=NULL,y='Contratos',fill=NULL))

