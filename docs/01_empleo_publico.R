# 01_empleo_publico — revisión del análisis en RStudio
# Abra Proyecto.Rproj y pulse Source para ejecutar este archivo completo.
# Los objetos quedan en Environment; los gráficos aparecen en Plots.
# Este mismo código se utiliza al generar la página Quarto.
# Modifique el código bajo los separadores; conserve sus nombres.

if (!file.exists("_quarto.yml")) stop("Abra Proyecto.Rproj antes de ejecutar este script.")
source("scripts/instalar_paquetes.R", encoding = "UTF-8")

## ---- 01-empleo-publico-paso-1
# Preparar las herramientas
source("scripts/graficos.R", encoding="UTF-8")

## ---- 01-empleo-publico-paso-2
# Cargar datos y calcular indicadores
e <- leer('empleo_publico')
tipo_funcion <- leer('empleo_publico_funcion')
total <- filter(tipo_funcion,tipo=='Total Sector Público')
comp <- e |> filter(dependencia!='Sector Público',anio%in%c(2021,2025)) |> pivot_wider(names_from=anio,values_from=cargos,names_prefix='a') |> mutate(cambio=a2025-a2021,variacion=100*(a2025/a2021-1))
funcion_grafico <- tipo_funcion |> group_by(anio) |>
  mutate(participacion_pct=100*cargos/cargos[tipo=='Total Sector Público'],
         etiqueta=paste0(n(cargos),'\n',n(participacion_pct,1),'% del total'),
         ajuste=case_when(tipo=='Total Sector Público' ~ -0.65,
                           tipo=='Permanente' ~ -0.65,
                           TRUE ~ 1.25)) |>
  ungroup()
## ---- 01-empleo-publico-paso-3
# Total del Sector Público y sus funciones permanentes y transitorias.
print(ggplot(funcion_grafico,aes(anio,cargos,colour=tipo,linetype=tipo))+ 
  geom_line(linewidth=.9)+geom_point(size=2.2)+
  geom_text(aes(label=etiqueta,vjust=ajuste),show.legend=FALSE,size=2.7,lineheight=.9)+
  scale_colour_manual(values=c('Total Sector Público'=paleta[1],Permanente=paleta[3],Transitorio=paleta[2]))+
  scale_linetype_manual(values=c('Total Sector Público'='solid',Permanente='solid',Transitorio='dashed'))+
  scale_x_continuous(breaks=2021:2025)+
  expand_limits(y=0)+
  scale_y_continuous(labels=label_number(big.mark='.',decimal.mark=','),
    expand=expansion(mult=c(.05,.12)))+
  labs(title='Cargos del Sector Público por tipo de función',
    subtitle='Etiqueta: número de cargos y porcentaje del total anual',
    x=NULL,y='Cargos (promedio anual)',colour=NULL,linetype=NULL))
## ---- 01-empleo-publico-paso-4
# Mostrar gráfico o tabla
tabla(comp |> transmute(Dependencia=dependencia,`2021`=a2021,`2025`=a2025,`Cambio en cargos`=cambio,`Variación (%)`=variacion))

