# Ejecutar desde la raíz del proyecto: Rscript scripts/procesar_datos.R
# Análisis exclusivamente en R. Los archivos originales nunca se modifican.
if (.Platform$OS.type == 'windows') invisible(Sys.setlocale('LC_CTYPE','Spanish_Chile.utf8'))
suppressPackageStartupMessages({library(readxl); library(dplyr); library(tidyr); library(readr); library(jsonlite); library(rvest)})
options(stringsAsFactors=FALSE)
dir.create('datos',showWarnings=FALSE)
guardar <- function(x,nombre) {stopifnot(nrow(x)>0); write_csv(x,file.path('datos',paste0(nombre,'.csv')),na='')}
num <- function(d,r,cols) as.numeric(unlist(d[r,cols],use.names=FALSE))

# DIPRES: promedio anual de cargos, hoja General del archivo aportado.
f <- '01_empleo_publico/Personal disponible en el sector publico.xlsx'
e <- read_excel(f,sheet='General',col_names=FALSE,.name_repair='minimal')
stopifnot(e[[1]][2]=='Sector Público')
empleo <- bind_rows(lapply(c(2,5,8,11,14,17,20),function(r) data.frame(anio=2021:2025,dependencia=e[[1]][r],cargos=num(e,r,2:6))))
stopifnot(all(empleo$cargos>=0), !anyDuplicated(empleo[c('anio','dependencia')]))
tot <- empleo |> filter(dependencia=='Sector Público')
sumas <- empleo |> filter(dependencia!='Sector Público') |> group_by(anio) |> summarise(cargos=sum(cargos),.groups='drop')
stopifnot(all(tot$cargos==sumas$cargos))
guardar(empleo,'empleo_publico')

# Mismo total, desagregado por función permanente o transitoria.
tipo_funcion <- bind_rows(lapply(seq_along(c(2,3,4)),function(i) {
  r <- c(2,3,4)[i]
  data.frame(anio=2021:2025,tipo=c('Total Sector Público','Permanente','Transitorio')[i],cargos=num(e,r,2:6))
}))
funcion_ancha <- tipo_funcion |> pivot_wider(names_from=tipo,values_from=cargos)
stopifnot(all(funcion_ancha$`Total Sector Público`==funcion_ancha$Permanente+funcion_ancha$Transitorio),
          all(tipo_funcion$cargos>=0),!anyDuplicated(tipo_funcion[c('anio','tipo')]))
guardar(tipo_funcion,'empleo_publico_funcion')
# FONASA: sumar conteos publicados, no contar filas como personas.
# 2018-2020 se auditan pero no se incorporan al gráfico por claves repetidas.
archivos <- list.files('02_salud_fonasa',pattern='[.]csv$',full.names=TRUE)
fonasa <- list(); calidad <- list()
for(f in archivos) {
  d <- read.csv(f,check.names=FALSE,colClasses='character')
  n <- ncol(d); v <- as.numeric(d[[n]])
  periodo <- unique(d[[1]]); stopifnot(length(periodo)==1,all(is.finite(v)),all(v>=0),all(v==floor(v)))
  anio <- as.integer(substr(periodo,1,4)); dup <- sum(duplicated(d[-n]))
  calidad[[length(calidad)+1]] <- data.frame(archivo=basename(f),anio=anio,filas=nrow(d),claves_repetidas=dup,beneficiarios=sum(v),publicado_en_grafico=anio>=2021)
  if(anio>=2021) {
    stopifnot(dup==0)
    tr <- which(names(d)%in%c('TRAMO','TRAMO_FONASA')); stopifnot(length(tr)==1)
    a <- aggregate(v,list(tramo=d[[tr]]),sum);names(a)[2]<-'beneficiarios';a$anio<-anio
    stopifnot(setequal(a$tramo,c('A','B','C','D')),sum(a$beneficiarios)==sum(v))
    fonasa[[length(fonasa)+1]]<-a
  }
}
guardar(bind_rows(calidad) |> arrange(anio),'auditoria_fonasa')
guardar(bind_rows(fonasa) |> select(anio,tramo,beneficiarios) |> arrange(anio,tramo),'fonasa_tramos')

# ISAPRES: cartera y movimientos anuales desde el libro aportado al proyecto.
f <- '02_salud_fonasa/cartera_isapres.xlsx'
d <- read_excel(f,sheet='Cartera Anual de Isapre',col_names=FALSE,.name_repair='minimal')
stopifnot(d[[1]][5]=='Isapres',d[[1]][8]=='Total Sistema')
anios <- as.integer(unlist(d[5,2:37]))
isapres_cartera <- data.frame(anio=anios,
  abiertas=as.numeric(unlist(d[6,2:37])),
  cerradas=as.numeric(unlist(d[7,2:37])),
  total=as.numeric(unlist(d[8,2:37])))
stopifnot(nrow(isapres_cartera)==36,!anyDuplicated(isapres_cartera$anio),
  all(isapres_cartera$total==isapres_cartera$abiertas+isapres_cartera$cerradas),
  all(isapres_cartera$total>0))
guardar(isapres_cartera,'isapres_cartera')
d <- read_excel(f,sheet='Suscripción y Desahucios',col_names=FALSE,.name_repair='minimal')
stopifnot(d[[1]][5]=='Contratos',d[[1]][6]=='Suscripciones',
  d[[1]][7]=='Desahucios',d[[1]][8]=='Incremento neto')
anios <- as.integer(gsub('[^0-9]','',unlist(d[5,2:36])))
isapres_movimientos <- data.frame(anio=anios,
  suscripciones=as.numeric(unlist(d[6,2:36])),
  desahucios=as.numeric(unlist(d[7,2:36])),
  incremento_neto=as.numeric(unlist(d[8,2:36])))
stopifnot(nrow(isapres_movimientos)==35,!anyDuplicated(isapres_movimientos$anio),
  all(isapres_movimientos$incremento_neto==isapres_movimientos$suscripciones-isapres_movimientos$desahucios))
guardar(isapres_movimientos,'isapres_movimientos')

# SII: valores almacenados como fracción del PIB, convertidos a porcentaje.
f <- list.files('03_tributacion/datos_originales/sii_pib',pattern='xlsx$',full.names=TRUE)
stopifnot(length(f)==1)
s <- read_excel(f,sheet='Consolidados',col_names=FALSE,.name_repair='minimal')
stopifnot(s[[1]][246]=='TOTAL INGRESOS TRIBUTARIOS',grepl('VALOR AGREGADO',s[[2]][97]))
tributos <- data.frame(
  anio = 2009:2025,
  total = num(s,246,7:23)*100,
  renta = num(s,5,7:23)*100,
  primera_categoria = num(s,7,7:23)*100,
  iva = num(s,97,7:23)*100
)
# Componentes construidos desde los totales publicados por el SII.
tributos$otros_renta <- tributos$renta-tributos$primera_categoria
tributos$otros_impuestos <- tributos$total-tributos$renta-tributos$iva
tributos$primera_categoria_pct_renta <- 100*tributos$primera_categoria/tributos$renta
# Se conserva este nombre para no romper análisis anteriores que usan "otros".
tributos$otros <- tributos$otros_impuestos
stopifnot(
  all(tributos$primera_categoria >= 0), all(tributos$otros_renta >= 0),
  all(tributos$otros_impuestos >= 0),
  all(abs(tributos$total-(tributos$iva+tributos$primera_categoria+
    tributos$otros_renta+tributos$otros_impuestos)) < 1e-8),
  all(tributos$primera_categoria_pct_renta >= 0 & tributos$primera_categoria_pct_renta <= 100),
  all(tributos$total < 40), all(tributos$total > 0)
)
guardar(tributos,'tributacion')

# INE: estimaciones anuales (no promedio artesanal de trimestres superpuestos).
f <- '04_empleo_formal/datos_originales/ene_anual.xlsx'
ene <- bind_rows(lapply(c(AS='Total',H='Hombres',M='Mujeres'),function(sexo) {
  hoja <- names(c(AS='Total',H='Hombres',M='Mujeres'))[match(sexo,c('Total','Hombres','Mujeres'))]
  d <- read_excel(f,sheet=hoja,col_names=FALSE,.name_repair='minimal')
  filas <- which(suppressWarnings(as.numeric(d[[1]]))%in%2010:2025)
  data.frame(anio=as.integer(d[[1]][filas]),sexo=sexo,ocupados_miles=as.numeric(d[[7]][filas]),desocupacion_pct=as.numeric(d[[23]][filas]),ocupacion_pct=as.numeric(d[[25]][filas]),participacion_pct=as.numeric(d[[27]][filas]))
}))
stopifnot(nrow(ene)==48,all(ene$ocupacion_pct>=0 & ene$ocupacion_pct<=100),!anyDuplicated(ene[c('anio','sexo')]))
guardar(ene,'empleo_ene')

# INE: mismas estimaciones anuales, hojas AP a MA, una por región.
regiones_ene <- c(
  AP='Arica y Parinacota', TA='Tarapacá', AN='Antofagasta', AT='Atacama', CO='Coquimbo',
  VA='Valparaíso', RM='Metropolitana', LI="O'Higgins", ML='Maule', NB='Ñuble', BI='Biobío',
  AR='La Araucanía', LR='Los Ríos', LL='Los Lagos', AI='Aysén', MA='Magallanes'
)
macrozonas_ene <- c(
  AP='Norte', TA='Norte', AN='Norte', AT='Norte', CO='Norte',
  VA='Centro', RM='Centro', LI='Centro', ML='Centro', NB='Centro', BI='Centro',
  AR='Sur', LR='Sur', LL='Sur', AI='Sur', MA='Sur'
)
ene_regional <- bind_rows(lapply(names(regiones_ene),function(hoja) {
  d <- read_excel(f,sheet=hoja,col_names=FALSE,.name_repair='minimal')
  filas <- which(suppressWarnings(as.numeric(d[[1]])) %in% 2010:2025)
  data.frame(anio=as.integer(d[[1]][filas]),region=unname(regiones_ene[[hoja]]),
             macrozona=unname(macrozonas_ene[[hoja]]),
             desocupacion_pct=as.numeric(d[[23]][filas]),
             ocupacion_pct=as.numeric(d[[25]][filas]))
}))
stopifnot(nrow(ene_regional)==256, !anyDuplicated(ene_regional[c('anio','region')]),
          all(ene_regional$ocupacion_pct >= 0 & ene_regional$ocupacion_pct <= 100),
          all(ene_regional$desocupacion_pct >= 0 & ene_regional$desocupacion_pct <= 100))
guardar(ene_regional,'empleo_ene_regional')

# DIPRES: totales incluyen operaciones de capital; no confundir con gasto corriente.
d <- read_excel('05_finanzas_publicas/datos_originales/dipres_gct_pib.xlsx',col_names=FALSE,.name_repair='minimal')
stopifnot(d[[1]][34]=='TOTAL INGRESOS',d[[1]][35]=='TOTAL GASTOS',d[[1]][36]=='PRESTAMO NETO/ENDEUDAMIENTO NETO')
pesos_2025 <- read_excel('05_finanzas_publicas/datos_originales/dipres_gct_pesos_2025.xlsx',col_names=FALSE,.name_repair='minimal')
stopifnot(pesos_2025[[1]][34]=='TOTAL INGRESOS',pesos_2025[[1]][35]=='TOTAL GASTOS',pesos_2025[[1]][36]=='PRESTAMO NETO/ENDEUDAMIENTO NETO')
fiscal <- data.frame(
  anio=1990:2025,
  ingresos_pct_pib=num(d,34,2:37),
  gastos_pct_pib=num(d,35,2:37),
  balance_pct_pib=num(d,36,2:37),
  ingresos_millones_pesos_2025=num(pesos_2025,34,2:37),
  gastos_millones_pesos_2025=num(pesos_2025,35,2:37),
  balance_millones_pesos_2025=num(pesos_2025,36,2:37)
)
stopifnot(
  max(abs(fiscal$ingresos_pct_pib-fiscal$gastos_pct_pib-fiscal$balance_pct_pib))<0.00001,
  max(abs(fiscal$ingresos_millones_pesos_2025-fiscal$gastos_millones_pesos_2025-fiscal$balance_millones_pesos_2025))<0.01
)
guardar(fiscal,'finanzas_publicas')

# MINVU: tabla estadística completa; la capa geográfica no reconcilia el total.
d <- read_excel('06_vivienda/datos_originales/minvu_censos.xlsx',sheet='2024 2002 Completa',col_names=FALSE,.name_repair='minimal')
stopifnot(d[[1]][8]=='Total País')
v <- data.frame(region=d[[1]][10:25],hogares=as.numeric(d[[7]][10:25]),deficit=as.numeric(d[[21]][10:25]))
stopifnot(nrow(v)==16,!anyDuplicated(v$region),all(v$deficit>=0),all(v$hogares>0),sum(v$deficit)==as.numeric(d[[21]][8]),sum(v$hogares)==as.numeric(d[[7]][8]))
v$deficit_por_100_hogares<-100*v$deficit/v$hogares
guardar(v,'vivienda_regiones')

# Banco Central: tabla HTML oficial guardada, relación FBCF/PIB a precios corrientes.
b <- read_html('07_inversion/datos_originales/bcentral_inversion.html') |> html_table(fill=TRUE)
stopifnot(length(b)==1)
b<-b[[1]]; fila<-which(grepl('fijo',b$Serie,ignore.case=TRUE)&grepl('corrientes',b$Serie,ignore.case=TRUE))
stopifnot(length(fila)==1)
cols<-which(grepl('^[0-9]{4}$',names(b)))
inversion<-data.frame(anio=as.integer(names(b)[cols]),fbcf_pct_pib=as.numeric(gsub(',','.',unlist(b[fila,cols]))))
stopifnot(all(is.finite(inversion$fbcf_pct_pib)),all(inversion$fbcf_pct_pib>0))
guardar(inversion,'inversion')
writeLines(capture.output(sessionInfo()),'datos/sesion_R.txt')
cat('Validación completada. Déficit nacional conciliado MINVU:',sum(v$deficit),'\n')

# Series censales comparables de vivienda (metodología abreviada).
source("scripts/procesar_vivienda.R", encoding="UTF-8")
procesar_vivienda()
