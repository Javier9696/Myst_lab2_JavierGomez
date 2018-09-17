### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

###librerias
suppressMessages(library(plotly)) # 
suppressMessages(library(Quandl)) # Descarga de Precios
suppressMessages(library(PortfolioAnalytics)) ##Teoria modenar de portafolio
suppressMessages(library(ROI)) # optimización para ele portafolio 

suppressMessages(library(knitr))  
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("xNriyB3ufAxdRmZgpq_p")
df
# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}
tk<-(read.xlsx(file="C:/Users/if698353/Documents/R/IAK.xlsx", sheetName="Holdings",
                            colIndex=1,startRow=10,endRow=54,header=FALSE))

tkt_1<-as.character(tk[,1])


cs <- c("date", "adj_close")
Capital_Inicial <- 10000
fs = c("2016-09-01","2018-09-01") # por la c es vector de caracteres 

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tkt_1))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tkt_1[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tkt_1
##-------------------
longitudes<-c()

for (i in 1:length(Datos)){
  longitudes[i]<-length(Datos[[i]]$date)
}

maximo<-max(longitudes)

completos<-which(longitudes==maximo)

DatosN <-Datos[completos]
#vector para almancear columas de interes
columnas<-c()
nuevos<-c()
#funcion para repetir una funcion por cada columna del data.frame
Precios <- do.call(cbind, DatosN)
#crear vector con nombres de columnas de interes = "NOMBREACTIVO.ADJ_CLOSE_R"
for(i in 1:length(tk)){
  nuevos[i]<- paste(tk[i], ".adj_close", sep="")
}
#Extraer 1 renglon para obtener los nombres de las columnas
nombres <- colnames(Precios[1,(names(Precios) %in% nuevos)])
#elejir una olumna date y las demas columnas de rendmientos
Precios <- Precios[,(names(Precios) %in% nuevos)]
row.names(Precios) <- DatosN[[1]]$date
#reasignar nombres al data.frame
tk_completos <- as.character(tk[completos])
colnames(Precios) <- tk_completos
# -- ------ -- #


Historico <- data.frame("Date" = row.names(Precios),
                        "Precio" = Precios[,1], 
                        "R_Precio" = 0, 
                        "R_Activo" = 0,
                        "R_Cuenta" = 0, 
                        "Capital" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a" = 0,
                        "Operacion" = NA, "Comisiones" = 0, "Mensaje" = NA)

# *Date*       : Fecha (Proviene desde los precios que bajaron).
# *Precio*     : Precio individual del activo.
# *R_Precio*   : Rendimiento diario del precio (dia a dia).
# *R_Activo*   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial).
# *Capital*    : El dinero no invertido (Equivalente a Efectivo).
# *Balance*    : El valor del portafolio (Precio diario X Titulos).
# *R_Cuenta*   : Balance + Capital (Cada dia respecto al capital inicial).
# *Titulos*    : Acciones que se tienen.
# *Titulos_a*  : Titulos acumulados.
# *Operacion*  : Indicativo de Compra (1), Mantener (0), Venta (-1).
# *Comisiones* : 0.0025 ? 0.25% por el valor de la transacci?n.
# *Mensaje*    : Un texto que indique alguna decisi?n o indicativo de que ocurri? algo.

Regla0_R <- -0.03  # Considerar una oportunidad de compra en un rendimiento de -6% o menor.
Regla1_I <- 0.20   # Porcentaje de capital para comprar titulos para posicion Inicial.
Regla2_P <- 0.25   # Se utiliza el P% del L capital restante en cada compra.
Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio.
Regla4_C <- 0.0025 # Comisiones pagadas por compra.
Regla5_K <- 100000 # Capital Inicial.