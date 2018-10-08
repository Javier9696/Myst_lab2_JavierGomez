### xNriyB3ufAxdRmZgpq_p  ##
#ab1

#limpiar el enviroment
rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)

###librerias
suppressMessages(library(plyr))
suppressMessages(library(plotly)) # 
suppressMessages(library(Quandl)) # Descarga de Precios
suppressMessages(library(PortfolioAnalytics)) ##Teoria modenar de portafolio
suppressMessages(library(ROI)) # optimización para ele portafolio 
suppressMessages(library(knitr))  
suppressMessages(library(kableExtra)) # Tablas en HTML
suppressMessages(library(xlsx)) # excel
options(knitr.table.format = "html") 

# Cargar el token de QUANDL
Quandl.api_key("dN9QssXxzTxndaqKUQ_i")
df
# Funcion para descagar precios
Bajar_Precios <- function(Columns, Tickers, Fecha_In, Fecha_Fn) {
  
  # Peticion para descargar precios
  Datos <- Quandl.datatable("WIKI/PRICES", qopts.columns=Columns, ticker=Tickers,
                            date.gte=Fecha_In, date.lte=Fecha_Fn)
  return(Datos)
}


tk<-(read.xlsx("C:/Users/javi__000/Documents/ITESO/Trading/Laboratorio2/IVV.xlsx", sheetName="Holdings",
               colIndex=1,startRow=9,endRow=65,header=TRUE))

tkt_1<-as.character(tk[,1])


cs <- c("date", "adj_close")
Capital_Inicial <- 10000
fs = c("2016-01-20","2018-01-20") # por la c es vector de caracteres 

# Descargar Precios y Calcular rendimientos
Datos <- list()

for(i in 1:length(tkt_1))
  Datos[[i]] <- Bajar_Precios(Columns=cs, Ticker=tkt_1[i], Fecha_In=fs[1], Fecha_Fn=fs[2])

names(Datos) <- tkt_1
# ordenamos las fecha para que esten de pasadas a actuales
for (i in 1: length(tkt_1)){
  Datos[[i]] <- Datos[[i]][order(Datos[[i]][,1]),]
}



##-------------------
longitudes<-c()

for (i in 1:length(Datos)){
  longitudes[i]<-length(Datos[[i]]$date)
}

#obtener la freq. de las longitudes que mas se repiten
longs <- count(longitudes)
#encontrar la que mas se repite
l <- longs[which.max(longs$freq),1]
#que todos esten parejos
completos<-which(longitudes== l)
#tener la lista de activos que tienen la misma cantidad de precios
DatosN <-Datos[completos]
#Vector  para almacenar las columnas
columnas<-c()
nuevos<-c()

#función para repetir la función 
Precios<-do.call(cbind,DatosN)

#Creamos el vector de columnas con los nombre
for (i in 1:length(tkt_1)){
  nuevos[i]<-paste(tkt_1[i],".adj_close",sep = "")
}


#extraemos 1 renglon poara obtner los nombres de las columnas
nombres<-colnames(Precios[1,(names(Precios) %in% nuevos)])

Precios <- Precios[,(names(Precios) %in% nuevos)]
row.names(Precios) <- DatosN[[1]]$date

# Reasignar nombres al data.frame
tk_completos <- as.character(tk[completos])
colnames(Precios) <- tk_completos 


Historico <- data.frame("Date"= row.names(Precios),
                        "Precio"= Precios[,1],
                        "R_Precio" = 0,
                        "R_Activo" = 0,
                        "R_Cuenta" = 0,
                        "Capital" = 0,"Flotante" = 0, "Balance" = 0, "Titulos" = 0,
                        "Titulos_a" = 0,
                        "Operacion" = NA, "Comisiones"= 0, "Comisiones_a"= 0, "Mensaje" = NA)

#Date       : Fecha (proviene desde los precios que bajaron)
#Precio     : Precio individual del activo
#R_Precio   : Rendimiento diario del precio (dia a dia)
#R_Activo   : Rendimiento acumulado del precio (Cada dia respecto al precio inicial)
#Capital    : El dinero no invertido (equivalente a efectivo)
#Flotante   : El valor de la posicion(precio diario x titulos acumulados)
#Balance    : Capital + Flotante
#R_Cuenta   : Balance + Capital (cada dia respecto al capital inicial)
#Titulos    : Acciones que se tienen
#Titulos_a  : Titulos acumulados
#Operacion  : Indicativo de compra (1), mantener (0), venta (-1)
#Comisiones : 0.0025 o 0.25% por el valor de la transaccion
#Mensaje    : Un texto que indique alguna decision o indicativo de que ocurrio algo

Regla0_R <- -0.03  # Considerar una oportunidad de compra en un rendimiento de -3% o menor
Regla1_I <- 0.20   # % de capital para comprar titulos para posicion inicial
Regla2_P <- 0.25   # Se utiliza el P% del L capital restante en cada compra
Regla3_W <- tk_completos # Se realiza la misma estrategia para todos los activos en el portafolio
Regla4_C <- 0.0025 # Comisiones pagadas por compra
Regla5_K <- 100000 # Capital inicial

#####
#Calcular los titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]

#Calcular los titulos acumulados
Historico$Titulos_a[1] <- Historico$Titulos[1]

#Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C

#Calcular las comisiones acumulados
Historico$Comisiones_a[1] <- Historico$Comisiones[1]

#Calcular flotante
Historico$Flotante[1]<- Historico$Titulos_a[1]*Historico$Precio[1]

#Todo remanente se deja registrado en la cuenta de efectivo
Historico$Capital[1] <- Regla5_K-Historico$Flotante[1]-Historico$Comisiones[1]

#Calcular el Balance
Historico$Balance[1] <- Historico$Flotante[1]+ Historico$Capital[1]

#Iniciamos con una postura de mantener
Historico$Operacion[1] <- 1 #"Posicion Inicial"

#El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

#Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

#Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)

#Calcular R_Activo

PosturaInicial <- Regla5_K%/%Historico$Precio[1]

for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- (PosturaInicial*Historico$Precio[i])/(PosturaInicial*Historico$Precio[1])-1
}



for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i]  <= Regla0_R){ # Generar Senal
    
    
    if(Historico$Capital[i-1] > 0){ # Si hay capital
      
      if(Historico$Capital[i-1]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
        
        Historico$Operacion[i] <- 1
        
        Historico$Titulos[i]   <- (Historico$Capital[i-1]*Regla2_P)%/%Historico$Precio[i]
        
        Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]+Historico$Titulos[i]
        
        Historico$Comisiones[i] <- Historico$Precio[i]*Historico$Titulos[i]*Regla4_C
        
        Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1] + Historico$Comisiones[i]
        
        Historico$Flotante[i]<- Historico$Precio[i]*Historico$Titulos_a[i]
        
        Historico$Capital[i] <- Historico$Capital[i-1] - Historico$Titulos[i]*Historico$Precio[i] -Historico$Comisiones ###
        
        Historico$Balance [i]<- Historico$Capital[i] + Historico$Flotante[i]
        
        Historico$R_Cuenta[i] <- Historico$Balance[i]/Regla5_K -1
        
        Historico$Mensaje[i] <- "Señal de compra ejecutada"
      }
    } ##
    else { # No hubo capital minimo
      Historico$Mensaje[i] <- "No hay capital para la operación"
      Historico$Operacion[i] <- 0
      Historico$Titulos [i] <- 0
      Historico$Comisiones[i] <- 0
      Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]
      Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]
      Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
      Historico$Capital[i] <- Historico$Capital[i-1]
      Historico$Balance[i] <- Historico$Flotante[i] + Historico$Capital[i]
      Historico$R_Cuenta[i] <- Historico$Balance[i]/Regla5_K - 1
    }
    
  }
  else { # Sin senal
    Historico$Mensaje[i] <- "No hay señal"
    Historico$Operacion[i] <- 0
    Historico$Titulos [i] <- 0
    Historico$Comisiones[i] <- 0
    Historico$Comisiones_a[i] <- Historico$Comisiones_a[i-1]
    Historico$Titulos_a[i] <- Historico$Titulos_a[i-1]
    Historico$Flotante[i] <- Historico$Titulos_a[i]*Historico$Precio[i]
    Historico$Capital[i] <- Historico$Capital[i-1]
    Historico$Balance[i] <- Historico$Flotante[i] + Historico$Capital[i]
    Historico$R_Cuenta[i] <- Historico$Balance[i]/Regla5_K - 1
  }
}

###### plotly
plot_ly(Historico) %>%
  add_trace(x = ~Date, y = ~round(R_Activo,4), type = 'scatter', mode = 'lines', name = 'Activo',
            line = list(color = 'red')) %>%
  add_trace(x = ~Date, y = ~round(R_Cuenta,4), type = 'scatter', mode = 'lines', name = 'Cuenta',
            line = list(color = 'blue')) %>% 
  layout(title = "Rend del activo VS Rend de la cuenta",
         xaxis = list(title = "Fechas", showgrid = T),
         yaxis = list(title = "Rendimiento"), 
         legend = list(orientation = 'h', y = -0.25, x = 0.5))


#Sharpe y Sortino para R_activo y R_cuenta

SharpeRatio(R=xts(x = Historico$R_Activo, order.by = as.Date(Historico$Date)),Rf =0.0215, FUN ="StdDev" )
SortinoRatio(R =xts(x = Historico$R_Activo,order.by = as.Date(Historico$Date)),MAR =0.0215)

SharpeRatio(R=xts(x = Historico$R_Cuenta,order.by = as.Date(Historico$Date)),Rf =0.0215, FUN ="StdDev" )
SortinoRatio(R =xts(x = Historico$R_Cuenta,order.by = as.Date(Historico$Date)),MAR =0.0215)
SDA <- sd(Historico$R_Precio)
MeanA <- mean(Historico$R_Precio)
RFA <- .0215
SharpeR <- (MeanA - RFA)/ SDA
SORTINO <- (MeanA - RFA) / sd(Historico$R_Precio[Historico$R_Precio < 0])
SDA1 <- sd(Historico$R_Cuenta)
MeanA1 <- mean(Historico$R_Cuenta)
SharpeR1 <- (MeanA1 - RFA)/ SDA1
SORTINO1 <- (MeanA1 - RFA) / sd(Historico$R_Cuenta[Historico$R_Cuenta < 0])