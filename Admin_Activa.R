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


tk<-(read.xlsx("C:/Users/javi__000/Documents/ITESO/Trading/Laboratorio2/IAK.xlsx", sheetName="Holdings",
               colIndex=1,startRow=10,endRow=66,header=TRUE))
#tk<- read_excel("C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab2_KarlaPerez/Etf_data.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

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


Precios<-Precios[,(names(Precios) %in% nuevos)]
row.names(Precios)<-Datos[[1]]$date

#reasignar
tk_completos <- as.character(tkt_1[completos])
colnames(Precios) <- tk_completos

###### Parte 2
  
Historico<-data.frame("Date" = row.names(Precios),
                      "Precios" = Precios[,1],
                      "R_Precio" = 0,
                      "R_Activo" = 0,
                      "R_Cuenta" = 0,
                      "Capital" = 0,
                      "Balance" = 0,"Titulos" = 0 ,"Titulos_a" = 0,
                      "Operacion" = NA, "Comisiones" = 0, "Mensaje" = NA)

#Date fecha
#Precio del activo 
#R_Precio, rendimiento diario del precio(diario, de un dia a otro)
#R_activo, rendimiento acumulado del precio 
#Balance, valor del portafolio
#titulos  acciones que se tienen 
#titulos_a titulos acumulados
#R_cuenta, balance+ capital 
#operaciones, indica si son de compra o de venta(1 compra, 0 mantengo, -1 venta)
#Comisiones 0.0025  por el valor de la transacción
# Mensaje, Texto que indique la decisión o  que informe algo ocurrido

Regla0_R <- -0.03 # Considera una oportunidad de compra  cuando el activo empiece a bajar su rendimiento de -3% o menor 
Regla1_I <- 0.2 # Porcentaje de capital que se utiliza para la operación inicial 
Regla2_P <- 0.25 # Se utiliza  el =% del L capital restante en cada compra
Regla3_W <- tk_completos # se realiza la misma estrategia para todos los activos en el portafolio 
Regla4_C <- -0.0025 #comisiones pagadas
Regla5_K <- 1000000 #Capital inicial 


# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #
# -- ----------------------------------------------------------------------------------------- -- #

# -- Calcular los Titulos de posicion inicial
Historico$Titulos[1] <- (Regla5_K*Regla1_I)%/%Historico$Precio[1]

# -- Se calculan comisiones iniciales
Historico$Comisiones[1] <- Historico$Titulos[1]*Historico$Precio[1]*Regla4_C

# -- Calcular el Balance
Historico$Balance[1] <- Historico$Titulos[1]*Historico$Precio[1]

# -- Todo remanente se dejar? registrado en la cuenta de efectivo.
Historico$Capital[1] <- Regla5_K-Historico$Balance[1]-Historico$Comisiones[1]

# -- Iniciamos con una postura de mantener.
Historico$Operacion[1] <- "Posicion Inicial"

# -- El rendimiento de capital en el tiempo 1 es 0
Historico$R_Cuenta[1] <- 0

# -- Mensaje inicial
Historico$Mensaje[1] <- "Inicializacion de cartera"

# -- Calcular R_Precio
Historico$R_Precio <- round(c(0, diff(log(Historico$Precio))),4)

# -- Calcular R_Activo
for(i in 1:length(Historico$Date)){
  Historico$R_Activo[i] <- round((Historico$Precio[i]/Historico$Precio[1])-1,2)
}

# -- ------------------------------------ -- #
# -- ------------------------------------ -- #
# -- ------------------------------------ -- #

for(i in 2:length(Historico$Date)){
  
  if(Historico$R_Precio[i] <= Regla0_R){ # Generar Se?al
    
    # Establecer capital actual, inicialmente, igual al capital anterior
    Historico$Capital[i] <- Historico$Capital[i-1]
    
    if(Historico$Capital[i] > 0){ # Si hay capital
      
      if(Historico$Capital[i]*Regla2_P > Historico$Precio[i]){ # Si Capital minimo
        
        Historico$Operacion[i] <- "Compra"
        Historico$Titulos[i]   <- (Historico$Capital[i]*Regla2_P)%/%Historico$Precio[i]
        
        compra <- Historico$Precio[i]*Historico$Titulos[i]  
        Historico$Comisiones[i] <- compra*Regla4_C
        
        Historico$Titulos_a[i] <- Historico$Titulos[i-1]+Historico$Titulos[i]
        
        
      }
      
    }
    else { # No hubo capital
      
      
    }
    
    
  }
  else { # Sin se?al
    
  }
  
}










