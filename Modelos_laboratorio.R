# Importamos librerias 
library(data.table)
library(plyr)
library(dplyr)
library(class)
library(caret)
library(zoo)
library(forecast)
library(pscl)

# GESTIÓN DE MEMORIA - habilitar estas lineas en caso de problemas de rendimiento en ordenadors con RAM
# gc(Ft)
# memory.limit(size = 4000)


# CARGA DATOS ------------------------------------------------------------------------------------------------

# Importamos los datos de http://estadisticas.tourspain.es/es-es/estadisticas/frontur/microdatos/paginas/default.aspx
Ft <- read.table('data/Frontur2015.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")

#Damos formato básico
colnames(Ft)[colnames(Ft)=="AÑO"] <- "ANO"
Ft$VIAJEROS <- as.numeric(Ft$VIAJEROS)



#-------------------------------------------------------------------------------------------
# ESTUDIO DE TODO EL HISTÓRICO DE LA SERIE LINEAL DE FROUNTUR


# Cargamos el histórico -----------------

  Ft_serie <- Ft[c(2,3,15)]
  
  #Cargamos datos de los dos años anteriores
  # Importamos los datos de http://estadisticas.tourspain.es/es-es/estadisticas/frontur/microdatos/paginas/default.aspx
  Ft2014 <- read.table('data/Frontur2014.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  colnames(Ft2014)[colnames(Ft2014)=="VIA.ENTRADA"] <- "ENTRADA"
  colnames(Ft2014)[colnames(Ft2014)=="AÑO"] <- "ANO"
  Ft2014$VIAJEROS<-as.numeric(Ft2014$VIAJEROS)
  Ft_serie <- rbind(Ft_serie,Ft2014[,c(2,3,15)])
  rm(Ft2014)
  
  Ft2013 <- read.table('data/Frontur2013.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  colnames(Ft2013)[colnames(Ft2013)=="VIA.ENTRADA"] <- "ENTRADA"
  colnames(Ft2013)[colnames(Ft2013)=="AÑO"] <- "ANO"
  Ft2013$VIAJEROS<-as.numeric(Ft2013$VIAJEROS)
  Ft_serie <- rbind(Ft_serie,Ft2013[,c(2,3,15)])
  rm(Ft2013)


# Damos formato al set de datos ---------------

  Ft_serie$FECHA <- as.Date(paste("01", as.numeric(Ft_serie$MES), as.numeric(Ft_serie$ANO), sep="-"),
                      format = "%d-%m-%Y")
  Ft_serie$ANO <- NULL
  Ft_serie$MES <- NULL
  Ft_serie <- Ft_serie[, c(2,1)]
  Ft.dt_ser <- data.table(Ft_serie)
  Ft.dt_ser <- Ft.dt_ser[, list(VIAJEROS = sum(VIAJEROS)), by = FECHA]


# Analizamos la serie --------------------------------------------------

  ggplot(Ft.dt_ser, aes(x = FECHA, y = VIAJEROS)) +  geom_smooth() 
  Ft.dt_ser <- Ft.dt_ser[order(FECHA)]
  ts = ts(Ft.dt_ser$VIAJEROS, frequency=12, start=c(2013,1), end=c(2015,9))
  ts
  plot(ts)
  fit = stl(ts, s.window="periodic")
  plot(fit)
  
  # Otra forma de pintarlo
  #  Ft_ser_components <- decompose(ts)
  #  plot(Ft_ser_components)
  
  # Observaciones
  #distiguimos como las visitas tienen un fuerte su componente cíclico 
  #y observamos como su tendencia es creciente

  

# Establecemos un modelo predictivo sobre la tendencia 2016 ------------

  #por holtwinters
  ts_hw <- HoltWinters(ts)
  ts_hw
  plot(ts_hw)
  #observamos como la linea predictiva ajusta bien al real
  tsforecasts <- forecast.HoltWinters(ts_hw, h=26)
  plot.forecast(tsforecasts)
  #alargamos la predición a dos años vista para ver que estimación tendríamos para 2016 y 2017
  rm(Ft_serie)



#-------------------------------------------------------------------------------------------
# MODELOS PREDICTIVOS 2015

# Damos formato al set de datos ---------------

  # Subdividimos la variable Destino por CC_AA y Provincia. (Ver PDF del diseño de los datos. pg.5)
  Ft$CC_AA<-substr(Ft$DESTINO, 1, 2)
  Ft$PROVINCIA<-substr(Ft$DESTINO, 3, 4)
  Ft$DESTINO <- NULL
  
  # Quitamos los valores no definidos
  Ft<-Ft[!grepl("999",Ft$PERNOCTACIONES),]
  Ft<-Ft[!grepl("9",Ft$PAQUETE),]
  
  # Modificamos los nombres de las columnas
  colnames(Ft)[colnames(Ft)=="DESTINO.PRINCIPAL"] <- "DESTINO"
  colnames(Ft)[colnames(Ft)=="VIAS.ACCESO"] <- "ACCESO"
  colnames(Ft)[colnames(Ft)=="PAIS.RESIDENCIAS"] <- "PAIS"
  colnames(Ft)[colnames(Ft)=="TIPO.TRANSPORTE"] <- "TRANSPORTE"
  colnames(Ft)[colnames(Ft)=="VIA.ENTRADA"] <- "ENTRADA"
  colnames(Ft)[colnames(Ft)=="AÑO"] <- "ANO"
  
  # Eliminamos las columnas que no interesan
  Ft$Id <- NULL
  
  # Damos formato
  Ft$CC_AA<-as.factor(Ft$CC_AA)
  Ft$PROVINCIA<-as.factor(Ft$PROVINCIA)
  Ft$PAIS <- as.factor(Ft$PAIS)
  Ft$ACCESO <- as.factor(Ft$ACCESO)
  Ft$ALOJAMIENTOS <- as.factor(Ft$ALOJAMIENTOS)
  Ft$MOTIVOS <- as.factor(Ft$MOTIVOS)
  Ft$PAQUETE <- as.factor(Ft$PAQUETE)
  Ft$PERNOCTACIONES <- as.factor(Ft$PERNOCTACIONES)
  Ft$TRANSPORTE <- as.factor(Ft$TRANSPORTE)
  Ft$TIPO.VIAJERO <- as.factor(Ft$TIPO.VIAJERO)
  Ft$DONANTE <- as.factor(Ft$DONANTE)
  Ft$ENTRADA <- as.factor(Ft$ENTRADA)

  #Damos formato a la fecha
  Ft$FECHA <- as.Date(paste("01", as.numeric(Ft$MES), as.numeric(Ft$ANO), sep="-"),
                      format = "%d-%m-%Y")
  Ft$FECHA <- as.yearqtr(Ft$FECHA, format = "%d-%m-%Y")
  Ft$ANO <- NULL
  Ft$MES <- NULL
  
  # Reordenamos la tabla
  str(Ft)
  Ft$DESTINO <- NULL
  Ft <- Ft[, c(14,12,13,1:11)]
  Ft.dt <- data.table(Ft)
  
  #Quitamos variables categóricas con sólo dos niveles. 
  Ft.dt$DONANTE <- NULL
  Ft.dt$PAQUETE <- NULL
  
  #Sumamos 
  # Ft <- ddply(Ft, .( FECHA, PAIS, ACCESO, ALOJAMIENTOS, MOTIVOS, PERNOCTACIONES, TRANSPORTE, TIPO.VIAJERO ), summarize, VIAJEROS = sum(VIAJEROS)) #sumamos 
  Ft.dt2 <- Ft.dt[, list(VIAJEROS = sum(VIAJEROS)), by = c("FECHA","CC_AA","PROVINCIA", "PAIS", "ACCESO", "ALOJAMIENTOS", "MOTIVOS",
                                                           "PERNOCTACIONES", "TRANSPORTE", "TIPO.VIAJERO", "ENTRADA")]
  # rm(Ft,Ft.dt)
  str(Ft.dt2)


#Analizamos el set de datos -----------

  #chequeamos si hay algún dato nulo
  sapply(Ft.dt2,function(x) sum(is.na(x)))

  library(Amelia)
  missmap(Ft.dt2, main = "Missing values vs observed")


#-------------------------------------------------------------------------------------------
# MODELO LOGÍSTICO PARA PREDICCIÓN DE LA COMUNIDAD DE DESTINO

  # Se prentende obtener un modelo que dependiendo de las circunstancias de como viaje a España 
  # se pueda estimar a que comunidad de España irá.  
  

  FT.modelo2 <- Ft.dt2[,c(1,2,5,6,8), with=FALSE]
  str(FT.modelo2)
  # banco de pruebas para determinar que variables poseen mayor signifacion para el modelo  
  test =  FT.modelo2[1:1000,]
  modelo.glm.sample <- glm(CC_AA ~.,family=binomial(link='logit'),data=test)
  summary(modelo.glm.sample)
  anova(modelo.glm.sample, test="Chisq")
  
  #Creamos el modelo
  Ft.sample <- sample(1:nrow(FT.modelo2), 0.8 * nrow(FT.modelo2))
  Ft.train2 <- FT.modelo2[Ft.sample,]
  Ft.test2 <- FT.modelo2[-Ft.sample,]

  modelo.glm <- glm(CC_AA ~.,family=binomial(link='logit'),data=Ft.train2)
  summary(modelo.glm)

  # Una forma de ver cuan bueno es el modelo por el coefeciente McFadden 
  pR2(modelo.glm)

  #Testamos el modelo
  #Creamos un set de precciones
  predict.glm <- predict(modelo.glm, Ft.test2, interval="predict")
  predict.glm <-round(predict.glm)
  
  #Comparamos lo que predice el modelo del dato real y observamos suprecisión 
  misClasificError <- mean(predict.glm != Ft.test2$CC_AA)
  print(paste('Accuracy',1-misClasificError))
  
  # El modelo no funciona. 
  
  rm(FT.modelo2,Ft.train2, Ft.test2)
  
  
  
