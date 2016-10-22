# Importamos librerias 
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)
library(rgdal)
library(RColorBrewer)
library(ggmap)
library(scales)
library(party)
library(randomForest)
library(class)
library(caret)
library(DT)

# CARGA DATOS ------------------------------------------------------------------------------------------------

  # Importamos los datos de http://estadisticas.tourspain.es/es-es/estadisticas/frontur/microdatos/paginas/default.aspx
  Ft <- read.table('data/Frontur2015.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  
  # Modificamos los nombres de las columnas
  colnames(Ft)[colnames(Ft)=="DESTINO.PRINCIPAL"] <- "DESTINO"
  colnames(Ft)[colnames(Ft)=="VIAS.ACCESO"] <- "ACCESO"
  colnames(Ft)[colnames(Ft)=="PAIS.RESIDENCIAS"] <- "PAIS"
  colnames(Ft)[colnames(Ft)=="TIPO.TRANSPORTE"] <- "TRANSPORTE"
  colnames(Ft)[colnames(Ft)=="VIA.ENTRADA"] <- "ENTRADA"
  
  # Subdividimos la variable Destino por CC_AA y Provincia. (Ver PDF del diseño de los datos. pg.5)
  Ft$CC_AA<-substr(Ft$DESTINO, 1, 2)
  Ft$PROVINCIA<-substr(Ft$DESTINO, 3, 4)
  Ft$DESTINO <- NULL
  
  Ft$MES<-as.factor(Ft$MES)
  Ft$VIAJEROS<-as.numeric(Ft$VIAJEROS)
  Ft$ALOJAMIENTOS<-as.numeric(Ft$ALOJAMIENTOS)
  Ft$PERNOCTACIONES<-as.factor(Ft$PERNOCTACIONES)
  Ft$CC_AA<-as.numeric(Ft$CC_AA)
  Ft$Provincia<-as.numeric(Ft$PROVINCIA)
  
  # Eliminamos las columnas que no interesan
   Ft$Id <- NULL
  # Ft$AÑO <- NULL
  
  # Reordenamos la tabla
  Ft <- Ft[, c(1:2,14:15,3:13)]


#---------------------------------------------------------------------------------------------------------------
# SECTOR HOTELERO 
#---------------------------------------------------------------------------------------------------------------

  Ft_hotel<-Ft[grepl("1",Ft$ALOJAMIENTOS),] #Viajeros alojados en hoteles
  Ft_hotel$VIAJEROS <- ceiling(Ft_hotel$VIAJEROS) 


#--------------------------------------------------------------------------------------
# 1. Número de viajeros alojados varias noches en hotel  por Provincia.

  Ft_hotel_viaj <- Ft_hotel[, c(3,4,10,15)]        #Seleccionamos las columnas que necesitamos 
  Ft_hotel_viaj <- ddply(Ft_hotel_viaj, .(CC_AA, PROVINCIA, PERNOCTACIONES), summarize, VIAJEROS = sum(VIAJEROS)) #sumamos 
  Ft_hotel_viaj <- dcast(Ft_hotel_viaj, CC_AA + PROVINCIA ~ PERNOCTACIONES, value =VIAJEROS)    #convertirmos formato corto
  
  colnames(Ft_hotel_viaj)[colnames(Ft_hotel_viaj)==1 ] <- "Ninguna noche"
  colnames(Ft_hotel_viaj)[colnames(Ft_hotel_viaj)==2 ] <- "1 noche"
  colnames(Ft_hotel_viaj)[colnames(Ft_hotel_viaj)==3 ] <- "De 2 a 3 noches"
  colnames(Ft_hotel_viaj)[colnames(Ft_hotel_viaj)==4 ] <- "De 4 a 7 noches"
  colnames(Ft_hotel_viaj)[colnames(Ft_hotel_viaj)==5 ] <- "De 9 a 15 noches"
  colnames(Ft_hotel_viaj)[colnames(Ft_hotel_viaj)==6 ] <- "Mas de 15 noches"
  
  ListCCAA <- read.table('data/ListadoCCAA.csv', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  ListCCAA$CC_AA <- as.numeric(ListCCAA$CC_AA)
  Ft_hotel_viaj$CC_AA <- as.numeric(Ft_hotel_viaj$CC_AA)
  Ft_hotel_viaj <- merge(ListCCAA, Ft_hotel_viaj,by="CC_AA")
  Ft_hotel_viaj$CC_AA <- NULL
  
  ListProv <- read.table('data/ListadoProvincias.csv', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  ListProv$PROVINCIA <- as.numeric(ListProv$PROVINCIA)
  Ft_hotel_viaj$PROVINCIA <-  as.numeric(Ft_hotel_viaj$PROVINCIA)
  Ft_hotel_viaj <- merge(ListProv, Ft_hotel_viaj ,by="PROVINCIA")
  
  Ft_hotel_viaj$PROVINCIA<- NULL

  datatable(Ft_hotel_viaj)

#---------------------------------------------------------------------------------------
# 2-  Nacionalidades que más se alojan en hotel.

  Ft_hotel_ranking <- Ft_hotel[, c(5,10,15)] 
  Ft_hotel_ranking <- dcast(Ft_hotel_ranking, PAIS ~ PERNOCTACIONES)
  ListPaises <- read.table('data/Pais_de_residencia2.csv', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  ListPaises <- ListPaises[!(ListPaises$PAIS == "999"),]
  ListPaises$PAIS <- as.numeric(ListPaises$PAIS)
  Ft_hotel_ranking$PAIS <- as.numeric(Ft_hotel_ranking$PAIS)
  Ft_hotel_ranking <- merge(ListPaises, Ft_hotel_ranking,by="PAIS")
  Ft_hotel_ranking$PAIS <- NULL
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=="Pais_text" ] <- "PAIS"
  
  Ft_hotel_ranking$R2 = rank(-Ft_hotel_ranking$`2`, ties = "first")
  Ft_hotel_ranking$R3 = rank(-Ft_hotel_ranking$`3`, ties = "first")
  Ft_hotel_ranking$R4 = rank(-Ft_hotel_ranking$`4`, ties = "first")
  Ft_hotel_ranking$R5 = rank(-Ft_hotel_ranking$`5`, ties = "first")
  Ft_hotel_ranking$R6 = rank(-Ft_hotel_ranking$`6`, ties = "first")
  
  Ft_hotel_ranking$`2`  <- NULL
  Ft_hotel_ranking$`3`  <- NULL
  Ft_hotel_ranking$`4`  <- NULL
  Ft_hotel_ranking$`5`  <- NULL
  Ft_hotel_ranking$`6`  <- NULL
  
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=='R1'] <- "Ninguna noche"
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=='R2'] <- "1 noche"
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=='R3'] <- "De 2 a 3 noches"
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=='R4'] <- "De 4 a 7 noches"
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=='R5'] <- "De 9 a 15 noches"
  colnames(Ft_hotel_ranking)[colnames(Ft_hotel_ranking)=='R6'] <- "Mas de 15 noches"

  datatable(Ft_hotel_ranking)
  

#---------------------------------------------------------------------------------------
# 3- Ver donde mas se alojan los turistas extranjeros. O hacer ranking de alojamientos 
#por nacionalidad

  Ft_alo <- Ft[,c(1,2,5,7,15)]
  Ft_alo$VIAJEROS <- ceiling(Ft_alo$VIAJEROS) 
  Ft_alo <- ddply(Ft_alo, .(PAIS, ALOJAMIENTOS), summarize, VIAJEROS = sum(VIAJEROS))
  Ft_alo <- dcast(Ft_alo, PAIS ~ ALOJAMIENTOS, value.var = "VIAJEROS")
  ListPaises$PAIS <- as.numeric(ListPaises$PAIS)
  Ft_alo$PAIS <- as.numeric(Ft_alo$PAIS)  
  Ft_alo<- merge(ListPaises,Ft_alo,by="PAIS")
  Ft_alo$PAIS <- NULL
  Ft_alo$`99` <- NULL
  
  colnames(Ft_alo)[colnames(Ft_alo)=="Pais_text" ] <- "PAIS"
  colnames(Ft_alo)[colnames(Ft_alo)=='0'] <- "NO_ALOJAMIENTO"
  colnames(Ft_alo)[colnames(Ft_alo)=='1'] <- "HOTEL"
  colnames(Ft_alo)[colnames(Ft_alo)=='3'] <- "CAMPING"
  colnames(Ft_alo)[colnames(Ft_alo)=='4'] <- "CASA_RURAL"
  colnames(Ft_alo)[colnames(Ft_alo)=='6'] <- "VIVIENDA_PROPIEDAD"
  colnames(Ft_alo)[colnames(Ft_alo)=='8'] <- "VIVIENDA_ALQUILADA"
  colnames(Ft_alo)[colnames(Ft_alo)=='9'] <- "VIVIENDA_FAMILIARES"
  colnames(Ft_alo)[colnames(Ft_alo)=='11'] <- "OTROS"
  
  Ft_alo$Alo_pref <- apply(Ft_alo[2:9], 1, function(x) colnames(Ft_alo)[which.max(x)+1] )            
  Ft_alo <- Ft_alo[,c(1,10)] 

  datatable(Ft_alo)

  
#---------------------------------------------------------------------------------------
# 4- Ver correlación entre alojados en hotel y que han contratado paquete de viajes.

  Ft_hotel_paq <- Ft[,c(7,9,15)]
  Ft_hotel_paq <- Ft_hotel_paq[!Ft_hotel_paq$ALOJAMIENTOS==99,]
  Ft_hotel_paq <- Ft_hotel_paq[!Ft_hotel_paq$PAQUETE==9,]
  Ft_hotel_paq <- Ft_hotel_paq[!Ft_hotel_paq$ALOJAMIENTOS==11,]
  
  Ft_hotel_paq$ALOJAMIENTOS <- as.character(Ft_hotel_paq$ALOJAMIENTOS)
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==0,]$ALOJAMIENTOS <- "NO_ALOJAMIENTO"
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==1,]$ALOJAMIENTOS <- "HOTEL"
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==3,]$ALOJAMIENTOS <- "CAMPING"
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==4,]$ALOJAMIENTOS <- "CASA_RURAL"
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==6,]$ALOJAMIENTOS <- "VIVIENDA_PROPIEDAD"
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==8,]$ALOJAMIENTOS <- "VIVIENDA_ALQUILADA"
  Ft_hotel_paq[Ft_hotel_paq$ALOJAMIENTOS==9,]$ALOJAMIENTOS <- "VIVIENDA_FAMILIARES"
  Ft_hotel_paq$ALOJAMIENTOS <- as.factor(Ft_hotel_paq$ALOJAMIENTOS)
  
  Ft_hotel_paq[Ft_hotel_paq$PAQUETE == 1,]$PAQUETE <- "CON_PAQUETE"
  Ft_hotel_paq[Ft_hotel_paq$PAQUETE == 2,]$PAQUETE <- "SIN_PAQUETE"
  Ft_hotel_paq$PAQUETE <- as.factor(Ft_hotel_paq$PAQUETE)
  
  #Tabla de contingencia
  table(Ft_hotel_paq$ALOJAMIENTOS, Ft_hotel_paq$PAQUETE)

  #Comprobamos si las variables son independientes
  chiresult <- chisq.test(table(Ft_hotel_paq$ALOJAMIENTOS, Ft_hotel_paq$PAQUETE))
  plot(Ft_hotel_paq$ALOJAMIENTOS, Ft_hotel_paq$PAQUETE)
  chiresult


  #CONCLUSIONES-----  
  #Rechazamos la hipótesis nula de independenciapor lo tanto concluimos que ambas variables estudiadas
  #son dependientes, existe una relación entre las opción de usar paquete y el tipo de alojamiento. 
  
  #Tabla de contribuciones
  chiresult$residualschiresult$residuals
  #El principal responsabe de significacion parece hotel, ya que su contribución total es superior al resto 

    
  #Arbol
  arbol <- ctree(VIAJEROS ~ ALOJAMIENTOS + PAQUETE, data = Ft_hotel_paq, controls = ctree_control(mincriterion = 0.7, maxdepth = 3))
  
  plot(arbol, type="extended",           
       inner_panel=node_inner(arbol,
                              abbreviate = FALSE,            # short variable names
                              pval = FALSE,                 # no p-values
                              id = FALSE),                  # no id of node
       terminal_panel=node_terminal(arbol,
                                    abbreviate = FALSE,
                                    digits = 1,                   # few digits on numbers
                                    fill = c("grey"),            # make box white not grey
                                    id = FALSE)
  
  )
  
  #CONCLUSIONES-----
  #Observamos como el paquete es seleccionado por aquellas personas que no tienen ningún alojamiento 
  
  rm(Ft_hotel, Ft_hotel_paq)

#---------------------------------------------------------------------------------------------------------------
# SECTOR TRANSPORTE
#---------------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------
# 1. Ranking del tipo de transporte por nacionalidad.

  Ft_trans <- Ft[,c(5,11,15)]
  Ft_trans <- Ft_trans[!Ft_trans$TRANSPORTE==9,]
  
  Ft_trans <-  ddply(Ft_trans, .(PAIS,TRANSPORTE), summarize, VIAJEROS = sum(VIAJEROS) )
  Ft_trans$VIAJEROS <- ceiling(Ft_trans$VIAJEROS) 
  
  Ft_trans$TRANSPORTE <- gsub("10",  "Microbus", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("21",  "Vuelo_charter", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("22",  "Vuelo_regular", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("41",  "Tren", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("42",  "Linea_reg", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("43",  "Crucero", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("1",  "Coche_propio", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("2",  "Coche_alquiler", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("3",  "Coche_caravana", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("4",  "Auto_caravana", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("5",  "Auto_caravana", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("6",  "Auto_caravana", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("7",  "Autobus", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- gsub("8",  "Autobus", Ft_trans$TRANSPORTE)
  Ft_trans$TRANSPORTE <- as.factor(Ft_trans$TRANSPORTE)
  
  Ft_trans <- dcast(Ft_trans, PAIS ~ TRANSPORTE, fun=sum, value.var = "VIAJEROS")
  ListPaises <- read.table('data/Pais_de_residencia2.csv', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  ListPaises <- ListPaises[!(ListPaises$PAIS == "999"),]
  
  ListPaises$PAIS <- as.numeric(ListPaises$PAIS)
  Ft_trans$PAIS <- as.numeric(Ft_trans$PAIS)  
  Ft_trans <- merge(ListPaises, Ft_trans,by="PAIS")
  Ft_trans $PAIS <- NULL
  colnames(Ft_trans )[colnames(Ft_trans )=="Pais_text" ] <- "PAIS"
  
  Ft_trans$Microbus = rank(-Ft_trans$Microbus, ties = "first")
  Ft_trans$Vuelo_charter = rank(-Ft_trans$Vuelo_charter, ties = "first")
  Ft_trans$Vuelo_regular = rank(-Ft_trans$Vuelo_regular, ties = "first")
  Ft_trans$Tren = rank(-Ft_trans$Tren, ties = "first")
  Ft_trans$Crucero = rank(-Ft_trans$Crucero, ties = "first")
  Ft_trans$Coche_propio = rank(-Ft_trans$Coche_propio, ties = "first")
  Ft_trans$Coche_alquiler = rank(-Ft_trans$Coche_alquiler, ties = "first")
  Ft_trans$Coche_caravana = rank(-Ft_trans$Coche_caravana, ties = "first")
  Ft_trans$Auto_caravana = rank(-Ft_trans$Auto_caravana, ties = "first")
  Ft_trans$Autobus_reg = rank(-Ft_trans$Autobus_reg, ties = "first")
  Ft_trans$Autobus_dir = rank(-Ft_trans$Autobus_dir, ties = "first")
  Ft_trans$Linea_reg = rank(-Ft_trans$Linea_reg, ties = "first")
  
  datatable(Ft_trans)


#---------------------------------------------------------------------------------------
# 2.Por cada nacionalidad, correlación entre tipo de transporte y paquete de viajes.

  Ft_trans_nac <- Ft[,c(1,2,9, 11,15)]
  Ft_trans_nac <- Ft_trans_nac[!Ft_trans_nac$PAQUETE==9,]
  Ft_trans_nac <- Ft_trans_nac[!Ft_trans_nac$TRANSPORTE==9,]
  
  
  Ft_trans_nac$TRANSPORTE <- gsub("10",  "Microbus", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("21",  "Vuelo_charter", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("22",  "Vuelo_regular", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("41",  "Tren", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("42",  "Linea_reg", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("43",  "Crucero", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("1",  "Coche_propio", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("^2$",  "Coche_alquiler", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("3",  "Coche_caravana", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("4",  "Auto_caravana", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("5",  "Auto_caravana", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("6",  "Auto_caravana", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("7",  "Autobus_reg", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <- gsub("8",  "Autobus_dir", Ft_trans_nac$TRANSPORTE)
  Ft_trans_nac$TRANSPORTE <-  as.factor(Ft_trans_nac$TRANSPORTE)  
  
  Ft_trans_nac[Ft_trans_nac$PAQUETE == 1,]$PAQUETE <- "CON_PAQUETE"
  Ft_trans_nac[Ft_trans_nac$PAQUETE == 2,]$PAQUETE <- "SIN_PAQUETE"
  Ft_trans_nac$PAQUETE <- as.factor(Ft_trans_nac$PAQUETE)

  #Tabla de contingencia
  table(Ft_trans_nac$TRANSPORTE, Ft_trans_nac$PAQUETE)
  
  #Comprobamos si las variables son independientes
  chiresult <- chisq.test(table(Ft_trans_nac$TRANSPORTE, Ft_trans_nac$PAQUETE))
  plot(Ft_trans_nac$TRANSPORTE, Ft_trans_nac$PAQUETE)
  chiresult  
  
  #CONCLUSIONES-----  
  #Rechazamos la hipótesis nula de independencia, por lo tanto concluimos que ambas variables estudiadas
  #son dependientes. Existe una relación entre las opción de usar paquete y el tipo de transporte. 
  
  #Tabla de contribuciones
  chiresult$residuals
  #El principal responsabe de significacion parece vuelo charter y crucero, 
  #ya que su contribución total es superior al resto 
  
  
  
#---------------------------------------------------------------------------------------
# 3.Ver la evolución por mes de alquileres de coche, de vuelos, de trenes
   # Ft_trans_evol <- Ft[,c(1,2,11,15)]
  # Ft_trans_evol$MES <-  as.character(Ft_trans_evol$MES)
  # 
  # #Cargamos datos de los dos años anteriores 
  # Ft2014 <- read.table('data/Frontur2014.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  # Ft2014 <- Ft2014[,c(2,3,11,15)]
  # colnames(Ft2014)[colnames(Ft2014)=="TIPO.TRANSPORTE"] <- "TRANSPORTE"
  # colnames(Ft2014)[colnames(Ft2014)=="AÑO"] <- "ANO"
  # Ft2014$VIAJEROS<-as.numeric(Ft2014$VIAJEROS)
  # Ft_trans_evol <- rbind(Ft_trans_evol,Ft2014)
  # rm(Ft2014)
  # 
  # Ft2013 <- read.table('data/Frontur2013.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  # Ft2013 <- Ft2013[,c(2,3,11,15)]
  # colnames(Ft2013)[colnames(Ft2013)=="TIPO.TRANSPORTE"] <- "TRANSPORTE"
  # colnames(Ft2013)[colnames(Ft2013)=="AÑO"] <- "ANO"
  # Ft2013$VIAJEROS<-as.numeric(Ft2013$VIAJEROS)
  # Ft_trans_evol <- rbind(Ft_trans_evol,Ft2013)
  # rm(Ft2013)
  # 
  # Ft_trans_evol$VIAJEROS <- ceiling(Ft_trans_evol$VIAJEROS) 
  # Ft_trans_evol <- Ft_trans_evol[!Ft_trans_evol$TRANSPORTE==9,]
  # Ft_trans_evol <- Ft_trans_evol[!Ft_trans_evol$TRANSPORTE==7,]
  # 
  # Ft_trans_evol$TRANSPORTE <- gsub("10",  "Microbus", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("21",  "Vuelo charter", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("41",  "Tren", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("42",  "Linea regional", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("43",  "Crucero", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("1",  "Coche propio", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("2",  "Coche alquiler", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("3",  "Auto caravana", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("4",  "Auto caravana", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("5",  "Auto caravana", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("6",  "Auto caravana", Ft_trans_evol$TRANSPORTE)
  # # Ft_trans_evol$TRANSPORTE <- gsub("7",  "Autobus reg", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("8",  "Autobus dir", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <- gsub("9",  "Otro", Ft_trans_evol$TRANSPORTE)
  # Ft_trans_evol$TRANSPORTE <-  as.factor(Ft_trans_evol$TRANSPORTE)
  # 
  # #  Damos formato al campo fecha
  # Ft_trans_evol$FECHA <- as.Date(paste("01", as.numeric(Ft_trans_evol$MES), as.numeric(Ft_trans_evol$ANO), sep="-"), 
  #                                format = "%d-%m-%Y")
  # Ft_trans_evol$ANO <- NULL
  # Ft_trans_evol$MES <- NULL
  # 
  # #  Reordenamos la tabla
  # Ft_trans_evol <- Ft_trans_evol[, c(3,1,2)]
  # 
  # Ft_trans_evol <-  ddply(Ft_trans_evol, .(FECHA,TRANSPORTE), summarize, VIAJEROS = sum(VIAJEROS) )
  # 
  # ggplot(Ft_trans_evol, aes(x = FECHA, y = VIAJEROS)) +
  #   geom_point() + geom_smooth() +  facet_grid(~TRANSPORTE)


#---------------------------------------------------------------------------------------
# 3. Evolución por via de entrada

  Ft_trans_evol <- Ft[,c(1,2,14,15)]
  Ft_trans_evol$MES <-  as.character(Ft_trans_evol$MES)
  
  #Cargamos datos de los dos años anteriores 
  # Importamos los datos de http://estadisticas.tourspain.es/es-es/estadisticas/frontur/microdatos/paginas/default.aspx
  Ft2014 <- read.table('data/Frontur2014.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  colnames(Ft2014)[colnames(Ft2014)=="VIA.ENTRADA"] <- "ENTRADA"
  colnames(Ft2014)[colnames(Ft2014)=="AÑO"] <- "ANO"
  Ft2014$VIAJEROS<-as.numeric(Ft2014$VIAJEROS)
  Ft_trans_evol <- rbind(Ft_trans_evol,Ft2014[,c(2,3,14,15)])

  Ft2013 <- read.table('data/Frontur2013.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")
  colnames(Ft2013)[colnames(Ft2013)=="VIA.ENTRADA"] <- "ENTRADA"
  colnames(Ft2013)[colnames(Ft2013)=="AÑO"] <- "ANO"
  Ft2013$VIAJEROS<-as.numeric(Ft2013$VIAJEROS)
  Ft_trans_evol <- rbind(Ft_trans_evol,Ft2013[,c(2,3,14,15)])
  # rm(Ft2013)
  
  Ft_trans_evol$VIAJEROS <- ceiling(Ft_trans_evol$VIAJEROS) 
  # object.size(Ft_trans_evol)
  Ft_trans_evol <-  ddply(Ft_trans_evol, .(ANO,MES,ENTRADA), summarize, VIAJEROS = sum(VIAJEROS) )
  
  Ft_trans_evol$ENTRADA <- gsub("1",  "Aeropuerto", Ft_trans_evol$ENTRADA)
  Ft_trans_evol$ENTRADA <- gsub("2",  "Carretera", Ft_trans_evol$ENTRADA)
  Ft_trans_evol$ENTRADA <- gsub("3",  "Puerto", Ft_trans_evol$ENTRADA)
  Ft_trans_evol$ENTRADA <- gsub("4",  "Tren", Ft_trans_evol$ENTRADA)
  Ft_trans_evol$ENTRADA <- as.factor(Ft_trans_evol$ENTRADA)
  
  #  Damos formato al campo fecha
  Ft_trans_evol$FECHA <- as.Date(paste("01", as.numeric(Ft_trans_evol$MES), as.numeric(Ft_trans_evol$ANO), sep="-"), 
                                 format = "%d-%m-%Y")
  Ft_trans_evol$ANO <- NULL
  Ft_trans_evol$MES <- NULL
  
  #  Reordenamos la tabla
  Ft_trans_evol <- Ft_trans_evol[, c(3,1,2)]
  
  #  Pintamos los resultados
  ggplot(Ft_trans_evol, aes(x = FECHA, y = VIAJEROS)) +
    geom_point() + geom_smooth() +  facet_grid(~ENTRADA)

  #CONCLUSIONES-----
  # La mayoría de los turistas que entran a España lo hacen por aeropuerto y carretera. 
  # La serie temporal denota una clara estacionalidad en las vías de entrada.
  # Se observa la tendencia alcista en todas las vías sobre todo para el año 2015. 
  # El tren juega un papel inapreciable y mantiene una tendencia plana. 
  
  
  
  
#---------------------------------------------------------------------------------------------------------------
# SECTOR VIVIENDA
#---------------------------------------------------------------------------------------------------------------


#---------------------------------------------------------------------------------------
#  1. Ver la evolución de viviendas en propiedad, o alquiladas por extranjeros por mes y por año.

  Ft_vivienda <- Ft[,c(1,2,7,15)]
  Ft_vivienda$MES<-as.character(Ft_vivienda$MES)
  Ft_vivienda$ALOJAMIENTOS<-as.character(Ft_vivienda$ALOJAMIENTOS) 
  
  Ft_vivienda <- rbind(Ft_vivienda, Ft2013[,c(2,3,7,15)])
  rm(Ft2013)
  Ft_vivienda <- rbind(Ft_vivienda, Ft2014[,c(2,3,7,15)])
  rm(Ft2014)
  
  Ft_vivienda <- Ft_vivienda[(Ft_vivienda$ALOJAMIENTOS == c(6,1)),]
  Ft_vivienda <- ddply(Ft_vivienda, .(ANO,MES,ALOJAMIENTOS), summarize, VIAJEROS = sum(VIAJEROS) )
  Ft_vivienda$ALOJAMIENTOS <- gsub("6",  "VIVIENDA_EN_PROPIEDAD", Ft_vivienda$ALOJAMIENTOS)
  Ft_vivienda$ALOJAMIENTOS <- gsub("1",  "VIVIENDA_ALQUILADA", Ft_vivienda$ALOJAMIENTOS)
  
  Ft_vivienda$ALOJAMIENTOS <- as.factor(Ft_vivienda$ALOJAMIENTOS)
  
  #  Damos formato al campo fecha
  Ft_vivienda$FECHA <- as.Date(paste("01", as.numeric(Ft_vivienda$MES), as.numeric(Ft_vivienda$ANO), sep="-"), 
                                 format = "%d-%m-%Y")
  Ft_vivienda$ANO <- NULL
  Ft_vivienda$MES <- NULL
  
  #  Reordenamos la tabla
  Ft_vivienda <- Ft_vivienda[, c(3,1,2)]
  
  #  Pintamos los resultados
  ggplot(Ft_vivienda, aes(x = FECHA, y = VIAJEROS, col=ALOJAMIENTOS)) +
    geom_point() + geom_smooth() 
  
  
  ggplot( Ft_vivienda[(Ft_vivienda$ALOJAMIENTOS == "VIVIENDA_EN_PROPIEDAD"),], aes(x = FECHA, y = VIAJEROS)) +
    geom_point() + geom_smooth() 
  
  
  #CONCLUSIONES-----
  # La vivienda alquilada tiene una tendencia alcista acentuada en el 2015
  # La vivienda en propieda como es lógico no tiene factor estacionalidad tan acentuado
  # parece que la recuperación no ha afectado a la vivienda en propiedad en el mismo nivel que en el 
  # de vivienda alquilada. 


#---------------------------------------------------------------------------------------
#  2. Ver la distribución (con el mapa de provincias) de extranjeros que vienen por trabajo, estudios, vacaciones...
# 

  Ft_tipoviaje <-  Ft[,c(4,8,15)]
  
  # CÓDIGO DESCIRPCIÓN
  # 2 Trabajo y negocios
  # 3 Estudios
  # 31 Ocio, Vacaciones
  # 33 Personal (Familiares, Salud, Compras)
  # 34 Otros motivos
  # 99 No procede
  
  Ft_tipoviaje <- Ft_tipoviaje[Ft_tipoviaje$MOTIVOS== c(2,3,31),]
  Ft_tipoviaje <- ddply(Ft_tipoviaje, .(PROVINCIA,MOTIVOS), summarize, VIAJEROS = sum(VIAJEROS) )
  Ft_tipoviaje$MOTIVOS <- gsub("2",  "TRABAJO&NEGOCIOS", Ft_tipoviaje$MOTIVOS)
  Ft_tipoviaje$MOTIVOS <- gsub("31",  "OCIO&VACACIONES", Ft_tipoviaje$MOTIVOS)
  Ft_tipoviaje$MOTIVOS <- gsub("3",  "ESTUDIOS", Ft_tipoviaje$MOTIVOS)
  
  Ft_tipoviaje$MOTIVOS <- as.factor(Ft_tipoviaje$MOTIVOS)
  Ft_tipoviaje$PROVINCIA <- as.numeric(Ft_tipoviaje$PROVINCIA)
  
  #Mapa España Google
  
  #GGPLOT España
  # Shapefiles de provincias españolas: http://www.arcgis.com/home/item.html?id=83d81d9336c745fd839465beab885ab7
  esp <- geocode('Espana', source = "google")
  map.esp<- get_map(location = as.numeric(esp),
                    color = "color",
                    maptype = "roadmap",
                    scale = 2,
                    zoom = 6)
  
  provincias <- readOGR(dsn = "Provincias", "Provincias_ETRS89_30N")
  provincias$Cod_CCAA<-as.numeric(provincias$Cod_CCAA)
  provincias@data
  
  points <- spTransform(provincias , CRS("+proj=longlat +datum=WGS84"))
  points <- fortify(points) #Esto convierte el shapefile con todos sus atributos en un dataframe con el que ggmap puede operar.
  points$id<-as.numeric(points$id)
  points$id<-points$id+1 #Hay que sumarle 1 porque si no coge el 0, que no es ninguna provincia.
  
  # MAPA DE VIAJES POR NEGOCIOS
  Ft_viaje_neg <- Ft_tipoviaje[Ft_tipoviaje$MOTIVOS== "TRABAJO&NEGOCIOS",]
  Ft_viaje_neg$MOTIVOS <- NULL 
  colnames(Ft_viaje_neg)[colnames(Ft_viaje_neg)=="VIAJEROS"] <- "TRABAJO"
  points3 <- left_join(points, Ft_viaje_neg , by = c("id" = "PROVINCIA")) 
  
  ggmap(map.esp) + geom_polygon(aes(x=long,y=lat, group=group, fill=TRABAJO), data=points3, color='black') +
    scale_fill_distiller(breaks = pretty_breaks(n = 4), palette='Spectral')  
  
  # MAPA DE VIAJES POR ESTUDIOS
  Ft_viaje_est <- Ft_tipoviaje[Ft_tipoviaje$MOTIVOS== "ESTUDIOS",]
  Ft_viaje_est$MOTIVOS <- NULL 
  colnames(Ft_viaje_est)[colnames(Ft_viaje_est)=="VIAJEROS"] <- "ESTUDIOS"
  points4 <- left_join(points, Ft_viaje_est , by = c("id" = "PROVINCIA")) 
  ggmap(map.esp) + geom_polygon(aes(x=long,y=lat, group=group, fill=ESTUDIOS), data=points4, color='black') +
    scale_fill_distiller(breaks = pretty_breaks(n = 4), palette='Spectral') 
  
  # MAPA DE VIAJES POR VACACIONES
  Ft_viaje_vac <- Ft_tipoviaje[Ft_tipoviaje$MOTIVOS== "OCIO&VACACIONES",]
  Ft_viaje_vac$MOTIVOS <- NULL 
  colnames(Ft_viaje_vac)[colnames(Ft_viaje_vac)=="VIAJEROS"] <- "VACACIONES"
  points5 <- left_join(points, Ft_viaje_vac , by = c("id" = "PROVINCIA")) 
  ggmap(map.esp) + geom_polygon(aes(x=long,y=lat, group=group, fill=VACACIONES), data=points5, color='black') +
    scale_fill_distiller(breaks = pretty_breaks(n = 4), palette='Spectral') 













