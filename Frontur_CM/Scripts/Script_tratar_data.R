
#carga del fichero 
Ft <- read.table('data/Frontur2015.txt', sep = ';', colClasses = "character", header=T, fileEncoding = "latin1")

#Tratamientos de los datos.
Ft_res <- Ft[,-c(1,6:14)]   #Nos quedamos con los que nos interesan

Ft_res$DESTINO.PRINCIPAL <- NULL
colnames(Ft_res)[colnames(Ft_res)=="AÑO"] <- "ANO"

# Ft_res$CC_AA<-substr(Ft$DESTINO.PRINCIPAL, 1, 2)
Ft_res$PROVINCIA<-substr(Ft$DESTINO.PRINCIPAL, 3, 4)
Ft_res$MUNICIPIO<-substr(Ft$DESTINO.PRINCIPAL, 5, 7)
# Ft_res <- Ft_res[!Ft_res$CC_AA == "00",]
Ft_res <- Ft_res[!Ft_res$PROVINCIA == "00",]
Ft_res <- Ft_res[!Ft_res$MUNICIPIO == "000",]

#Formato datos
Ft_res$ANO <-as.factor(Ft_res$ANO)
Ft_res$MES<-as.factor(Ft_res$MES) 
# Ft_res$CC_AA <- as.integer(Ft_res$CC_AA)
Ft_res$PROVINCIA <- as.integer(Ft_res$PROVINCIA)
Ft_res$MUNICIPIO <- as.integer(Ft_res$MUNICIPIO)
Ft_res$VIAJEROS <- as.numeric(Ft_res$VIAJEROS)
Ft_res$VIAJEROS <- round(Ft_res$VIAJEROS, 1)

#Reordenamos la tabla
Ft_res <- Ft_res[, c(1,2,5,6,3,4)]

#Sumamos de nuevo el numero de viajeros sin las columnas que no necesitamos.
Ft_res <- ddply(Ft_res, .(ANO, MES, PROVINCIA, MUNICIPIO, PAIS.RESIDENCIAS), summarize, VIAJEROS = sum(VIAJEROS))


#CARGA DE DATOS PROVINCIAS GEOLOCALIZADAS
# http://www.ine.es/daco/daco42/codmun/codmun11/11codmunmapa.htm
ListProv <- read.table('data/Listprov.csv', sep = ',', header = T, quote = "\"")
ListProv$X <- NULL
ListProv$NOMBRE <- as.character(ListProv$NOMBRE )
ListProv<-subset(ListProv,!(is.na(ListProv["lon"]) ))


#AÑADIMOS A CADA REGISTRO DE FROUNTUR LA LONGITUD Y LATITUD DE SU MUNICIPIO
#oPCION A - BUCLE
# Ft_res $longitude <- 0
# Ft_res $latitude <-  0
# Ft_res $NOM_MUN <- "Initial"
# for (i in 1: length(Ft_res $VIAJEROS)) {
#   Ft_res [i,]$NOM_MUN <-  ListProv[ListProv$CPRO == Ft_res [i,]$PROVINCIA & ListProv$CMUN == Ft_res [i,]$MUNICIPIO,]$NOMBRE
#   Ft_res [i,]$longitude<- ListProv[ListProv$CPRO == Ft_res [i,]$PROVINCIA & ListProv$CMUN == Ft_res [i,]$MUNICIPIO,]$lon
#   Ft_res [i,]$latitude <- ListProv[ListProv$CPRO == Ft_res [i,]$PROVINCIA & ListProv$CMUN == Ft_res [i,]$MUNICIPIO,]$lat
# }

# OPCION B - INNER
library(reshape2)
colnames(ListProv)[colnames(ListProv)=="CPRO"] <- "PROVINCIA"
colnames(ListProv)[colnames(ListProv)=="CMUN"] <- "MUNICIPIO"
colnames(ListProv)[colnames(ListProv)=="lon"] <- "longitude"
colnames(ListProv)[colnames(ListProv)=="lat"] <- "latitude"
Ft_merge <- merge(Ft_res, ListProv,by.x = c("PROVINCIA","MUNICIPIO"), by.y =  c("PROVINCIA","MUNICIPIO"), all.x = TRUE )

Ft_merge  <- Ft_merge [!is.na(Ft_merge $longitude),]

write.csv(Ft_merge, file = "data/Frontur_CM.csv", row.names = FALSE, sep =";" )
