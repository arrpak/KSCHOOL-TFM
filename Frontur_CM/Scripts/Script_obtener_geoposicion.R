
# http://www.ine.es/daco/daco42/codmun/codmun11/11codmunmapa.htm
CodMun <- read.table('data/11codmun.csv', sep = ';', header = T, quote = "\"")
CodMun$NOMBRE <- as.character(CodMun$NOMBRE )

library(ggmap)

CodMun$lon <- 0
CodMun$lat <- 0

# #tratamos los datos para que geocode detecte bien las provincias con caracteres acentuados.
# geocode <- geocode( "España", source = "google")  
# 
# #Arreglamos un poco los datos 
CodMun$NOMBRE <- gsub("á",  "a", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("Á",  "a", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("é",  "e", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("É",  "e", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("í",  "i", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("Í",  "i", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("ó",  "o", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("Ó",  "o", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("ú",  "u", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("Ú",  "u", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("ü",  "u", CodMun$NOMBRE)
CodMun$NOMBRE <- gsub("ñ",  "n", CodMun$NOMBRE)


for (i in 1: length(CodMun$CPRO)) {
  
  geocode <- geocode( CodMun[i,]$NOMBRE, source = "google")  
  CodMun[i,]$lon <-  geocode$lon
  CodMun[i,]$lat <-  geocode$lat  
  
}

write.csv(CodMun, file = "data/Listprov2.csv",row.names = FALSE)
