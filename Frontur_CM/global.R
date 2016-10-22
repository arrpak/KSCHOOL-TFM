<<<<<<< HEAD

library(plyr)
library(dplyr)

#CARGA PARAMETROS DE ENTRADA -------------------------------------------------

Meses <- c(
  "Enero" = 1,
  "Febrero" = 2,
  "Marzo" = 3,
  "Abril" = 4,
  "Mayo" = 5,
  "Junio" = 6,
  "Julio" = 7,
  "Agosto" = 8,
  "Septiembre" = 9
  # "Octubre" = 10,
  # "Noviembre" = 11,
  # "Diciembre" = 12
  
)

  ListPais <-  read.table('data/Pais_de_residencia_red.csv', sep=";", header = T, fileEncoding = "latin1" )
  ListPais$Pais <- as.character(ListPais$Pais)
  ListPais <- ListPais[order(ListPais$Pais),]
  ListPais <- ListPais[!ListPais$Clave.Pais == 108,] #Quitamos Espa?a
  

#CARGA VALORES DE SALIDA -------------------------------------------------

  #carga los datos
  Ft_res <- read.table('data/Frontur_CM.csv', sep = ',', colClasses = "character", header=T, fileEncoding = "latin1")
  Ft_res <- Ft_res[!Ft_res$VIAJEROS==0,]
   #Formato datos
  Ft_res$ANO <-as.factor(Ft_res$ANO)
  Ft_res$MES<-as.factor(Ft_res$MES)
  Ft_res$PROVINCIA <- as.integer(Ft_res$PROVINCIA)
  Ft_res$MUNICIPIO <- as.integer(Ft_res$MUNICIPIO)
  Ft_res$VIAJEROS <- as.numeric(Ft_res$VIAJEROS)
  Ft_res$VIAJEROS <- ceiling(Ft_res$VIAJEROS)  
  
  #Valor por defecto
  Ft_res_sal <- Ft_res[Ft_res$MES == 1 & Ft_res$PAIS.RESIDENCIAS == 126,]



=======

library(plyr)
library(dplyr)

#CARGA PARAMETROS DE ENTRADA -------------------------------------------------

Meses <- c(
  "Enero" = 1,
  "Febrero" = 2,
  "Marzo" = 3,
  "Abril" = 4,
  "Mayo" = 5,
  "Junio" = 6,
  "Julio" = 7,
  "Agosto" = 8,
  "Septiembre" = 9
  # "Octubre" = 10,
  # "Noviembre" = 11,
  # "Diciembre" = 12
  
)

  ListPais <-  read.table('data/Pais_de_residencia_red.csv', sep=";", header = T, fileEncoding = "latin1" )
  ListPais$Pais <- as.character(ListPais$Pais)
  ListPais <- ListPais[order(ListPais$Pais),]
  ListPais <- ListPais[!ListPais$Clave.Pais == 108,] #Quitamos Espa?a
  

#CARGA VALORES DE SALIDA -------------------------------------------------

  #carga los datos
  Ft_res <- read.table('data/Frontur_CM.csv', sep = ',', colClasses = "character", header=T, fileEncoding = "latin1")
 
   #Formato datos
  Ft_res$ANO <-as.factor(Ft_res$ANO)
  Ft_res$MES<-as.factor(Ft_res$MES)
  Ft_res$PROVINCIA <- as.integer(Ft_res$PROVINCIA)
  Ft_res$MUNICIPIO <- as.integer(Ft_res$MUNICIPIO)
  Ft_res$VIAJEROS <- as.numeric(Ft_res$VIAJEROS)
  Ft_res$VIAJEROS <- ceiling(Ft_res$VIAJEROS)  
  
  #Valor por defecto
  Ft_res_sal <- Ft_res[Ft_res$MES == 1 & Ft_res$PAIS.RESIDENCIAS == 126,]



>>>>>>> origin/master
