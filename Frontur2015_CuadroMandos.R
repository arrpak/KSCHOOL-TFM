#CUADRO DE MANDOS
# Basado en SHINY y la libreria leaflet

library(shiny)

if (!require(devtools))
  install.packages("devtools")
  devtools::install_github("rstudio/leaflet")

runApp(appDir = "Frontur_CM")

