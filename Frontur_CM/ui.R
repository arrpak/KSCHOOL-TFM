library(shiny)
library(leaflet)


  shinyUI(navbarPage("FRONTUR", id="nav",

# 1a PESTANA
  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

#PANEL DE CONTROL DE LA DERECHA
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("FILTRO"),
#SELECIONAR MES 
        selectInput("mes", "Mes:", Meses),
#SELECIONAR PAIS DE ORIGEN
        selectInput("pais", "Pais de Origen:", ListPais$Pais)
      ),

      checkboxInput("legend", "Show legend", TRUE)

    )
  )

))
