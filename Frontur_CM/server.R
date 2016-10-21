library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)



shinyServer(function(input, output, session) {

  ## Interactive Map ###########################################

  
  threshold <- 4000
  
  # Creamos el mapa
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -3.70, lat = 40.41, zoom = 6)  #Madrid     
  })
  

  # FILTRAMOS LOS DATOS EN FUNCION  DE LOS PARAMETROS DE ENTRADA
  observe({
 

    if (nrow(Ft_res_sal) > 0 )
    {
      leafletProxy("map", data = Ft_res_sal) %>% clearShapes() %>% clearControls()
    }    
    
    #Mes seleccionado
    Mes <-  input$mes

    #Pais seleccionado
   
    Pais <- input$pais
     # Pais <- "Alemania"   
    PaisNum <- ListPais[ListPais$Pais == Pais, ]$Clave.Pais
    
    # PaisNum <- 401
    # Mes <- 1
    Ft_res_sal <-  Ft_res[Ft_res$MES == Mes & Ft_res$PAIS.RESIDENCIAS == PaisNum ,]

    #Definimos el radio del circulo
    radius <-  Ft_res_sal$VIAJEROS / max(Ft_res_sal$VIAJEROS) * 50000
    radius <- ifelse(radius <=  threshold, threshold, radius)
        
    if (nrow(Ft_res_sal) > 10 )
    {  
        colorData <- Ft_res_sal$VIAJEROS
        pal <- colorBin("Spectral", colorData, 7, pretty = FALSE)      
        
      #Pintamos los circulos    
      leafletProxy("map", data = Ft_res_sal) %>%
        clearShapes() %>%
        addCircles(~longitude, ~latitude, radius=radius, layerId=~VIAJEROS,
                   stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData))

      #Pintamos los leyenda 
      proxy <- leafletProxy("map", data = Ft_res_sal)
      proxy %>% clearControls()
      if (input$legend) 
      {
        colorData <- Ft_res_sal$VIAJEROS
        pal <- colorBin("Spectral", colorData, 3, pretty = FALSE)           
        proxy %>% addLegend(position = "bottomright",
                            pal = pal, values = Ft_res_sal$VIAJEROS)
      }      
    }
    else
    {

      #Pintamos los circulos    
      leafletProxy("map", data = Ft_res_sal) %>%
        clearShapes() %>%
        addCircles(~longitude, ~latitude, radius=radius, layerId=~VIAJEROS,
                   stroke=FALSE, fillOpacity=0.7, fillColor= "#c39bcc")   
    }
    
  })

  mostrarDetalle <- function(id, lat, lng, mes, pais) {

    PaisNum <- ListPais[ListPais$Pais == pais, ]$Clave.Pais    
    selectedFT <- Ft_res[Ft_res$longitude == lng & Ft_res$latitude == lat & Ft_res$MES == mes & Ft_res$PAIS.RESIDENCIAS == PaisNum,   ]
    
    content <- as.character(tagList(
      tags$h4("MUNICIPIO:", selectedFT$NOMBRE),
      tags$br(),
      sprintf("Viajeros: %s", selectedFT$VIAJEROS), 
      tags$br()
      # sprintf("Media de noches : %s%%", ____), tags$br()
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId =selectedFT)
  }  

  
# Mostramos el pop-up de informacion
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    
    if (is.null(event))
      return()
    isolate({
      mostrarDetalle(event$id, event$lat, event$lng, input$mes, input$pais)      
    })
  })

  })
