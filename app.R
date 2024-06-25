#############################################################################################

#Atles peixos

############################################################################################

#Versio: 0
#Autor:Ramon Servitje

#############################################################################################
#Carrega de llibreries innicials

library(tidyverse)
library(shiny)
library(sf)
library(leaflet)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(ggspatial)
library(reshape2)
library(vegan)
library(geojsonsf)


############################################################################################
#carrega de capa base servei WMS del ICGC 

wms_icgh <- "https://geoserveis.icgc.cat/icc_mapesbase/wms/service"



############################################################################################
#carrega de capes inicial de punts ( no es pot carregar com un geojson;Leaflet dona error)




df_observacions_1 <- read.csv("CSV_observacions/Observacions_peixos_totals.csv", sep = ",", quote = "", header = TRUE, dec = ".", fill = TRUE)

df_observacions_2 <- mutate(df_observacions_1, latitut= as.numeric(X.latitude.),longitut=as.numeric(X.longitude.))

df_observacions <- df_observacions_2 %>% filter( !is.na(latitut),  !is.na(longitut))

capa_observacions <-  st_as_sf( df_observacions , coords = c("longitut","latitut") )

#Carrega de poligons i polilinees

Quadricula_5 <- read_sf("Capes_JSON/UTM1x1_5mts.geojson") %>% st_transform(crs = '+proj=longlat +datum=WGS84')  

Quadricula_10 <- read_sf("Capes_JSON/UTM1x1_10mts.geojson") %>% st_transform(crs = '+proj=longlat +datum=WGS84')  

Quadricula_20 <- read_sf("Capes_JSON/UTM1x1_20mts.geojson") %>% st_transform(crs = '+proj=longlat +datum=WGS84')  

Quadricula_30 <- read_sf("Capes_JSON/UTM1x1_30mts.geojson") %>% st_transform(crs = '+proj=longlat +datum=WGS84')  

Municipis_litorals <- read_sf("Capes_JSON/Municipis_costaners.geojson") %>% st_transform(crs = '+proj=longlat +datum=WGS84')  


##############################################################################################
#Programacio formulari d entrada


ui <- fluidPage(
  
  tags$head(
     titlePanel( h3(strong("ATLES PEIXOS DE CATALUNYA")))),
  
  br(),
  
  leafletOutput(outputId = 'map1')  
  
)


############################################################################################## 
# Programacio del Servidor


server <- function(input, output) {
  
  output$map1 = renderLeaflet({  
    
    leaflet( ) %>%
      
      #Carrega de la Quadricula 31TDG10 (Color vermell)
     
      addPolygons(data = Quadricula_30,stroke = TRUE, fillOpacity = 1,
                  color = "pink",
                  weight = 1,
                  opacity = 0.8,
                  fill = FALSE,
                  group = "Quadricula 1x1 fins 30mts"
      ) %>%
      
      
      addPolygons(data = Quadricula_20,stroke = TRUE, fillOpacity = 1,
                  color = "brown",
                  weight = 1,
                  opacity = 0.8,
                  fill = FALSE,
                  group = "Quadricula 1x1 fins 20mts"
      ) %>%
       
      addPolygons(data = Quadricula_10,stroke = TRUE, fillOpacity = 1,
                  color = "#FF0000",
                  weight = 1,
                  opacity = 0.8,
                  fill = FALSE,
                  group = "Quadricula 1x1 fins 5mts"
      ) %>%
      
      addPolygons(data = Quadricula_5,stroke = TRUE, fillOpacity = 1,
                  color = "blue",
                  weight = 1,
                  opacity = 0.8,
                  fill = FALSE,
                  group = "Quadricula 1x1 fins 10mts"
      
      ) %>%
      
      
      
      #Carrega de la capa de municipis litorals(lila)
      
      addPolygons(data = Municipis_litorals ,stroke = TRUE, fillOpacity = 1,
                  color = "#800080",
                  weight = 1,
                  opacity = 0.8,
                  fill = FALSE,
                  label = ~Municipis_litorals$NOMMUNI,
                  group = "Municipis litorals") %>%
      
      addCircles(data = capa_observacions ,stroke = TRUE, fillOpacity = 1,
                  color = "green",
                  weight = 0.5,
                  opacity = 0.8,
                  fill = TRUE
              
                 ) %>%
      
      #Carrega del Mapa base (ortofoto Esri)
      
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Satel.lit ICGC"
      ) %>%
      
      #Carrega del Mapa base ICGC (Mapa base ICGC 1:5000) del serveis WMS
      
      addWMSTiles(
        wms_icgh,
        layers = "mtc5m",
        group = "Base ICGC"
        #options = WMSTileOptions(format = "image/png", transparent = FALSE)
      ) %>%
     

     
      
      #Posicionament de focus al iniciar el mapa ( centre de la quadricula)
      
      setView(lng = 1.922069 , lat = 41.633880  , zoom = 7) %>%
      
      
      # Menu de control de capes afegir/treure i de mapa base ( baseGroups)
      

    addLayersControl(
      
      baseGroups = c("Satel.lit ICGC","Base ICGC (default)" ),
      
      overlayGroups = c( "Municipis litorals",
                         "Quadricula 1x1 fins 5mts", 
                         "Quadricula 1x1 fins 10mts", 
                         "Quadricula 1x1 fins 20mts", 
                         "Quadricula 1x1 fins 30mts"
                        
      ),
   
      
      options = layersControlOptions(collapsed = TRUE)
      
    )  
     
      
      
    
    
  }
  ) 
  
  
}

# Execucio de l app

shinyApp(ui = ui, server = server)
