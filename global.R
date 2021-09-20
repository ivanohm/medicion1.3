
# Global

## Programa de visualización de datos de la medición municipal 

##

## Instalamos las librerias a usar 

## librerias
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(dplyr)
library(stringr)
library(reshape2)
library(shinycssloaders)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)
library(sf)
library(sp)
library(ggspatial)
library(viridis)
library(data.table)
library(ggiraph)
library(RColorBrewer)
library(rgdal)
library(leaflet)
library(tableHTML)
library(shinythemes)
library(htmlwidgets)
library(bslib)
library(showtext)
library(dplyr)


## Carga de datos
load(file = "data/datos.rda")

# Esto es porque se quitó el ultimo mapa 
#load(file = "data/datos1.rda")
#save(Base, cat_anio, cat_ind, cat_indic, cat_mun, cat_zm, dicc_base, MAP_ENT, centroides,  file = "data/datos.rda")

   



