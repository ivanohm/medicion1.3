## Logo de la pestaña
tags$head(
  HTML('<link rel="icon" href="favicon.jpg" 
                type="image/jpg" />'))

navbarPage(title = div("CONEVAL", span(img(src = "LOGOTIPO-CONEVAL.svg", height = 35))) , id = "coneval", collapsible = F,
           theme = bs_theme(version = 4, bootswatch = "minty", 
                            secondary = "#20c997", 
                            base_font = font_google("Cormorant Garamond"),
                            heading_font = font_google("Cormorant Garamond", wght = 700)),
           tabPanel(title = "Inicio",
                    titlePanel(h2("La pobreza en los municipios de México")), 
                    fluidRow(
                      column(width = 6,
                           
                           plotlyOutput("pobr_2d")
                           ),
                      column(width = 6, 
                             
                             plotlyOutput('indic_3d')
                           )
                      )
                    )
                    ,
           
             tabPanel(title = "Indicadores",
                    fluidRow(
                      box(
                        title = h2("Indicadores"), width = NULL, background = "light-blue"
                        
                      ),
                      
                      box(
                        title = "Selección", width = 4, solidHeader = TRUE,
                        
                        column(width = 2,
                          pickerInput(
                          inputId = "anio_t",
                          label = "Periodo", 
                          choices = (cat_anio$anio),
                          selected = "2020",
                          width = "fit",
                          options = list(
                          )
                        )),
                        column(width = 2,
                        pickerInput(
                          inputId = "indicador_t",
                          label = "Indicador:", 
                          choices = levels(factor(dicc_base$NAME, exclude = "")),
                          selected = "Población en situación de pobreza",
                          width = "fit",
                          options = list(
                            `live-search` = TRUE)
                        ))
                        
                      )),
                    fluidRow(
                      tabBox(
                        title = "Distribución", width = 6,
                        #tabPanel("Indicador","Gráfica", plotlyOutput("ch_indic")),
                        tabPanel("Indigenas","Gráfica", plotlyOutput("ch_indig")),
                        tabPanel("Tipología","Gráfica", plotlyOutput("ch_tip"))
                      ),
                      tabBox(
                        title = "Top de municipios ", width = 6,
                        tabPanel("Mayor","municipios", plotlyOutput("mun_more")),
                        tabPanel("Menor","municipios", plotlyOutput("mun_less"))
                      )),
                    fluidRow(
                      
                      box( title = "Diagrama", width = 6, 
                           plotlyOutput("sankey_plot")),
                      box( title = "Año comparación", width = 2, 
                           radioGroupButtons(
                             inputId = "anio_comp",
                             label = "Año:",
                             choices = c(cat_anio$anio),
                             selected = "2015",
                             direction = "vertical"
                           ),
                           sliderTextInput(
                             inputId = "bins",
                             label = "Numero de intervalos:",
                             selected = 5,
                             choices = c(seq(2,10,1)),
                             grid = TRUE
                           ))
                    )
                    ),
           
          
    
           tabPanel(title = "Municipios",
                    box(
                      title = h2("Municipios"), width = NULL, background = "light-blue"
                      
                    ),
                    box(title = "Selección", width = NULL, status = "primary", solidHeader = TRUE,
                        column(width = 4,
                               pickerInput(
                                 inputId = "ENTIDAD",
                                 label = "Entidad:", 
                                 choices = levels(factor(cat_mun$ENTIDAD)),
                                 selected = "07 - Chiapas",
                                 width = "auto",
                                 options = list(
                                   `live-search` = TRUE)
                               )),
                        column(width = 5,
                               pickerInput(
                                 inputId = "municipio_inp",
                                 label = "Municipio:", 
                                 choices = levels(factor(cat_mun$MUNICIPIO)),
                                 width = "auto",
                                 options = list(
                                   `live-search` = TRUE)
                               )
                        )),
                    fluidRow(

                      box(
                        title = "Indicadores de pobreza", width = 4, status = "primary",
                        plotlyOutput("ind_pobr")
                      ),
                      tabBox(
                        title = "", width = 8, 
                        tabPanel("Mapa",downloadButton('map_ent_desc'),leafletOutput("map_ent", width = "100%", height = 400),
                                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                               width = 430, height = "auto",
                                               
                                               h2("Indicadores"),
                                               pickerInput(
                                                 inputId = "anio_3",
                                                 label = "Periodo", 
                                                 choices = (cat_anio$anio),
                                                 selected = "2020",
                                                 width = "fit",
                                                 options = list(
                                                 )
                                               ),
                                               
                                               pickerInput(
                                                 inputId = "indicador_3",
                                                 label = "Indicador:", 
                                                 choices = levels(factor(dicc_base$NAME, exclude = "")),
                                                 selected = "Población en situación de pobreza",
                                                 width = "fit",
                                                 options = list(
                                                   `live-search` = TRUE)
                                               )
                                 )),
                        tabPanel("Tabla",DT::dataTableOutput("tabla3",width = 3))
                        
                      )
                      
                      
                      
                    ),
                    fluidRow(
                      box(title = "Carencias sociales", width = 4, status = "primary",
                          plotlyOutput("indic_mun")
                      ),
                      box("Indicadores", width = 8, status = "primary",
                          DT::dataTableOutput("tabla4"))
                    )
             ),
    
           tabPanel(title = "Datos",
                    fluidRow(
                      column(4,
                             selectInput(inputId = "grupo", label = "Grupo(s)", choices = c("Sin selección"="", levels(factor(dicc_base$GROUP))), selected = "Pobreza", multiple=TRUE)
                      ),
                      column(4,
                             
                             selectInput("indicadores", "Indicador(es)", choices = levels(factor(dicc_base %>% filter(GROUP == "Pobreza") %>% `$`('NAME'))), multiple=TRUE)

                      ),
                      column(2,
                             selectInput("tipo", "Tipo", c("Sin selección"="", levels(factor(dicc_base$tipo))), selected = "Porcentaje",multiple=TRUE)
                             
                      ),
                      column(2,
                             selectInput("anio", "Año", c("Sin selección"="", levels(factor(dicc_base$anio))), selected = levels(factor(dicc_base$anio)), multiple=TRUE)
                             
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("tab_indic")
             
             )
             
)
