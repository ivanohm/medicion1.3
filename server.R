server <- function(input, output, session) {


    ## Filtros para la tabla de datos 
    
    observe({
        indicadores <- if (is.null(input$grupo)) character(0) else {
            filter(dicc_base, GROUP %in% input$grupo) %>%
                `$`('NAME') %>%
                unique() %>%
                sort()
        }
        stillSelected <- isolate(input$indicadores[input$indicadores %in% indicadores])
        updateSelectizeInput(session, "indicadores", choices = indicadores,
                             selected = "Población en situación de pobreza", server = TRUE)
    })

    
    ## Tabla de datos 
    
    output$tab_indic <- renderDataTable({
        
           
        
        datatable(data = tab_indic <- Base %>% select(ENTIDAD, MUNICIPIO, NOM_ZM, MUN_IND, TIPOL, dicc_base %>% filter(GROUP %in% input$grupo & NAME %in% input$indicadores & tipo %in% input$tipo & anio %in% input$anio) %>% `$` ("VAR"))
                  
                  ,
                  escape = FALSE, filter = 'top', rownames = FALSE, 
                  extensions = list('ColReorder' = NULL, 'RowReorder' = NULL, 
                                    'Buttons' = NULL, 'RowGroup' = NULL), 
                  options = list(rowGroup = list(dataSrc = 0),
                                 dom = 'BRrltpi', scrollX = TRUE, autoWidth = TRUE, 
                                 lengthMenu = list(c(25, 50, 100, -1), c('25', '50', '100', 'All')), 
                                 ColReorder = TRUE, rowReorder = TRUE, 
                                 buttons = list('copy', 'print', 
                                                list(extend = 'collection', 
                                                     buttons = c('csv', 'excel', 'pdf'), 
                                                     text = 'Download'), I('colvis'))
                                 ), 
                  #colnames = c("Entidad", "Municipio", "Nombre de la ZM","Muncipio indigena","Tipología","Porcentaje", "Personas"),
                  
                  
        ) #%>% formatRound(6, digits=1) %>% formatCurrency(7, mark = ",", digits = 0, currency = "")
    }, server = TRUE)
    
    ## Municipios -----------------------------------------------------------------------------------------
    
    ### Municipios 
    
    
    
    municipio <- reactive(
        
        cat_mun %>% subset(ENTIDAD == input$ENTIDAD)
    )
    
    observeEvent(input$ENTIDAD, {
        updatePickerInput(session = session, inputId = "municipio_inp",
                          choices = levels(factor(municipio()$MUNICIPIO)))
    })
    
    ## grafica de indicadores de pobreza 
    output$ind_pobr <- renderPlotly({
        dat_plot_mun <- Base %>% filter(MUNICIPIO == input$municipio_inp) %>% select(MUNICIPIO, 
                                                                                     starts_with("POBR_POR_"), 
                                                                                     starts_with("POBM_POR_"),
                                                                                     starts_with("POBX_POR_"))
        dat_plot_mun <- reshape2::melt(dat_plot_mun, id = "MUNICIPIO", value.name = "PORCENTAJE")
        dat_plot_mun$AÑO <- str_split_fixed(dat_plot_mun$variable, pattern = "_", n = 3)[,3]
        dat_plot_mun$INDICADOR [str_split_fixed(dat_plot_mun$variable, pattern = "_", n = 3)[,1] == "POBR"] <- "Pobreza"
        dat_plot_mun$INDICADOR [str_split_fixed(dat_plot_mun$variable, pattern = "_", n = 3)[,1] == "POBM"] <- "Pobreza moderada"
        dat_plot_mun$INDICADOR [str_split_fixed(dat_plot_mun$variable, pattern = "_", n = 3)[,1] == "POBX"] <- "Pobreza extrema"
        dat_plot_mun$INDICADOR <- factor(dat_plot_mun$INDICADOR, 
                                         c("Pobreza extrema", 
                                           "Pobreza moderada",
                                           "Pobreza"))
        dat_plot_mun$PORCENTAJE <- round(dat_plot_mun$PORCENTAJE, 1)
        
        
        plot_ind_pobr<- ggplot(dat_plot_mun, aes(x = INDICADOR, y = PORCENTAJE, fill = AÑO)) +
            geom_bar(stat = "identity", position = "dodge")+
            scale_fill_brewer(palette="Greens", "Año")+
            theme_minimal()+
            labs(x = "Indicador", y="Porcentaje %", fill = "Año")#+
            #coord_flip()
        ggplotly(plot_ind_pobr)
        
        
    })
    
    ## Mapa 
    output$map_ent <- renderLeaflet({
        mypalette <- colorBin( palette="Greens", domain=Base[,paste0(paste0(filter(cat_indic, NAME %in% input$indicador_3) %>% select(ID_VAR)),"_POR_", input$anio_3)], na.color="transparent", bins=5)
        
        # Prepare the text for tooltips:
        mytext <- paste(
            "Entidad: ", input$ENTIDAD,"<br/>",
            "Municipio: ", MAP_ENT[[input$ENTIDAD]]@data$MUNICIPIO,"<br/>", 
            "Porcentaje: ", round(MAP_ENT[[input$ENTIDAD]]@data[,paste0(paste0(filter(cat_indic, NAME %in% input$indicador_3) %>% select(ID_VAR)),"_POR_", input$anio_3)],1), "<br/>", 
            "Personas: ", format(MAP_ENT[[input$ENTIDAD]]@data[,paste0(paste0(filter(cat_indic, NAME %in% input$indicador_3) %>% select(ID_VAR)),"_PER_", input$anio_3)], big.mark=",",scientific=FALSE), 
            sep="") %>%
            lapply(htmltools::HTML)
        
        # Final Map
        s <- leaflet(MAP_ENT[[input$ENTIDAD]]) %>% 
            addProviderTiles(providers$Esri.WorldTopoMap)  %>%
            setView(lng =  filter(centroides, ENTIDAD == input$ENTIDAD) %>% select(lng), lat = filter(centroides, ENTIDAD == input$ENTIDAD) %>% select(lat), zoom = 7) %>%
            addPolygons( 
                fillColor = ~mypalette(eval(rlang::sym(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_3) %>% select(ID_VAR)),"_POR_", input$anio_3)))), 
                stroke=TRUE, 
                fillOpacity = 0.9, 
                color="white", 
                weight=0.5,
                popup = mytext,
                popupOptions = popupOptions( 
                    style = list("font-weight" = "normal", padding = "6px 16px"), 
                    textsize = "13px", 
                    direction = "auto"
                )
            ) %>%
            addPolygons(data = MAP_ENT[[input$ENTIDAD]][MAP_ENT[[input$ENTIDAD]]$MUNICIPIO == input$municipio_inp,] , fill = F, weight = 2, color = "magenta", group = "Entidad") %>% 
            addLegend( pal=mypalette, values=~eval(rlang::sym(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_3) %>% select(ID_VAR)),"_POR_", input$anio_3))), opacity=0.9, title = "Porcentaje", position = "bottomleft" )
        
        s   
        
    })
    
    

    ## Tabla
    output$tabla3 <- renderDataTable({
        por_pobr <- Base %>% filter(ENTIDAD == input$ENTIDAD) %>% select(MUNICIPIO, paste0("POBR_POR_", input$anio_3))
        por_pobr$RANGO <- cut(por_pobr[,paste0("POBR_POR_", input$anio_3)],breaks =c(Base %>% select(paste0("POBR_POR_",input$anio_3)) %>% min(., na.rm = T),20,40,60,80,100), right=F, include.lowest = T)
        por_pobr <- por_pobr %>% group_by(RANGO) %>% summarize(MUNICIPIO = n())
        datatable( por_pobr,
                   rownames = F,
                   extensions = 'FixedColumns',
                   options = list(
                       dom = 't')
                   
        )
        
    })
    
    ## grafica
    output$indic_mun <- renderPlotly({
        dat_plotindic_mun <- Base %>% filter(MUNICIPIO == input$municipio_inp) %>% select(MUNICIPIO, 
                                                                                          starts_with("RZEDU_POR_"), 
                                                                                          starts_with("CARSALUD_POR_"),
                                                                                          starts_with("CARSS_POR_"),
                                                                                          starts_with("CARSBVIV_POR_"),
                                                                                          starts_with("CARVIV_POR_"),
                                                                                          starts_with("CARALIM_POR_"))
        dat_plotindic_mun <- reshape2::melt(dat_plotindic_mun, id = "MUNICIPIO", value.name = "PORCENTAJE")
        dat_plotindic_mun$AÑO <- str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,3]
        dat_plotindic_mun$INDICADOR [str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,1] == "RZEDU"] <- "Rezago educativo"
        dat_plotindic_mun$INDICADOR [str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,1] == "CARSALUD"] <- "Servicios de salud"
        dat_plotindic_mun$INDICADOR [str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,1] == "CARSS"] <- "Seguridad \n social"
        dat_plotindic_mun$INDICADOR [str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,1] == "CARVIV"] <- "Calidad y \n espacios \n de la vivienda"
        dat_plotindic_mun$INDICADOR [str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,1] == "CARSBVIV"] <- "Servicios básicos \n en la vivienda"
        dat_plotindic_mun$INDICADOR [str_split_fixed(dat_plotindic_mun$variable, pattern = "_", n = 3)[,1] == "CARALIM"] <- "Alimentación"
        
        
        
        dat_plotindic_mun$PORCENTAJE <- round(dat_plotindic_mun$PORCENTAJE, 1)
        dat_plotindic_mun <- reshape2::dcast(dat_plotindic_mun, formula = INDICADOR~AÑO, value.var = "PORCENTAJE")
        
        
        radar_car <- plot_ly(
            type = 'scatterpolar',
            fill = 'toself'
        ) 
        radar_car <- radar_car %>%
            add_trace(
                r = c(dat_plotindic_mun$`2010`),
                theta = c(dat_plotindic_mun$INDICADOR),
                fillcolor = list(color = brewer.pal(n = 3, name = "Greens")[1]), marker = list(color = brewer.pal(n = 3, name = "Greens")[1]),
                name = '2010'
            ) 
        radar_car <- radar_car %>%
            add_trace(
                r = c(dat_plotindic_mun$`2015`),
                theta = c(dat_plotindic_mun$INDICADOR),
                fillcolor = list(color = brewer.pal(n = 3, name = "Greens")[2]), marker = list(color = brewer.pal(n = 3, name = "Greens")[2]),
                name = '2015'
            )
        radar_car <- radar_car %>%
            add_trace(
                r = c(dat_plotindic_mun$`2020`),
                theta = c(dat_plotindic_mun$INDICADOR),
                fillcolor = list(color = brewer.pal(n = 3, name = "Greens")[3]), marker = list(color = brewer.pal(n = 3, name = "Greens")[3]),
                name = '2020'
            )
        radar_car <- radar_car %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,100)
                    ),
                    angularaxis = list(tickfont = list(size = 8))
                )
            )
        
        radar_car
    })
    
    ### Tabla de indicadores 
    
    output$tabla4 <- renderDataTable({
        municipio <- Base %>% subset(MUNICIPIO == input$municipio_inp)
        
        
        municipio <- reshape2::melt(data = municipio, id.vars = c("CVEGEO", "ENTIDAD", "MUNICIPIO", "NOM_ZM", "MUN_IND", "TIPOL"), value.name = "VALOR", variable.name = "INDICADOR")
        
        municipio$ID_VAR <- str_split_fixed(municipio$INDICADOR, "_", n = 2)[,1]
        
        municipio <- merge(municipio, cat_indic, by = "ID_VAR")
        
        municipio$TIPO <- ifelse(str_detect(municipio$INDICADOR, "POR"), "Porcentaje", 
                                 ifelse(str_detect(municipio$INDICADOR, "PER"), "Personas", "Carencias promedio"))
        municipio$TIPO <- factor(municipio$TIPO, c("Porcentaje", "Personas", "Carencias promedio"))
        municipio$Grupo <- factor(municipio$Grupo, c("Pobreza", "Privación social", "Indicadores de carencia social", "Bienestar"))
        
        municipio$AÑO <- ifelse(str_detect(municipio$INDICADOR, "2010"), "2010", 
                                ifelse(str_detect(municipio$INDICADOR, "2015"), "2015", "2020"))
        municipio <- reshape2::dcast(data = municipio, formula = Grupo + NAME~TIPO+AÑO, value.var = "VALOR")
        municipio$NAME <- factor(municipio$NAME)
        datatable(municipio,
                  escape = FALSE, filter = 'top', rownames = FALSE, 
                  extensions = list('ColReorder' = NULL, 'RowReorder' = NULL, 
                                    'Buttons' = NULL, 'RowGroup' = NULL), 
                  options = list(rowGroup = list(dataSrc = 0),
                                 dom = 'BRrltpi', scrollX = TRUE, autoWidth = TRUE, 
                                 lengthMenu = list(c(10, 50, 100, -1), c('10', '50', '100', 'All')), 
                                 ColReorder = TRUE, rowReorder = TRUE, 
                                 columnDefs = list(list(visible=FALSE, targets=c(5:10))),
                                 buttons = list('copy', 'print', 
                                                list(extend = 'collection', 
                                                     buttons = c('csv', 'excel', 'pdf'), 
                                                     text = 'Download'), I('colvis'))),
                  colnames = c("Grupo", "Indicador", "Porcentaje 2010", "Porcentaje 2015", "Porcentaje 2020", "Personas 2010", "Personas 2015", "Personas 2020", "Carencias promedio 2010", "Carencias promedio 2015", "Carencias promedio 2020")
        ) %>% formatRound(c(3,4,5,9,10,11), digits=1) %>% formatCurrency(c(6,7,8), mark = ",", digits = 0, currency = "")
    })
    
   
    
    ## grafica 1 municipios indigenas
    
    output$ch_indig <- renderPlotly({
        char_indic <- Base %>% select("CVEGEO", "ENTIDAD", "MUNICIPIO", "MUN_IND", paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)),"_POR_", input$anio_t))
        sample_size = char_indic %>% group_by(MUN_IND) %>% summarize(num=n())
        
        chart_indics<- char_indic %>%
            left_join(sample_size) %>%
            mutate(myaxis = paste0(MUN_IND, "\n", "n=", format(num, big.mark=",",scientific=FALSE))) %>%
            ggplot( aes(x=myaxis , y=!! rlang::sym(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)) ,"_POR_", input$anio_t)), fill=MUN_IND )) +
            geom_boxplot(width=0.1, color="grey", alpha=0.9) +
            scale_fill_brewer(palette = "Greens")+
            theme_ipsum() +
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle(paste0("Municipios indigenas", input$anio)) +
            xlab("")+
            ylab("Porcentaje")
        
        chart_indics
    })
    # grafica 2 de municipios por tipologia
    output$ch_tip <- renderPlotly({
        char_indic <- Base %>% select("CVEGEO", "ENTIDAD", "MUNICIPIO", "TIPOL", paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)),"_POR_", input$anio_t)) %>% filter(TIPOL != "Sin dato")
        sample_size = char_indic %>% group_by(TIPOL) %>% summarize(num=n())
        
        chart_indics<- char_indic %>%
            left_join(sample_size) %>%
            mutate(myaxis = paste0(TIPOL, "\n", "n=", format(num, big.mark=",",scientific=FALSE))) %>%
            ggplot( aes(x=myaxis , y=!! rlang::sym(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)) ,"_POR_", input$anio_t)), fill=TIPOL )) +
            
            geom_boxplot(width=0.1, color="darkgrey", alpha=0.6) +
            scale_fill_brewer(palette = "Greens")+
            theme_ipsum() +
            theme(
                legend.position="none",
                plot.title = element_text(size=11)
            ) +
            ggtitle("Tipología municipal") +
            xlab("")+
            ylab("Porcentaje")
        
        chart_indics
    })
    
    # Grafica 3 pobreza en los 15 municipios mas
    output$mun_more <- renderPlotly({
        ## Grafica comparativa 
        PLOT_1<- Base %>% select("ENTIDAD", "MUNICIPIO", starts_with(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)),"_POR_"))) %>% 
            unite("ENTIDAD", MUNICIPIO, ENTIDAD, remove = T, sep = " - ")
        names(PLOT_1) = c(str_remove(names(PLOT_1), paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)), "_POR_")))
        PLOT_1 <- PLOT_1 %>% top_n(!! rlang::sym(input$anio_t), n = 15)
        PLOT_1_A <- PLOT_1
        PLOT_1_A <- PLOT_1_A %>% arrange(desc(!! rlang::sym(input$anio_t))) %>% mutate(order = 1:n()) 
        PLOT_1_A <- PLOT_1_A %>% filter(order <= 15)
        
        PLOT_PLOT_1 <- ggplot() +
            # reshape the data frame & get min value so you can draw an eye-tracking line (this is one geom)
            geom_segment(
                data = gather(PLOT_1, AÑO, val, -ENTIDAD) %>% 
                    group_by(ENTIDAD) %>% 
                    top_n(-1) %>% 
                    slice(1) %>%
                    ungroup()%>% 
                    left_join(PLOT_1_A, by="ENTIDAD"),
                aes(x = 0, xend = val, y = reorder(ENTIDAD, order), yend = ENTIDAD),
                linetype = "dotted", size = 0.5, color = "grey60"
            ) +
            # reshape the data frame & get min/max category values so you can draw the segment (this is another geom)
            geom_segment(
                data = gather(PLOT_1, AÑO, val, -ENTIDAD) %>% 
                    group_by(ENTIDAD) %>% 
                    summarise(start = range(val)[1], end = range(val)[2]) %>% 
                    ungroup(),
                aes(x = start, xend = end, y = ENTIDAD, yend = ENTIDAD),
                color = "#21409A", size = .5
            ) +
            # reshape the data frame & plot the points
            geom_point(
                data = gather(PLOT_1, AÑO, value, -ENTIDAD),
                aes(value, ENTIDAD, color = AÑO), 
                size = 2
            ) +
            scale_color_brewer(palette = "Greens")+
            # i just extended the scale a bit + put axis on top; choose aesthetics that work 
            # for you
            scale_x_comma(position = "bottom", limits = c(min(min(gather(PLOT_1, AÑO, value, -ENTIDAD)[,"value"], na.rm = T))-1, max(gather(PLOT_1, AÑO, value, -ENTIDAD)[,"value"], na.rm = T))) +
            #scale_color_ipsum(name = "Año") +
            labs(
                x = "Porcentaje", y = NULL
            )+ 
            theme_ipsum_rc(base_size = 9.5, base_family = "Times New Roman") +
            theme(legend.position = "bottom")
        #scale_y_discrete(labels = c("Nacional"=expression(bold(Nacional))))
        
        #####################################################################
        ggplotly(PLOT_PLOT_1)
        
    })
    
    # Grafica 4 pobreza en los 15 municipios menos 
    output$mun_less <- renderPlotly({
        ## Grafica comparativa 
        PLOT_1<- Base %>% select("ENTIDAD", "MUNICIPIO", starts_with(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)),"_POR_"))) %>% 
            unite("ENTIDAD", MUNICIPIO, ENTIDAD, remove = T, sep = " - ") %>% filter(!! rlang::sym(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)),"_POR_", input$anio_t)) >0)
        names(PLOT_1) = c(str_remove(names(PLOT_1), paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)), "_POR_")))
        PLOT_1 <- PLOT_1 %>% top_n(!! rlang::sym(input$anio_t), n = -15)
        PLOT_1_A <- PLOT_1
        PLOT_1_A <- PLOT_1_A %>% arrange(!! rlang::sym(input$anio_t)) %>% mutate(order = 1:n()) 
        PLOT_1_A <- PLOT_1_A %>% filter(order <= 15)
        
        PLOT_PLOT_1 <- ggplot() +
            # reshape the data frame & get min value so you can draw an eye-tracking line (this is one geom)
            geom_segment(
                data = gather(PLOT_1, AÑO, val, -ENTIDAD) %>% 
                    group_by(ENTIDAD) %>% 
                    top_n(-1) %>% 
                    slice(1) %>%
                    ungroup()%>% 
                    left_join(PLOT_1_A, by="ENTIDAD"),
                aes(x = 0, xend = val, y = reorder(ENTIDAD, order), yend = ENTIDAD),
                linetype = "dotted", size = 0.5, color = "grey60"
            ) +
            # reshape the data frame & get min/max category values so you can draw the segment (this is another geom)
            geom_segment(
                data = gather(PLOT_1, AÑO, val, -ENTIDAD) %>% 
                    group_by(ENTIDAD) %>% 
                    summarise(start = range(val)[1], end = range(val)[2]) %>% 
                    ungroup(),
                aes(x = start, xend = end, y = ENTIDAD, yend = ENTIDAD),
                color = "#21409A", size = .5
            ) +
            # reshape the data frame & plot the points
            geom_point(
                data = gather(PLOT_1, AÑO, value, -ENTIDAD),
                aes(value, ENTIDAD, color = AÑO), 
                size = 2
            ) +
            scale_color_brewer(palette = "Greens")+
            # i just extended the scale a bit + put axis on top; choose aesthetics that work 
            # for you
            scale_x_comma(position = "bottom", limits = c(min(min(gather(PLOT_1, AÑO, value, -ENTIDAD)[,"value"], na.rm = T)), max(gather(PLOT_1, AÑO, value, -ENTIDAD)[,"value"], na.rm = T))) +
            #scale_color_ipsum(name = "Año") +
            labs(
                x = "Porcenataje", y = NULL
            )+ 
            theme_ipsum_rc(base_size = 9.5, base_family = "Times New Roman") +
            theme(legend.position = "bottom")
        #scale_y_discrete(labels = c("Nacional"=expression(bold(Nacional))))
        
        #####################################################################
        ggplotly(PLOT_PLOT_1)
        
    })
    
    ## Año de referencia
    
    output$anio_ref <- renderValueBox({
        valueBox(
            paste0(input$anio_t), "Año de referencia",
            color = "green"
        )
    })
    
    
    #Sankey 
    sankeyData <- reactive({
        
        sankeyData <- Base %>% select(starts_with(paste0(paste0(filter(cat_indic, NAME %in% input$indicador_t) %>% select(ID_VAR)), "_POR")))
        names(sankeyData) = c(cat_anio$anio)
        for (i in 1:nrow(cat_anio)) {
            sankeyData[,paste0(2005+5*i)] <- cut(sankeyData[,i], breaks = c(seq(0,100,100/input$bins)), right=F, include.lowest = T)
        }
        sankeyData <- data.frame(table(sankeyData[,paste0(input$anio_t)], sankeyData[,paste0(input$anio_comp)], useNA = "ifany"))
        sankeyData <- sankeyData %>% filter(Freq!=0)
        sankeyData <- sankeyData %>% filter(!is.na(Var1))
        sankeyData <- sankeyData %>% filter(!is.na(Var2))
        names(sankeyData) <- c("source", "target", "value")
        sankeyData$source <- paste(sankeyData$source, "->", paste0(input$anio_t))
        sankeyData$target <- paste(sankeyData$target, "->", paste0(input$anio_comp))
        sankeyNodes <- list(label = c(sankeyData$source,sankeyData$target) %>% unique())
        
        trace2 <- list(
            domain = list(
                x = c(0, 1), 
                y = c(0, 1)
            ), 
            link = list(
                label = paste0("Transición",1:nrow(sankeyData)), 
                source = sapply(sankeyData$source,function(e) {which(e == 
                                                                         sankeyNodes$label) }, USE.NAMES = FALSE) - 1, 
                target = sapply(sankeyData$target,function(e) {which(e == 
                                                                         sankeyNodes$label) }, USE.NAMES = FALSE) - 1, 
                value = sankeyData$value
            ), 
            node = list(label = sankeyNodes$label), 
            type = "sankey"
        )
        trace2
    })
    
    output$sankey_plot <- renderPlotly({
        trace2 <- sankeyData()
        p <- plot_ly()
        p <- add_trace(p, domain=trace2$domain, link=trace2$link, 
                       node=trace2$node, type=trace2$type)
        p 
        
        
    })
    
    ## plots 3d 
    
    
    output$pobr_2d <- renderPlotly({
        colors <- c("#A1D99B", "#41AB5D", "#006D2C")
        
        fig <- plot_ly(data = Base, x = ~POBM_POR_2015, y = ~POBX_POR_2015, color = ~TIPOL, size = ~POBR_POR_2015,  colors = colors,
                       text = ~paste('Entidad:', ENTIDAD, '<br>Municipio:', MUNICIPIO, '<br>Pobreza:', round(POBR_POR_2015,1),
                                     '<br>Población:', POB_2015)) 
        
        fig <- fig %>% layout(title = "Pobreza extrema y pobreza moderada, por tipología",
                              xaxis = list(title = "Pobreza moderada", 
                                           gridcolor = 'rgb(255, 255, 255)',
                                           range = c(0,100),
                                           zerolinewidth = 2,
                                           ticklen = 5,
                                           gridwidth = 3),
                              yaxis = list(title = "Pobreza extrema", 
                                           gridcolor = 'rgb(255, 255, 255)',
                                           range = c(0,100),
                                           zerolinewidth = 2,
                                           ticklen = 5,
                                           gridwidth = 3))
        
        fig
    })
    
    output$indic_3d <- renderPlotly({
        colors <- c("#A1D99B", "#41AB5D", "#006D2C")
        figura <- plot_ly(Base, x = ~CARSS_POR_2015, y = ~CARSALUD_POR_2015, z = ~CARALIM_POR_2015, color = ~TIPOL, colors = colors,
                       marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(0, 100),
                       text = ~paste('Entidad:', ENTIDAD, '<br>Municipio:', MUNICIPIO, '<br>Pobreza:', round(POBR_POR_2015,1),
                                     '<br>Población:', POB_2015))
        
        
        figura <- figura %>% layout(title = 'Indicadores de carencias sociales',
                              scene = list(xaxis = list(title = '(X) Carencia por acceso a la seguridad social',
                                                        gridcolor = 'rgb(255, 255, 255)',
                                                        range = c(0,100),
                                                        zerolinewidth = 1,
                                                        ticklen = 5,
                                                        gridwidth = 2),
                                           yaxis = list(title = '(Y) Carencia por acceso a los servicios de salud',
                                                        gridcolor = 'rgb(255, 255, 255)',
                                                        range = c(0,100),
                                                        zerolinewidth = 1,
                                                        ticklen = 5,
                                                        gridwith = 2),
                                           zaxis = list(title = '(Z) Carencia por acceso a la alimentación',
                                                        gridcolor = 'rgb(255, 255, 255)',
                                                        range = c(0,100),
                                                        zerolinewidth = 1,
                                                        ticklen = 5,
                                                        gridwith = 2)),
                              paper_bgcolor = 'rgb(243, 243, 243)',
                              plot_bgcolor = 'rgb(255, 255, 255)')
        
        figura
    })

}
