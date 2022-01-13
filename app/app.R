require(shiny)
require(shinythemes)
require(shinydashboard)
require(shinyjs)
require(shinyWidgets)
require(tigris)
options(tigris_use_cache = TRUE)
require(rgdal)
require(shinyWidgets)
require(leaflet)
require(sp)
require(dplyr)
require(spdplyr)
require(leaflet.extras)
require(graphics)
require(htmltools)
require(matrixStats)
require(viridis)
require(ggplot2)
require(scales)
require(wesanderson)
require(shinycssloaders)
require(leafpop)
source("Functions_v3_upload.R")

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
ui <-
   navbarPage("PathoTracer", id = "select1",  theme = shinytheme("flatly"),collapsible = T, inverse = T, #position = "fixed-top",
              tags$head(tags$style(HTML('.navbar-static-top {background-color: #0e7837;}',
                                        '.navbar-default .navbar-nav>.active>a {background-color: #0e7837;}'))),
              selected = "Pathogen maps",
              navbarMenu("Pathogen maps", 
                         tabPanel("Bacteria",fluidRow(column(width = 10,     
                         tags$head(tags$style('.selectize-dropdown {z-index: 100000000000; position: static}  
                                                               ')),
                                                             tabsetPanel( id = "bacteriaMaps",type = "tabs",
                                                                          tabPanel("Bacterial blight populations",
                                                                                   
                                                                                   selectInput('CountrySelection',         
                                                                                               label = "Select a country",
                                                                                               selected = "All data",
                                                                                               choices = forChoices,
                                                                                               multiple = FALSE,
                                                                                               width = 300    ), 
                                                                                   withSpinner(leafletOutput("MapPlot1",width="90%",height="630"), type = 1),
                                                                                   
                                                                                
                                                                          ),#tabPanel SNP
                                                                          tabPanel("Sweet gene induction", 
                                                                                   leafletOutput("MapPlot2",width="90%",height="630")
                                                                          ) #tabPanel Sweet
                                                             ) #tabsetPanel bacteriaMaps
                         ), #column width 10
                         column(width = 2,
                                
                                fluidRow(htmlOutput("hanna1_bacteria")),
                                fluidRow(plotOutput("CompositionPlot_bacteria", width = "90%", height = 250)),
                                fluidRow(htmlOutput("hanna2_bacteria"))
                         ) #column width 2
                         ),#fluidRow1
                         fluidRow(   width = "40%",
                                     htmlOutput("hanna3"),
                                     DT::dataTableOutput("sweetlines"),
                                     DT::dataTableOutput("ricedatabase") )
                         ), #tabPanel bacteria
                         tabPanel("Fungi",fluidRow(column(width = 10,
                                                          tabsetPanel(id = "fungiMaps", type = "tabs",
                                                                      tabPanel("M. grisea Avr gene frequency",
                                                                               
                                                                               leafletOutput("MapPlot3",width="90%",height="630")
                                                                      )# tabPanel M blast    
                                                          ) #tabsetPanel fungimaps      
                         ), #tabpanel fungi
                         column(width =2, 
                                
                                fluidRow(htmlOutput("hanna1_fungi")),
                                fluidRow(plotOutput("CompositionPlot_fungi", width = "90%", height = 250)),
                                fluidRow(htmlOutput("hanna2_fungi"))
                         ) #column
                         )# fluidRow
                         ),# tabsetPanel fungi
                         tabPanel("Viruses",
                                  fluidRow(column(width =10 ,
                                                  "Contact us for more information"    ))
                         ) #tabPanel virus
              ), ## navbarMenu
              tabPanel("Pest maps", "Contact us for more information"),
              tabPanel("More information",
                       tags$div(
                        #  tags$br(),tags$h4("Welcome!"), tags$br(),
                        #  "This application was built using R-Shiny and is hosted on the free tier of shinyapps.io.  It contains visualizations of various pathogen databases, assembled through sample collections of several research institutes. The app was developed for use by rice breeders and agricultural workers who take part in the selection and development of rice varieties for deployment. ",
                        #  tags$br(),tags$br(),
                        #  "To use the app, select a map to display. Click on a province to display more information.",
                        #  tags$br(),"", tags$br(),
                        #  tags$br(),"Publications", tags$br(),
                        #  tags$br(),tags$br(),
                        #  "",
                        #  tags$h1("Heading"), 
                          tags$h4("This application was built using R-Shiny. It is made available through the free tier of shinyapps.io."),
                          tags$h4(
                             HTML(paste("To use the app, ", tags$span(style="color:red", "select a map to display"), 
                                        ". Then, ", tags$span(style="color:red", "click on a province"), " to display more information.", sep = ""))
                          ),
                          tags$h3(" "),
                          tags$h5("Disclaimer of liability"), 
                          tags$h5("The data contained in this application is for general information only. Any decision you make after viewing the content is your responsibility and is strictly at your own risk."),
                          tags$h3(" "),
                          tags$h5(  DT::dataTableOutput("citations")),
                        
                          
                       )
              )
   ) #navbarPage

server <- function(input, output, session) {
   output$citations = DT::renderDataTable(package_citations,options = list(dom = 'ltipr'), rownames = F, colnames = "PathoTracer was built with the following R packages:")
   observeEvent(input$select1, {
      print(input$select1)
      output$hanna1_bacteria <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_bacteria <- NULL
      output$CompositionPlot_bacteria <- NULL
      output$hanna3 <- NULL
      output$ricedatabase <- NULL
      output$sweetlines <- NULL
      output$hanna1_fungi <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_fungi <- NULL
      output$CompositionPlot_fungi <- NULL
   })
   observeEvent(input$bacteriaMaps, {
      print(input$bacteriaMaps)
      output$hanna1_bacteria <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_bacteria <- NULL
      output$CompositionPlot_bacteria <- NULL
      output$hanna3 <- NULL
      output$ricedatabase <- NULL
      output$sweetlines <- NULL
      output$hanna1_fungi <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_fungi <- NULL
      output$CompositionPlot_fungi <- NULL
      
   })
   observeEvent(input$CountrySelection, {
      print(input$CountrySelection)
      output$hanna1_bacteria <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_bacteria <- NULL
      output$CompositionPlot_bacteria <- NULL
      output$hanna3 <- NULL
      output$ricedatabase <- NULL
      output$sweetlines <- NULL
      output$hanna1_fungi <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_fungi <- NULL
      output$CompositionPlot_fungi <- NULL
   })
   #####   #####   #####   #####   #####   ##### ##
   
   output$MapPlot1 <- renderLeaflet({
      world %>%
         leaflet()%>%
         addProviderTiles("Stamen.TerrainBackground")%>%
         addProviderTiles("CartoDB.PositronOnlyLabels")%>%
         addPolygons(data= allmerged,
                     layerId = ~NAME_1,
                     popup = paste0(
                       "<strong>",  allmerged$NAME_1,"</strong>" , "<strong>, </strong>", 
                       "<strong>",  allmerged$Country,"</strong>"),
                     fillColor = ~pal3( allmerged$Map.Display),
                     fillOpacity = 1,
                     weight = 0.8,
                     stroke = TRUE,
                     color = "black",
                     opacity = 0.9,
                     smoothFactor = 0.2,
                     highlight = highlightOptions(
                        weight = 0.7,
                        color = "gray",
                        fillOpacity = 0.8,
                        bringToFront =TRUE),
                     labelOptions = labelOptions(
                        style = list("font-weight" = "normal",
                                     padding = "3 px 8 px"),
                        textsize = "15 px",
                        direction = "auto")) %>% 
         addLegend(pal = pal3, 
                   values = allmerged$Map.Display,
                   position = "topright",
                   title = "Predominant population ID", 
                   opacity = 0.9)%>%
         
         addResetMapButton()
   }) #renderleaflet MapPlot1
   
   output$MapPlot2 <- renderLeaflet({
      simple_philippines %>%
         leaflet()%>% 
         setView(lng = mean(sweetmap@bbox[1,]), 
                 lat = mean(sweetmap@bbox[2,]), 
                 zoom = 6.5)%>%
         # addProviderTiles("Stamen.TerrainBackground")%>%
         addProviderTiles("CartoDB.Positron")%>%
         addPolygons(data= sweetmap,
                     layerId = ~NAME_1,# hehehehe this might save you later
                     popup = paste0(
                        "<strong>", sweetmap$NAME_1,"</strong>" , "<strong>, </strong>", 
                        "<strong>", sweetmap$Country,"</strong>"
                     ),
                     #     fillColor = ~percentpal(sweetmap1$Percent.Display),
                     fillColor = ~colorFactor(tryColor, levels = as.factor(sweetmap$Map.Display), ordered = TRUE)(sweetmap$Map.Display),
                     fillOpacity = 1,
                     weight = 1,
                     stroke = TRUE,
                     color = "black",
                     opacity = 0.9,
                    # label = htmltools::HTML(paste0(
                  #      "<strong>", sweetmap$NAME_1,"</strong>" , "<strong>, </strong>", 
                  #      "<strong>", sweetmap$Country,"</strong>","<br/>"
                        
                  #   )),
                     smoothFactor = 0.2,
                     highlight = highlightOptions(
                        weight = 1,
                        color = "black",
                        fillOpacity = 0.8,
                        bringToFront =TRUE),
                     labelOptions = labelOptions(
                        style = list("font-weight" = "normal",
                                     padding = "3 px 8 px"),
                        textsize = "15 px",
                        direction = "auto"))  %>% 
         addLegend(
            #  pal = percentpal, 
            pal = colorFactor(tryColor, levels = as.factor(sweetmap$Map.Display), ordered = TRUE),
            #   values = ~sweetmap1$Percent.Display,
            values = ~sweetmap$Map.Display,
            position = "topright",
            title = "OsSWEET gene activation", 
            opacity = 0.9)%>%
         addResetMapButton()
   }) #renderLeaflet MapPlot2
   
   output$MapPlot3 <- renderLeaflet({
      pal2 <- leaflet::colorNumeric(
         palette = viridis::viridis_pal(begin = 0,  option = "D", alpha = 0.72)(5),
         domain = unique(blast$Map.Display)
      )
      simple_philippines %>%
         leaflet()%>%
         addProviderTiles("Stamen.TerrainBackground")%>%
         addProviderTiles("CartoDB.PositronOnlyLabels")%>%
         setView(lng = mean(blastmerged@bbox[1,]), 
                 lat = mean(blastmerged@bbox[2,]), 
                 zoom = 5.2)%>%
         addPolygons(data= blastmerged,
                     layerId = ~NAME_1,
                     popup = paste0(
                        "<strong>", blastmerged$NAME_1,"</strong>" , "<strong>, </strong>", 
                        "<strong>", blastmerged$Country,"</strong>"),
                     fillColor = ~pal2(blastmerged$Map.Display),
                     fillOpacity = 1,
                     weight = 0.8,
                     stroke = TRUE,
                     color = "black",
                     opacity = 0.9,
                     smoothFactor = 0.2,
                     highlight = highlightOptions(
                        weight = 0.7,
                        color = "gray",
                        fillOpacity = 0.8,
                        bringToFront =TRUE),
                     labelOptions = labelOptions(
                        style = list("font-weight" = "normal",
                                     padding = "3 px 8 px"),
                        textsize = "15 px",
                        direction = "auto")) %>% 
         addLegend(pal = pal2, 
                   values = blastmerged$Map.Display,
                   position = "topright",
                   bins = 2,
                   labFormat = labelFormat(digits = 1),
                   title = "No. of dominant (>= 80%) <br> avr genes ", 
                   opacity = 0.9)%>%
         addResetMapButton()
   }) #renderleaflet MapPlot3
   filtered <-  reactive({ if( input$CountrySelection == "All data"){
      allmerged 
   }else{
      allmerged %>% dplyr::filter(Country == input$CountrySelection)
   }# else statement, filtered
   }) #reactive filtered
   
   observeEvent(input$CountrySelection,{
      output$MapPlot1 <- renderLeaflet({
         world %>%
            leaflet()%>%
            addProviderTiles("Stamen.TerrainBackground")%>%
            addProviderTiles("CartoDB.PositronOnlyLabels")%>%
            addPolygons(data= filtered(),
                        layerId = ~NAME_1,
                        popup = paste0(
                           "<strong>",  filtered()$NAME_1,"</strong>" , "<strong>, </strong>", 
                           "<strong>",  filtered()$Country,"</strong>"),
                        fillColor = ~pal3( filtered()$Map.Display),
                        fillOpacity = 1,
                        weight = 0.8,
                        stroke = TRUE,
                        color = "black",
                        opacity = 0.9,
                        smoothFactor = 0.2,
                        highlight = highlightOptions(
                           weight = 0.7,
                           color = "gray",
                           fillOpacity = 0.8,
                           bringToFront =TRUE),
                        labelOptions = labelOptions(
                           style = list("font-weight" = "normal",
                                        padding = "3 px 8 px"),
                           textsize = "15 px",
                           direction = "auto")) %>% 
            addLegend(pal = pal3, 
                      values = allmerged$Map.Display,
                      position = "topright",
                      title = "Predominant population ID", 
                      opacity = 0.9)%>% setView(lng = mean(filtered()@bbox[1,]), 
                                                lat = mean(filtered()@bbox[2,]), 
                                                zoom = 4.2)%>% 
            addResetMapButton()
      }) #renderleaflet MapPlot1
        
         
       
   }, ignoreInit = TRUE)## observeEvent input$CountrySelection
   observeEvent(input$MapPlot1_shape_click,{
      click <- input$MapPlot1_shape_click
      print(click$lat)
      if(is.null(click$id))
         return()
      print(click$id)
      selectedProv <- filtered() %>% dplyr::filter(NAME_1 == click$id)
      #added these for zooming in when a province is clicked
      selectedProvName <- click$id
      str(selectedProvName)
      #for the compostion plot, selectedComposition
      selectedComposition <- as.data.frame(droplevels(subset(asfda, as.character(NAME_1) == selectedProvName)))
      str(selectedComposition)
      selectedComposition
      #for strain provider
      Provider <- Inst %>%  filter (NAME_1 == click$id) %>%  
         summarise( Provider = paste(unique(as.character(Institute)), collapse = ", ")) %>% 
         droplevels() 
      #for gene recommendation
      selectedCompositiongenes <- selectedComposition %>%  
         filter(nmode == max(nmode))
      strains_flat_filter <- as.data.frame(droplevels(subset(strains, as.character(NAME_1) == selectedProvName)))
      ifelse(is.null(click$id)||is.null(selectedComposition)||is.null(selectedCompositiongenes)||is.null(selectedProv),
             return(NULL), ifelse(selectedProv$Map.Display == "uncharacterized"||selectedProv$Map.Display == "Mixed population"||strains_flat_filter$Percent <=50,
                                  Genes <- "Not enough data for recommendation",
                                  Genes <- as.vector(pull(dplyr::left_join(x = selectedCompositiongenes,
                                                                           y = geneRec, 
                                                                           by = c("PopIDmod"="Map.Display"), Gene.recommendation)))
             ))
      output$hanna1_bacteria <- renderUI({
         if(is.null(click$id)){
            HTML(paste0("(Click on a location)"))
         } else{
            HTML(paste0(
               "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
               "<strong>", selectedProv$Country,"</strong>", "<br/>", "<br/>",
               "<strong>Predominant ID: </strong>", "<br/>",
               selectedProv$Map.Display, "<br/>",  "<br/>",
               "<strong>Composition </strong>",
               paste("(n = ", as.character(sum(selectedComposition$Count)), sep = "", collapse = ""),
               paste( "): ")
               
            ))
            
         }
      }) # hanna1 
      output$hanna2_bacteria <- renderUI({
         if(is.null(click$id)||is.null(Genes)){
            return()
         } else{
            HTML(paste0(
               "<strong>Sample provider(s): </strong>", "<br/>",Provider,
               "<br/>","<br/>",
               "<strong>Gene recommendation</strong>", "<strong> (under validation): </strong>", "<br/>",
               as.character(Genes)
               # "<br/>","<strong>Variety/Line recommendation(s) (under validation): </strong>", "<br/>",
               # gsub('"', "",gsub("^c\\(|\\)$", "", 
               #                   rice %>% dplyr::select(one_of(as.character(Genes)))))
            ))
         }
         
      }) # hanna2
      output$hanna3 <- renderUI({
         if(is.null(click$id)||is.null(Genes)||Genes == "Not enough data for recommendation"){
            return()
         } else{
            HTML(paste0(
               "<br/>","<strong>Variety/Line recommendation (to be validated)*: </strong>", "<br/>","<br/>"
            ))
         }
         
      }) # hanna
      output$CompositionPlot_bacteria <- renderPlot(
         if(is.null(selectedComposition)){
            return()
         }else{
            vectorOfColors <-as.vector( pull(dplyr::left_join(x = setNames(data.frame(selectedComposition[,"PopIDmod"]  ) , "Map.Display"),
                                                              y = colorPops, by = "Map.Display"), Color))
            plotSelected <-ggplot(selectedComposition, 
                   aes(x = PopID, y = Count/sum(Count), fill = PopID ))+
               geom_col (position = "dodge")+
               scale_y_continuous(labels = percent, limits = c(0,1))+
               theme_set( theme_minimal()+theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust =1) ))+
               scale_fill_manual(values = vectorOfColors,
                                 guide_legend(title = NULL, ncol = 4, nrow = 2))+
               labs(x = "Population ID", y = "Composition")
            plotSelected
         })
      
      
      
      ######## hanna1 FUNGI
      output$hanna1_fungi <- renderUI({
         if(is.null(click$id)){
            HTML(paste0("(Click on a location)"))
         } else{
            HTML(paste0(
               "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
               "<strong>", selectedProv$Country,"</strong>", "<br/>", "<br/>",
               "<strong>Predominant ID: </strong>", "<br/>",
               selectedProv$Map.Display, "<br/>",  "<br/>",
               "<strong>Composition </strong>",
               paste("(n = ", as.character(sum(selectedComposition$Count)), sep = "", collapse = ""),
               paste( "): ")
               
            ))
            
         }
      }) # hanna1 
      output$hanna2_fungi <- renderUI({
         if(is.null(click$id)||is.null(Genes)){
            return()
         } else{
            HTML(paste0(
               "<strong>Strain provider(s): </strong>", "<br/>",Provider,
               "<br/>",
               "<strong>Gene recommendation</strong>", "<strong> (under validation): </strong>", "<br/>",
               as.character(Genes)
               # "<br/>","<strong>Variety/Line recommendation(s) (under validation): </strong>", "<br/>",
               # gsub('"', "",gsub("^c\\(|\\)$", "", 
               #                   rice %>% dplyr::select(one_of(as.character(Genes)))))
            ))
         }
         
      }) # hanna2
    
      output$CompositionPlot_fungi <- renderPlot(
         if(is.null(selectedComposition)){
            return()
         }else{
            vectorOfColors <-as.vector( pull(dplyr::left_join(x = setNames(data.frame(selectedComposition[,"PopIDmod"]  ) , "Map.Display"),
                                                              y = colorPops, by = "Map.Display"), Color))
            ggplot(selectedComposition, 
                   aes(x = PopID, y = Count/sum(Count), fill = PopID ))+
               geom_col (position = "dodge")+
               scale_y_continuous(labels = percent, limits = c(0,1))+
               theme_set( theme_minimal()+theme(legend.position = "top", legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust =1) ))+
               scale_fill_manual(values = vectorOfColors,
                                 guide_legend(title = NULL, ncol = 4, nrow = 2))+
               labs(x = "Population ID", y = "Composition")
         }
      )
      ######################################################################
      ################## i want the rice lines as a table###################
      ######################################################################
      
      output$ricedatabase <- DT::renderDataTable(
         expr = ricetable(),
         options = list(
            scrollX = TRUE, 
            paging = TRUE,
            lengthMenu = c(10, 20, 50)),
         selection = "single",
         rownames = TRUE,
         caption = "Breeding materials and released varieties"  
      )
      ricetable <- reactive({
         input$MapPlot1_shape_click
         print(str(Genes))
         toFilter <- names(ricedatabase)[(names(ricedatabase) %in% unlist(strsplit(Genes, ", ")))] 
         print(toFilter)
         print(  unlist(strsplit(Genes, ", ")))
         if(Genes == "Not enough data for recommendation"|| is.null(Genes)||is.null(click$id)) {
            ricedata <- NULL
         } else{
            if(length(toFilter) == 1){
               ricedata <- ricedatabase %>% dplyr::filter(ricedatabase[,Genes] == "R") %>% filter_all(any_vars(!is.na(.)))
            }else{
               ricedata <- ricedatabase[apply(ricedatabase[,toFilter] == "R" 
                                              , 1, all),] %>% filter_all(any_vars(!is.na(.)))
               
            }}
         
         ricedata
      })
      ##################### rice ##############################
      
      ######################################################################
      ############################ sweet mutants############################
      ######################################################################
      
      output$sweetlines <- DT::renderDataTable(
         expr = sweetmutantsreact(),
         options = list(
            scrollX = TRUE, 
            paging = TRUE,
            lengthMenu = c(10, 20, 50)),
         selection = "single",
         rownames = TRUE,
         caption = "SWEET mutant lines"  
      )
      sweetmutantsreact <- reactive({
         if(Genes == "Not enough data for recommendation"|| is.null(Genes)||is.null(click$id)) {
            mutantlines <- NULL
         } else{
            mutantlines <- sweetmutants
         }
         
         
         mutantlines
      })
      
      
      ############################ sweet mutants############################
   })# dulo ng observeEvent
   observeEvent(input$MapPlot1_click, { print("map clicked") 
      print(input$MapPlot1_click)

      output$CountrySelection<- NULL
      output$hanna1_bacteria <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_bacteria <- NULL
      output$CompositionPlot_bacteria <- NULL
      output$ricedatabase <- NULL
      output$sweetlines <- NULL
      output$hanna3 <- NULL
   })
   observeEvent(input$MapPlot2_click, { print("map clicked") 
   
      output$hanna1_bacteria <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_bacteria <- NULL
      output$CompositionPlot_bacteria <- NULL
   })
   observeEvent(input$MapPlot2_shape_click,{
      click <- input$MapPlot2_shape_click
      print(click$lat)
      if(is.null(click$id))
         return()
      print(click$id)
      selectedProv <- sweetmap %>% dplyr::filter(NAME_1 == click$id)
      selectedProvName <- click$id
      str(selectedProvName)
      #for the compostion plot, selectedComposition
      selectedComposition <- as.data.frame(droplevels(subset(sweetsummary, as.character(NAME_1) == selectedProvName)))
      str(selectedComposition)
      selectedComposition
      output$hanna1_bacteria <- renderUI({
         if(is.null(click$id)){
            HTML(paste0("(Click on a location)"))
         } else{
            HTML(paste0(
               "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
               "<strong>", selectedProv$Country,"</strong>", "<br/>", "<br/>",
               "<strong>Putative OsSWEET activation: </strong>", 
               selectedProv$Map.Display, "<br/>",
               paste0( selectedComposition$Percent.Display, "%  ", "(n = ", selectedComposition$Count, ")") ,
               
               "<br/>",
               #  paste("n = ", as.character(sum(selectedComposition$Count)), sep = "", collapse = ""),      "<br/>",
               "<br/>","<strong>Variety/Line recommendation(s): </strong>", "<br/>",
               paste0("IR64 SWEET promoter mutant lines"), "<br/>",
               paste0("Ciherang Sub1 SWEET promoter mutant lines")
               
            ))
         }
      }) # hanna1 
   }) # observe event MapPlot2_shape_click
   observeEvent(input$MapPlot3_shape_click,{
      click <- input$MapPlot3_shape_click
      print(click$lat)
      if(is.null(click$id))
         return()
      print(click$id)
      selectedProv <- blastmerged %>% dplyr::filter(NAME_1 == click$id)
      selectedProvName <- click$id
      selectedComposition <- as.data.frame(droplevels(subset(blastafdsa, as.character(NAME_1) == selectedProvName)))
      str(selectedComposition)
      selectedComposition
      output$hanna1_fungi <- renderUI({
         if(is.null(click$id)){
            HTML(paste0("(Click on a location)"))
         } else{
            HTML(paste0(
               "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
               "<strong>", selectedProv$Country,"</strong>", "<br/>", "<br/>",
               "<strong>Predominant avr gene(s): </strong>", "<br/>",
               gsub('"', "",gsub("^c\\(|\\)$|percent.", "", 
                                 selectedProv$Genes)),
               "<br/>",  "<br/>"
               
            ))
         }
      }) # hanna1 
      output$hanna2_fungi <- NULL
      output$CompositionPlot_fungi <- renderPlot(
         if(is.null(selectedComposition)){
            return()
         }else{
            ggplot(reshape::melt(selectedComposition, id =c("NAME_1", "Count")), 
                   aes(x = variable, y = value ,  fill = value))+
               geom_bar (stat = "identity")+
               theme_set( theme_minimal()+theme(legend.position = "top", legend.title = element_blank(), 
                                                axis.text.x = element_text(angle = 0, hjust =1) ))+
               scale_x_discrete(labels = genenames)+
               labs(x = "avr gene", y = "Percent of strains", 
                  #  title = paste0( selectedProv$NAME_1, ", ", selectedProv$Country),
                    subtitle = paste("n = ", as.character(sum(selectedComposition$Count)), 
                                     sep = "", collapse = "") ) +
               theme(plot.title = element_text(hjust = -0.9, size = 12),
                     plot.subtitle = element_text(hjust = -0.3, size = 11),
                     legend.position = c(0.7, 1.1),
                     legend.direction = "horizontal")+
               scale_fill_gradient(low = "green", high = "red")+
               coord_flip()
         }
      )
   }) #observeEvent MapPlot3 shape click
   observeEvent(input$MapPlot3_click, { print("map clicked") 
      print(input$MapPlot3_click)
      output$CountrySelection<- NULL
      output$hanna1_bacteria <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_bacteria <- NULL
      output$CompositionPlot_bacteria <- NULL
      output$ricedatabase <- NULL
      output$sweetlines <- NULL
      output$hanna3 <- NULL
      output$hanna1_fungi <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2_fungi <- NULL
      output$CompositionPlot_fungi <- NULL
   }) #ObserveEvent mapplot3_click
   
   session$onSessionEnded(stopApp)
} ### input, output, session



shinyApp(ui = ui, server = server)


