require(shiny)
require(tigris)
options(tigris_use_cache = TRUE)
require(rgdal)
require(shinyWidgets)
require(leaflet)
require(sp)
require(dplyr)
require(spdplyr)
require(leaflet.extras)
require(shinydashboard)
require(graphics)
require(htmltools)
require(matrixStats)
require(viridis)
require(ggplot2)
require(scales)
require(wesanderson)
require(shinyjs)

source("Functions_v3_upload.R")
#source("/Users/hannanguyen/Google_Drive/PathoTracer/Database/Data_test_filter/Functions_v2.R")

ui <- dashboardPage( 
                    skin ="green",
                    dashboardHeader(title = "PathoTracer",
                                    titleWidth = 300),
                    dashboardSidebar(
                      width =300,
                                     sidebarMenu(
                                       id = "trouble",
                                     menuItem("SNP-based populations", tabName = "SNP", icon = icon("circle-notch")),
                                     menuItem("Sweet gene induction", icon = icon("leaf"), tabName = "Sweet", 
                                                badgeLabel = "new", badgeColor = "green")),
                                     #uiOutput('CountrySelectionUI'),
                                     conditionalPanel(
                                      "input.trouble == 'SNP' ",
                                      selectInput('CountrySelection', label = "Select a country",
                                                  selected = "All data",
                                                  choices = forChoices,
                                                  multiple = FALSE,
                                                  width = 300)
                                     ),conditionalPanel(
                                       "input.trouble == 'Sweet' ",
                                       fluidRow()
                                     ),
                                     uiOutput('YearSelection'),
                                    # fluidRow( 
                                     #   column(8, offset =1, 
                                      #          sliderInput(inputId = 'YearInput', 
                                       #                     label = "Select year to plot",
                                        #                    min = min(AllYears, na.rm = TRUE),
                                         #                   max = max(AllYears, na.rm = TRUE), 
                                          #                  value = c(min(AllYears, na.rm = TRUE),max(AllYears, na.rm = TRUE)   ),
                                           #                 step = 1,
                                            #               ticks = TRUE,
                                             #               sep = ""
                                        #        ) 
                                         #) 
                                     #),
                                     fluidRow( ),
                                     fluidRow(column(8, offset = 1, h4("More information"))),
                                     fluidRow(column(8, offset = 1, htmlOutput("hanna1"))),
                                     fluidRow(column(10, offset = 1, plotOutput("CompositionPlot", width="100%",height=250) )),
                                     fluidRow( ),
                                     fluidRow(column(8, offset = 1, htmlOutput("hanna2"))),
                                     fluidRow( )
                    ),
                    dashboardBody( 
                      tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {Shiny.onInputChange(variableName, null);});"),
                  #    tags$script(HTML("$('body').addClass('fixed');")),
                      tags$style(
                        type = "text/css", 
                                 "#map {height: calc(90vh - 53px) !important;}"),
                       #  "html, body {width:100%;height:80% !important;}"),
                                  tags$head(
                                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                                  tabItems(
                                    tabItem(tabName = "SNP",
                                         #   h5("SNP-based populations"),
                                            fluidRow(leafletOutput("MapPlot1",width="100%",height="680"))
                                            #,
                                            #conditionalPanel(
                                            #  condition = "is.null(click$id) == 'FALSE'",
                                            #  fluidRow(absolutePanel(id="controls",
                                            #                top = 100, left = 300, right = "auto", bottom = "auto",
                                            #                width = 330, height = "auto",
                                            #                style = "opacity: 0.65; z-index: 1000;" ,
                                            #                class = "panel panel-default",
                                            #                draggable = TRUE, 
                                            #                htmlOutput("hanna2"),
                                            #                HTML("<b>Add CSV Point Data:</b><br>"),
                                            #                HTML("<b>Add CSV Point Data:</b><br>"),
                                            #                HTML("<b>Add CSV Point Data:</b><br>"),
                                            #                HTML("<b>Add CSV Point Data:</b><br>"))))
                                            )
                                           ,
                                    tabItem(tabName = "Sweet",
                                            h5("1972 - 2012 data"),
                                          #  h5("SWEET Gene Induction"),
                                            fluidRow(leafletOutput("MapPlot2",width="100%",height="680")))
                                    ),
                                    fluidRow()
                                  )
                                  )#dashboardBody closing parenthesis
                    

server <- function(input, output, session){
  #observe({ 
  #   print(input$YearInput)
   #   print(input$YearInput[1])
  #    print(input$YearInput[2])
   #   SelectedYear <- input$YearInput
    #  updateSliderInput(session, 
     #                   inputId = 'YearInput', 
      #                  label = "Select year to plot",
       #                 min = input$YearInput[1],
        #                max = input$YearInput[2], 
         #               value = SelectedYear,
          #              step = 1
      #) 
   #})
#Storing the selected years as a variable, used to filter the dataset
    observeEvent(input$YearInput,{
        output$YearSelection<- renderUI({
             if(is.null(AllYears)){
               selectInput("YearSelection", "Select a year",
                           choices = "All data",
                           selected = "All years")
            }else{
            selectInput("YearInput", "Select a year",
                        choices = c(AllYears, "All years"),
                        selected = "All years"
            )
          }
        })
    }) # end of observeEvent for YearInput. Data is filtered based on selected Year(s)
    
############ uiOutput in sidebar must change, depending on selected tab
observe({
      if(input$trouble == "Sweet"){
        shinyjs::reset("click")
        shinyjs::reset("selectedProv")
        shinyjs::reset("selectedProvName") 
        shinyjs::reset("selectedComposition") 
        shinyjs::reset("Provider") 
        shinyjs::reset("CompositionPlot") 
        shinyjs::reset("hanna1") 
        shinyjs::reset("hanna2") 
        shinyjs::reset("selectedCompositiongenes") 
        shinyjs::reset("click$id")
        pal3 <- NULL
        click <- NULL
        selectedProv <- NULL
        selectedProvName <- NULL
        selectedComposition <- NULL
        Provider <- NULL
        selectedCompositiongenes <- NULL
       # output$CountrySelectionUI<- NULL
        output$CountrySelection<- NULL
        output$hanna1 <- renderUI({HTML(paste0("(Click a location)"))})
        output$hanna2 <- NULL
        output$CompositionPlot <- NULL
        tryColor <- c(
          wesanderson::wes_palette("Rushmore1",  5, type = c("discrete", "continuous")),
          wesanderson::wes_palette("Cavalcanti1",  5, type = c("discrete", "continuous")),
          wesanderson::wes_palette("Darjeeling2",  5, type = c("discrete", "continuous"))
        )
        pal3<- colorFactor(tryColor, levels = as.character(sweetmap$Map.Display), ordered = TRUE)
        percentpal <- colorQuantile(tryColor,domain = 0:100, n =5)
        #SWEEEETTT
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
   output$hanna1 <- renderUI({
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
        }) # observe event
    observeEvent(input$MapPlot2_click, { print("map clicked") 
     # output$CountrySelectionUI<- NULL
      output$hanna1 <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2 <- NULL
      output$CompositionPlot <- NULL
      })
      }# end of if statement for SWEEEEET
#########################################################################  
##################### IF INPUT$TROUBLE == "SNP" #########################
#########################################################################
  if(input$trouble == "SNP"){
 click <- NULL
 selectedProv <- NULL
 selectedProvName <- NULL
 selectedComposition <- NULL
 Provider <- NULL
 selectedCompositiongenes <- NULL
observe({
  if("All data" %in% input$CountrySelection){
    output$CountrySelection<- NULL
    output$hanna1 <- renderUI({HTML(paste0("(Click a location)"))})
    output$hanna2 <- NULL
    output$CompositionPlot <- NULL
    Genes <-NULL 
    output$MapPlot1 <- renderLeaflet({
      #   world$zoom <- sample(1:8, size = length(world$NAME_1), replace =T) # zoom
      world %>%
        leaflet()%>%
        addProviderTiles("Stamen.TerrainBackground")%>%
        addProviderTiles("CartoDB.PositronOnlyLabels")%>%
        addPolygons(data= allmerged,
                    layerId = ~NAME_1,# hehehehe this might save you later
                    popup = paste0(
                      "<strong>", allmerged$NAME_1,"</strong>" , "<strong>, </strong>", 
                      "<strong>", allmerged$Country,"</strong>"),
                    fillColor = ~pal3(allmerged$Map.Display),
                    fillOpacity = 1,
                    weight = 0.8,
                    stroke = TRUE,
                    color = "black",
                    opacity = 0.9,
                    #     label = lapply(allmerged$labs, HTML),
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
    }) #renderleaflet
    output$hanna1 <- renderUI({HTML(paste0("(Click on a location)"))})
    observeEvent(input$MapPlot1_shape_click,{
      click <- input$MapPlot1_shape_click
      print(click$lat)
      if(is.null(click$id))
        return()
      print(click$id)
      selectedProv <- allmerged %>% dplyr::filter(NAME_1 == click$id)
      #added these for zooming in when a province is clicked
      selectedProvName <- click$id
      leafletProxy("MapPlot1") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 6)
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
      ifelse(is.null(click$id)||is.null(selectedComposition)||is.null(selectedCompositiongenes)||is.null(selectedProv),
             return(NULL), ifelse(sum(selectedComposition$Count)<=10||selectedProv$Map.Display == "Mixed population",
                                  Genes <- "Not enough data for recommendation",
                                  Genes <- as.vector(pull(dplyr::left_join(x = selectedCompositiongenes,
                                                                           y = geneRec, 
                                                                           by = c("PopIDmod"="Map.Display"), Gene.recommendation)))
             ))
      
      output$hanna1 <- renderUI({
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
      output$hanna2 <- renderUI({
        if(is.null(click$id)||is.null(Genes)){
          return()
        } else{
          HTML(paste0(
            "<strong>Strain provider(s): </strong>", "<br/>",Provider,
            "<br/>",
            "<strong>Gene recommendation: </strong>", "<br/>",
            as.character(Genes),"<br/>",
            "<br/>","<strong>Variety/Line recommendation(s): </strong>", "<br/>",
            gsub('"', "",gsub("^c\\(|\\)$", "", 
                              rice %>% dplyr::select(one_of(as.character(Genes)))))
          ))
        }
        
      }) # hanna2
      output$CompositionPlot <- renderPlot(
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
    })# dulo ng observeEvent
  }else{ ### If a country is selected
  
    output$hanna1 <- renderUI({HTML(paste0("(Click a location)"))})
    output$hanna2 <- NULL
    output$CompositionPlot <- NULL
    
    output$MapPlot1 <- renderLeaflet({
      filtered <-  allmerged %>% dplyr::filter(Country == input$CountrySelection)
      output$hanna2 <- renderUI({
        if(is.null(click$id)||is.null(Genes)){
          return()
        } else{
          HTML(paste0(
            "<strong>Strain provider(s): </strong>", "<br/>",Provider,
            "<br/>",
            "<strong>Gene recommendation: </strong>", "<br/>",
            as.character(Genes),"<br/>",
            "<br/>","<strong>Variety/Line recommendation(s): </strong>", "<br/>",
            gsub('"', "",gsub("^c\\(|\\)$", "", 
                              rice %>% dplyr::select(one_of(as.character(Genes)))))
            #subset(rice, select = selectedProv$Gene.recommendation)
            
          ))
        }
      }) # hanna2
    
      observeEvent(input$MapPlot1_shape_click, {
        click <- input$MapPlot1_shape_click
        print(click$lat)
        if(is.null(click$id))
          return()
        print(click$id)
        selectedProv <- filtered %>% dplyr::filter(NAME_1 == click$id)
        #added these for zooming in when a province is clicked
        #selectedProvName <- which(allmerged$NAME_1 == click$id)
        #z <- world$zoom[[selectedProvName]]
        #selectedProvName <- world$NAME_1[[selectedProvName]]
        #  leafletProxy("MapPlot1") %>%
        #       setView(lng = click$lng, lat = click$lat, zoom = 6.5)
        
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
  
        ifelse(is.null(click$id)||is.null(selectedComposition)||is.null(selectedCompositiongenes)||is.null(selectedProv),
               return(NULL), ifelse(sum(selectedComposition$Count)<=10||selectedProv$Map.Display == "Mixed population",
                                    Genes <- "Not enough data for recommendation",
                                    Genes <- as.vector(pull(dplyr::left_join(x = selectedCompositiongenes,
                                                                             y = geneRec, 
                                                                             by = c("PopIDmod"="Map.Display"), Gene.recommendation)))
               ))
        output$hanna1 <- renderUI({
          if(is.null(click$id)){
            HTML(paste0("(Select a location)"))
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
        output$hanna2 <- renderUI({
          if(is.null(click$id)||is.null(Genes)){
            return()
          } else{
            HTML(paste0(
              "<strong>Strain provider(s): </strong>", "<br/>",Provider,
              "<br/>",
              "<strong>Gene recommendation: </strong>", "<br/>",
              as.character(Genes),"<br/>",
              "<br/>","<strong>Variety/Line recommendation(s): </strong>", "<br/>",
              gsub('"', "",gsub("^c\\(|\\)$", "", 
                                rice %>% dplyr::select(one_of(as.character(Genes)))))
              #subset(rice, select = selectedProv$Gene.recommendation)
              
            ))
          }
        }) # hanna2
      })# dulo ng observeEvent
      popup_all <- paste0(
        "<strong>", filtered$NAME_1,"</strong>" , "<strong>, </strong>", 
        "<strong>", filtered$Country,"</strong>"
      )
      world %>%
        leaflet()%>%
        addProviderTiles("Stamen.TerrainBackground")%>%
        addProviderTiles("CartoDB.PositronOnlyLabels")%>%
        addPolygons(data= filtered,
                    layerId = ~NAME_1,# hehehehe this might save you later
                    popup = popup_all,
                    fillColor = ~pal3(filtered$Map.Display),
                    fillOpacity = 1,
                    weight = 1,
                    stroke = TRUE,
                    color = "gray",
                    opacity = 0.9,
                    # label = lapply(filtered$labs, HTML),
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
        addLegend(pal = pal3, 
                  values = allmerged$Map.Display,
                  position = "topright",
                  title = "Predominant population ID", 
                  opacity = 0.9)%>%
        addResetMapButton()
    })
  }#dulo ng else
  
}) #observe   if("All data" %in% input$CountrySelection){
    output$hanna1 <- renderUI({HTML(paste0("(Click on a location)"))})
    pal3<- colorFactor(tryColor, levels = colorPops$Map.Display, ordered = TRUE) 
    observeEvent(input$MapPlot1_shape_click,{
      #output$CountrySelection<- renderUI({
       # if(is.null(input$CountrySelection)){
      #    selectInput("CountrySelection", "Select a country",
      #                choices = forChoices,
       #               selected = "All data")
      #  }else{
       #   selectInput("CountrySelection", "Select a country",
        ##              choices = forChoices,
          #            selected = input$CountrySelection
       #   )
      #  }
   #   })
      click <- input$MapPlot1_shape_click
      print(click$lat)
      if(is.null(click$id))
        return()
      print(click$id)
      selectedProv <- allmerged %>% dplyr::filter(NAME_1 == click$id)
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
      ifelse(is.null(click$id)||is.null(selectedComposition)||is.null(selectedCompositiongenes)||is.null(selectedProv),
             return(NULL), ifelse(sum(selectedComposition$Count)<=10||selectedProv$Map.Display == "Mixed population",
                                  Genes <- "Not enough data for recommendation",
                                  Genes <- as.vector(pull(dplyr::left_join(x = selectedCompositiongenes,
                                                                           y = geneRec, 
                                                                           by = c("PopIDmod"="Map.Display"), Gene.recommendation)))
             ))
      output$hanna1 <- renderUI({
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
      output$hanna2 <- renderUI({
        if(is.null(click$id)||is.null(Genes)){
          return()
        } else{
          HTML(paste0(
            "<strong>Strain provider(s): </strong>", "<br/>",Provider,
            "<br/>",
            "<strong>Gene recommendation: </strong>", "<br/>",
            as.character(Genes),"<br/>",
            "<br/>","<strong>Variety/Line recommendation(s): </strong>", "<br/>",
            gsub('"', "",gsub("^c\\(|\\)$", "", 
                              rice %>% dplyr::select(one_of(as.character(Genes)))))
          ))
        }
        
      }) # hanna2
      output$CompositionPlot <- renderPlot(
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
    })# dulo ng observeEvent
    observeEvent(input$MapPlot1_click, { print("map clicked") 
      print(input$MapPlot1_click)
      output$CountrySelection<- NULL
      output$hanna1 <- renderUI({HTML(paste0("(Click a location)"))})
      output$hanna2 <- NULL
      output$CompositionPlot <- NULL
      
    })
    #######################################
    ###### COUNTRY SELECTION EVENT ########
    #######################################
    observeEvent(input$CountrySelection,
                 {  print("selection changed") 
                   print(paste0("You have selected ", input$CountrySelection))
                   shinyjs::reset("click")
                   shinyjs::reset("selectedProv")
                   shinyjs::reset("selectedProvName") 
                   shinyjs::reset("selectedComposition") 
                   shinyjs::reset("Provider") 
                   shinyjs::reset("CompositionPlot") 
                   shinyjs::reset("hanna1") 
                   shinyjs::reset("hanna2") 
                   shinyjs::reset("selectedCompositiongenes") 
                   shinyjs::reset("click$id")
               #    click <- NULL
                #   selectedProv <- NULL
                 #  selectedProvName <- NULL
                  # selectedComposition <- NULL
                   #Provider <- NULL
                  # selectedCompositiongenes <- NULL
                   # output$CountrySelectionUI<- NULL
                 click$id <-NULL
                 output$hanna1 <- renderUI({HTML(paste0("(Click a location)"))})
                 Genes <- NULL
                 }        
    )#observeEvent CountrySelection
    
  }# trouble == SNP
    })
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
                    label = htmltools::HTML(paste0(
                      "<strong>", sweetmap$NAME_1,"</strong>" , "<strong>, </strong>", 
                      "<strong>", sweetmap$Country,"</strong>","<br/>"
                      
                    )),
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
    })
    session$onSessionEnded(stopApp)
}

hanna2 <- shinyApp(ui = ui, server = server)
hanna2