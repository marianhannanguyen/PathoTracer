require(shiny)
require(shinyWidgets)
require(leaflet)
require(tigris)
options(tigris_use_cache = TRUE)
require(sp)
require(dplyr)
require(spdplyr)
#require(dplyr)

#require(GADMTools)
#require(sp)

#require(here)
#require(rgdal)
#require(tidyverse)
#require(wesanderson)
require(htmltools)
#require(raster)
#require(rmapshaper)

#Sys.setlocale(locale="en_US.UTF-8")
#source("dependencies.R")

source("functionsetc.R")

#source("/Users/hannanguyen/Google_Drive/PathoTracer/forLocal.R")



ui <- fluidPage(
  titlePanel("PathoTracer"),
  mainPanel(width = 8,
            leafletOutput("MapPlot1")# this maybe the one causing the
            #error during app loading. Change this to a conditional panel
            ),
  sidebarPanel( width = 4,
                uiOutput('ui.action'),
                fluidRow(),
                h4("More information"),
                h6("(select a location)"),
                htmlOutput("hanna1")
               # uiOutput('ui.action2')
                #fluidRow(column(12, dataTableOutput("LocationOutput")))
                # textAreaInput()
  )
)
server <- function(input, output, session){
  #Sys.setlocale(locale="en_US.UTF-8")
  #Sys.setlocale("LC_ALL", 'en_US.UTF-8') # hehe
 
    output$ui.action<- renderUI({
      if(is.null(allmerged)){
        return()
      } else{
        selectInput("CountryInput", "Select a country",
                    choices = c(sort(as.character(unique(allmerged$Country))), "All data"),
                    selected = "All data"
                    #,multiple = TRUE   ## dont do this, hanna. so messy. TOO MESSY
        )
      }
    })
    
   # output$ui.action2<- renderUI({
  #    if(is.null(allmerged)){
   #     return()
    #  } else{
     #   sliderInput("YearInput", label = h3("Year Range"), 
      #              min = min(allmerged$year), 
       #             max = max(allmerged$year), 
        #            value = c(min(allmerged$year), max(allmerged$year)))
        
     # }
   # })
observe({
  if("All data" %in% input$CountryInput){
   # allmerged <- aggregate(x = subset(allmerged, Year == c(input$YearInput[1]:input$YearInput[2])),
    #                       by = Province)
      output$MapPlot1 <- renderLeaflet({
        philippines %>%
          leaflet()%>%
          addProviderTiles("CartoDB.Positron")%>%
          addPolygons(data= allmerged,
                      layerId = ~NAME_1,# hehehehe this might save you later
                      popup = paste0(
                        "<strong>", allmerged$NAME_1,"</strong>" , "<strong>, </strong>", 
                        "<strong>", allmerged$Country,"</strong>"),
                      fillColor = ~pal3(allmerged$Map.Display),
                      fillOpacity = 1,
                      weight = 1,
                      stroke = TRUE,
                      color = "gray",
                      opacity = 0.9,
                      label = lapply(allmerged$labs, HTML),
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
                        direction = "auto")) %>% 
          addLegend(pal = pal3, 
                    values = allmerged$Map.Display,
                    position = "bottomleft",
                    title = "Predominant population ID")
        
      })
      #   output$LocationOutput <- renderDataTable(allstrains[,names(allstrains)!="labs"])
  observeEvent(input$MapPlot1_shape_click,{
        click <- input$MapPlot1_shape_click
        print(click$lat)
        if(is.null(click$id))
          return()
        print(click$id)
        selectedProv <- allmerged %>% dplyr::filter(NAME_1 == click$id)
        output$hanna1 <- renderUI({
          HTML(paste0(
            "<br/>",
            "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
            "<strong>", selectedProv$Country,"</strong>", "<br/>", #"<br/>",
            "<strong>Predominant ID: </strong>", "<br/>",
            selectedProv$Map.Display, "<br/>","<br/>",
            "<strong>Population ID(s): </strong>", "<br/>",
            as.character(selectedProv$All.PopIDs), "<br/>",
            "<strong>Composition: </strong>", 
            "<br/>",
            as.character(selectedProv$Percent),
            " (n = ", as.character(selectedProv$n), ")","<br/>",
            "<strong>Gene recommendation </strong>: ", "<br/>",
            as.character(selectedProv$Gene.recommendation),"<br/>",
            "<strong>Strain provider(s): </strong>", "<br/>",selectedProv$Institute,
            "<br/>",
            "<strong>Variety/Line recommendation(s): </strong>", "<br/>",
            gsub('"', "",gsub("^c\\(|\\)$", "", 
                              rice %>% dplyr::select(one_of(as.character(selectedProv$Gene.recommendation)))))
            #subset(rice, select = selectedProv$Gene.recommendation)
            
          ))}) 
      })# dulo ng observeEvent
    }else{ 
      output$MapPlot1 <- renderLeaflet({
        filtered <-  allmerged %>% dplyr::filter(Country == input$CountryInput) 
        # filteredstrains <- allstrains %>% filter(Country == input$CountryInput)
        #  output$LocationOutput <- renderDataTable(filteredstrains[,names(filteredstrains)!="labs"])
        observeEvent(input$MapPlot1_shape_click, {
          click <- input$MapPlot1_shape_click
          print(click$lat)
          if(is.null(click$id))
            return()
          print(click$id)
          # selectedProv <-  allmerged %>% filter(Lat == lat| Lng == long) 
          # print(summary(selectedProv))
          selectedProv <- filtered %>% dplyr::filter(NAME_1 == click$id)
          output$hanna1 <- renderUI({
            HTML(paste0(
              "<br/>",
              "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
              "<strong>", selectedProv$Country,"</strong>", "<br/>", #"<br/>",
              "<strong>Predominant ID: </strong>","<br/>",
              selectedProv$Map.Display, "<br/>","<br/>",
              "<strong>Population ID(s): </strong>", "<br/>",
              as.character(selectedProv$All.PopIDs), "<br/>",
              "<strong>Composition: </strong>", 
              "<br/>",
              as.character(selectedProv$Percent),
              " (n = ", as.character(selectedProv$n), ")","<br/>",
              "<strong>Gene recommendation </strong>: ", "<br/>",
              as.character(selectedProv$Gene.recommendation),"<br/>",
              "<strong>Strain provider(s): </strong>", "<br/>",selectedProv$Institute,
              "<br/>",
              "<strong>Variety/Line recommendation(s): </strong>", "<br/>",
              gsub('"', "",gsub("^c\\(|\\)$", "", 
                                rice %>% dplyr::select(one_of(as.character(selectedProv$Gene.recommendation)))))
              
            ))})
        })# dulo ng observeEvent
        popup_all <- paste0(
          "<strong>", filtered$NAME_1,"</strong>" , "<strong>, </strong>", 
          "<strong>", filtered$Country,"</strong>"
          , 
          "<br/>",
          "<strong>Predominant ID: </strong>",filtered$Map.Display, "<br/>","<br/>",
          "<strong>Population ID(s): </strong>",
          as.character(filtered$All.PopIDs), "<br/>",
          "<strong>Composition: </strong>",
          as.character(filtered$Percent),
          " (n = ", as.character(filtered$n), ")","<br/>",
          "<strong>Gene recommendation </strong>: ", 
          as.character(filtered$Gene.recommendation),"<br/>",
          "<strong>Strain provider(s): </strong>", filtered$Institute)
        
        philippines %>%
          leaflet()%>%
          addProviderTiles("CartoDB.Positron")%>%
          addPolygons(data= filtered,
                      layerId = ~NAME_1,# hehehehe this might save you later
                      popup = popup_all,
                      fillColor = ~pal3(filtered$Map.Display),
                      fillOpacity = 1,
                      weight = 1,
                      stroke = TRUE,
                      color = "gray",
                      opacity = 0.9,
                      label = lapply(filtered$labs, HTML),
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
                    position = "bottomleft",
                    title = "Predominant population ID")
      })
    }#dulo ng else
    
  })
  
  
  
  session$onSessionEnded(stopApp)
  
  
  
}

hanna2 <- shinyApp(ui = ui, server = server)
hanna2

