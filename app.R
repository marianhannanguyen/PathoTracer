library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(tigris)
library(GADMTools)
library(sp)
library(rgdal)
library(tidyverse)
library(wesanderson)
library(htmltools)
library(raster)
library(rmapshaper)

vietnam <- readOGR(dsn = "/Users/hannanguyen/Google_Drive/1_Projects/1_Pathotracer/maps/gadm36_VNM_shp", 
                   layer = "gadm36_VNM_2")
pakistan <- readOGR(dsn = "/Users/hannanguyen/Google_Drive/1_Projects/1_Pathotracer/maps/gadm36_PAK_shp", 
                    layer = "gadm36_PAK_2")
indonesia<- readOGR(dsn = "/Users/hannanguyen/Google_Drive/1_Projects/1_Pathotracer/maps/gadm36_IDN_shp", 
                    layer = "gadm36_IDN_1")
Cambodia <- readOGR(dsn = "/Users/hannanguyen/Google_Drive/1_Projects/1_Pathotracer/maps/gadm36_KHM_shp", 
                    layer = "gadm36_KHM_1")
philippines <- readOGR(dsn = "/Users/hannanguyen/Google_Drive/1_Projects/1_Pathotracer/maps/gadm36_PHL_shp", 
                       layer = "gadm36_PHL_1")

tryColor <- c('#d73027','#f46d43','#fee08b','#fdae61','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850','#006837')
Sys.setlocale("LC_ALL", 'en_US.UTF-8')
allstrains <- read.csv("Strains2.csv", 
                       encoding = "UTF-8",
                       header = TRUE, check.names = FALSE)
allstrains$Province <- replace(as.character(allstrains$Province ), 
                               allstrains$Province  == "West Java", "Jawa Barat")
allstrains$Province <- replace(as.character(allstrains$Province ), 
                               allstrains$Province   == "East Java", "Jawa Timur")
allstrains$Province <- replace(as.character(allstrains$Province ), 
                               allstrains$Province  == "Middle Java", "Jawa Tengah")
allstrains$Province <- replace(as.character(allstrains$Province ), 
                               allstrains$Province == "North Sumatra", "Sumatera Utara")
allstrains$Province  <- replace(as.character(allstrains$Province ), 
                                allstrains$Province == "South Sumatra", "Sumatera Selatan")

allstrains$labs <- lapply(seq(nrow(allstrains)), function(i) {
  paste0(
    "<strong>", allstrains[i, "Province"],"</strong>" , "<strong>, </strong>", 
    "<strong>", allstrains[i, "Country"],"</strong>", "<br/>",
    "<strong>Predominant ID: </strong>",
    allstrains[i, "Map.Display"], "<br/>", "<br/>",
    "<strong>Population ID(s): </strong>",
    allstrains[i, "All PopIDs"],"<br/>", 
    "<strong>Composition: </strong>",
    allstrains[i, "Percent"],
    "(n = ",
    allstrains[i, "n"],")","<br/>",
    "<strong>Gene recommendation </strong>: ", 
    allstrains[i, "Gene recommendation"],"<br/>",
    #  as.character(pmerged$Gene.recommendation),"<br/>"
    "<strong>Institute(s): </strong>","<br/>",
    allstrains[i, "Institute"]) })


pal3<- colorFactor(tryColor, domain = allstrains$Map.Display)

simple_cambodia <- ms_simplify(Cambodia)
simple_vietnam <- ms_simplify(vietnam)
simple_philippines <- ms_simplify(philippines)
simple_pakistan <- ms_simplify(pakistan)
simple_indonesia <- ms_simplify(indonesia)


Cstrains <- subset(allstrains, Country == "Cambodia", drop = TRUE)
Cstrains <- rename(Cstrains, NAME_1 = "Province" )
Vstrains <- subset(allstrains, Country == "Vietnam", drop = TRUE)
Vstrains <- rename(Vstrains, NAME_1 = "Province" )
Pstrains <- subset(allstrains, Country == "Philippines", drop = TRUE)
Pstrains <- rename(Pstrains, NAME_1 = "Province" )
pakstrains <- subset(allstrains, Country == "Pakistan", drop = TRUE)
pakstrains <- rename (pakstrains, NAME_2 = "Province")
indostrains <- subset(allstrains, Country == "Indonesia", drop = TRUE)
indostrains <- rename (indostrains, NAME_1 = "Province")

simple_cambodia <- readRDS(here::here("app/maps","simple_cambodia.rds"))
simple_vietnam <- readRDS(here::here("app/maps/","simple_vietnam.rds"))
simple_philippines <- readRDS(here::here("app/maps/","simple_philippines.rds"))
simple_pakistan <- readRDS(here::here("app/maps/","simple_pakistan.rds"))
simple_indonesia <- readRDS(here::here("app/maps/","simple_indonesia.rds"))

cmerged <- geo_join(simple_cambodia, Cstrains, by = "NAME_1", how = "inner")
vmerged <- geo_join(simple_vietnam, Vstrains, by = "NAME_1", how = "inner")
pmerged<- geo_join(simple_philippines, Pstrains, by = "NAME_1", how = "inner")
pakmerged <- geo_join(simple_pakistan, pakstrains, by = "NAME_2", how = "inner")
indomerged <- geo_join(simple_indonesia, indostrains, by = "NAME_1", how = "inner")

pakmergedscratch <- pakmerged
pakmergedscratch$NAME_1 <- NULL
pakmergedscratch$NAME_1 <- pakmergedscratch$NAME_2


pakmergedscratch$GID_1 <- pakmergedscratch$GID_2
pakmergedscratch$NL_NAME_1 <- pakmergedscratch$NL_NAME_2

pakmergedscratch$GID_2 <- NULL

pakmergedscratch$NL_NAME_2 <- NULL
pakmergedscratch$NAME_2 <- NULL

allmerged <- bind(cmerged, vmerged, pmerged, pakmergedscratch,indomerged)

mhn <- function(var){
  require(shiny)
  require(shinyWidgets)
  require(leaflet)
  require(dplyr)
  require(sf)
  require(spdplyr)
  require(rgdal)
  
  
  shinyApp(ui = fluidPage(
    titlePanel(""),
    mainPanel(width = 8,
              leafletOutput("MapPlot1")  ),
    sidebarPanel( width = 4,
                  selectInput("CountryInput", "Select a country",
                              choices = c(sort(as.character(unique(allmerged$Country))), "All data"),
                              selected = "All data"
                              #,multiple = TRUE   ## dont do this, hanna. so messy. TOO MESSY
                  ),
                  fluidRow(),
                  h4("More information"),
                  h6("select a location"),
                  htmlOutput("hanna1")
                  #fluidRow(column(12, dataTableOutput("LocationOutput")))
                  # textAreaInput()
    )
  ),
  server = function(input, output){
    Sys.setlocale("LC_ALL", 'en_US.UTF-8') # hehe
    observe({
      if("All data" %in% input$CountryInput){
        popup_all <- paste0(
          "<strong>", allmerged$NAME_1,"</strong>" , "<strong>, </strong>", 
          "<strong>", allmerged$Country,"</strong>")
        output$MapPlot1 <- renderLeaflet({
          ####
          philippines %>%
            leaflet()%>%
            addProviderTiles("CartoDB.Positron")%>%
            addPolygons(data= allmerged,
                        layerId = ~NAME_1,# hehehehe this might save you later
                        popup = popup_all,
                        fillColor = ~pal3(allmerged$Map.Display),
                        fillOpacity = 0.7,
                        weight = 0.4,
                        stroke = TRUE,
                        color = "#BDBDC3",
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
                          direction = "auto")) 
          #%>% 
          #  addLegend(pal = pal3, 
          #           values = allmerged$Map.Display,
          #          position = "bottomleft",
          #         title = "Predominant population ID")
          ####
        })
        #   output$LocationOutput <- renderDataTable(allstrains[,names(allstrains)!="labs"])
        observeEvent(input$MapPlot1_shape_click, {
          click <- input$MapPlot1_shape_click
          print(click$lat)
          if(is.null(click$id))
            return()
          print(click$id)
          selectedProv <- allmerged %>% filter(NAME_1 == click$id)
          output$hanna1 <- renderUI({
            HTML(paste0(
              "<br/>",
              "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
              "<strong>", selectedProv$Country,"</strong>", "<br/>", "<br/>",
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
                                rice %>% select(one_of(as.character(selectedProv$Gene.recommendation)))))
              #subset(rice, select = selectedProv$Gene.recommendation)
              
            ))}) 
        })# dulo ng observeEvent
      }else{ 
        output$MapPlot1 <- renderLeaflet({
          filtered <-  allmerged %>% filter(Country == input$CountryInput) 
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
            selectedProv <- filtered %>% filter(NAME_1 == click$id)
            output$hanna1 <- renderUI({
              HTML(paste0(
                "<br/>",
                "<strong>", selectedProv$NAME_1,"</strong>" , "<strong>, </strong>", 
                "<strong>", selectedProv$Country,"</strong>", "<br/>", "<br/>",
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
                                  rice %>% select(one_of(as.character(selectedProv$Gene.recommendation)))))
                
              ))})
          })# dulo ng observeEvent
          popup_all <- paste0(
            "<strong>", filtered$NAME_1,"</strong>" , "<strong>, </strong>", 
            "<strong>", filtered$Country,"</strong>"
            #, 
            #"<br/>",
            #"<strong>Predominant ID: </strong>",filtered$Map.Display, "<br/>","<br/>",
            #"<strong>Population ID(s): </strong>",
            #as.character(filtered$All.PopIDs), "<br/>",
            #"<strong>Composition: </strong>",
            #as.character(filtered$Percent),
            #" (n = ", as.character(filtered$n), ")","<br/>",
            #"<strong>Gene recommendation </strong>: ", 
            #as.character(filtered$Gene.recommendation),"<br/>",
            #"<strong>Institute: </strong>", filtered$Institute)
          )
          philippines %>%
            leaflet()%>%
            addProviderTiles("CartoDB.Positron")%>%
            addPolygons(data= filtered,
                        layerId = ~NAME_1,# hehehehe this might save you later
                        popup = popup_all,
                        fillColor = ~pal3(filtered$Map.Display),
                        fillOpacity = 0.7,
                        weight = 0.4,
                        stroke = TRUE,
                        color = "#BDBDC3",
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
                          direction = "auto"))
          #%>% 
          # addLegend(pal = pal3, 
          #          values = allmerged$Map.Display,
          #         position = "bottomleft",
          #        title = "Predominant population ID")
        })
      }#dulo ng else
    })
  }
  
  )#dulo ng shinyApp
  
  
  
  
  
  
}

mhn(allmerged)

