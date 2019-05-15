#Sys.setlocale(locale="en_US.UTF-8")
rice <- read.csv("ricerecommendation.csv", header = T,stringsAsFactors = F,
                 na.strings = "")

allstrains <- readRDS("IRRIXo.rds")
simple_cambodia <- readRDS("maps/simple_cambodia.rds")
simple_vietnam <- readRDS("maps/simple_vietnam.rds")
simple_philippines <- readRDS("maps/simple_philippines.rds")
simple_pakistan <- readRDS("maps/simple_pakistan.rds")
simple_indonesia <- readRDS("maps/simple_indonesia.rds")
philippines <- simple_philippines
tryColor <- c('#d73027','#fee08b','#f46d43','#d9ef8b','#a6d96a','#fdae61','#ffffbf','#66bd63','#1a9850','#006837')

names(allstrains)[names(allstrains) == "Map.display"] <- "Map.Display"
pal3<- colorFactor(tryColor, domain = allstrains$Map.Display)
allstrains$labs <- lapply(seq(nrow(allstrains)), function(i) {
  paste0(
    "<strong>", allstrains[i, "Province"],"</strong>" , "<strong>, </strong>", 
    "<strong>", allstrains[i, "Country"],"</strong>", "<br/>",
    "<strong>Predominant ID: </strong>",
    allstrains[i, "Map.Display"], "<br/>", "<br/>",
    "<strong>Population ID(s): </strong>",
    allstrains[i, "All.PopIDs"],"<br/>", 
    "<strong>Composition: </strong>",
    allstrains[i, "Percent"],
    "(n = ",
    allstrains[i, "n"],")","<br/>",
    "<strong>Gene recommendation </strong>: ", 
    allstrains[i, "Gene recommendation"],"<br/>",
    #  as.character(pmerged$Gene.recommendation),"<br/>"
    "<strong>Institute(s): </strong>","<br/>",
    allstrains[i, "Institute"]) })
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




cmerged <- geo_join(simple_cambodia, Cstrains, by = "NAME_1", how = "inner")
vmerged <- geo_join(simple_vietnam, Vstrains, by = "NAME_1", how = "inner")
pmerged<- geo_join(simple_philippines, Pstrains, by = "NAME_1", how = "inner")
pakmerged <- geo_join(simple_pakistan, pakstrains, by = "NAME_2", how = "inner")
indomerged <- geo_join(simple_indonesia, indostrains, by = "NAME_1", how = "inner")
pakmergedscratch <- pakmerged
#pakmergedscratch$NAME_1 <- NULL
#pakmergedscratch$NAME_1 <- pakmergedscratch$NAME_2
# pakmergedscratch$GID_1 <- pakmergedscratch$GID_2
# pakmergedscratch$NL_NAME_1 <- pakmergedscratch$NL_NAME_2
# pakmergedscratch$GID_2 <- NULL
#  pakmergedscratch$VARNAME_2
#  pakmergedscratch$NL_NAME_2 <- NULL
#  pakmergedscratch$NAME_2 <- NULL
allmerged <- raster::bind(cmerged, vmerged, pmerged, pakmergedscratch,indomerged)
popup_all <- paste0(
  "<strong>", allmerged$NAME_1,"</strong>" , "<strong>, </strong>", 
  "<strong>", allmerged$Country,"</strong>", "<br/>",
  "<strong>Predominant ID: </strong>",allmerged$Map.Display, "<br/>","<br/>",
  "<strong>Population ID(s): </strong>",
  as.character(allmerged$All.PopIDs), "<br/>",
  "<strong>Composition: </strong>",
  as.character(allmerged$Percent),
  " (n = ", as.character(allmerged$n), ")","<br/>",
  "<strong>Gene recommendation </strong>: ", 
  as.character(allmerged$Gene.recommendation),"<br/>",
  "<strong>Strain provider(s): </strong>", allmerged$Institute)
