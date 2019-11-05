Modes <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}



simple_cambodia <- readRDS("maps/simple_cambodia.rds")
simple_vietnam <- readRDS("maps/simple_vietnam.rds")
simple_philippines <- readRDS("maps/simple_philippines.rds")
simple_pakistan <- readRDS("maps/simple_pakistan.rds")
simple_pakistan$NAME_1 <- NULL
simple_pakistan$NAME_1 <- simple_pakistan$NAME_2
simple_pakistan$GID_1 <- simple_pakistan$GID_2
simple_pakistan$NL_NAME_1 <- simple_pakistan$NL_NAME_2
simple_pakistan$GID_2 <- NULL
simple_pakistan$VARNAME_2
simple_pakistan$NL_NAME_2 <- NULL
simple_pakistan$NAME_2 <- NULL
simple_indonesia <- readRDS("maps/simple_indonesia.rds")
#simple_india <- readRDS("maps/India.rds")
simple_myanmar <-readRDS("maps/Myanmar.rds")
world <- raster::bind(simple_cambodia, simple_vietnam, simple_pakistan, simple_philippines, simple_indonesia, 
                      # simple_india, 
                      simple_myanmar)

# Gene recommendations
geneRec <- readRDS("data/geneRec.rds")
# misc
forChoices <- readRDS("data/forChoices.rds")
Inst <- readRDS("data/Inst.rds")
# Strains
strains <- readRDS("data/strains.rds")
asfda <- readRDS("data/asfda.rds")

#dealing with the colors

#mappedPops <- setNames(as.data.frame(levels(as.factor(allmerged@data$Map.Display))), "Map.Display")
mappedPops <- setNames(as.data.frame(levels(as.factor(asfda$PopID))), "Map.Display") 
# append "population ID"

#need to append the other values from Map.Display
mappedPops <- mappedPops %>% 
  mutate(placeholder =
           case_when(
             Map.Display == "others" ~ "others",
             Map.Display == "uncharacterized" ~ "uncharacterized",
             Map.Display != "others" & Map.Display!= "uncharacterized" ~ paste("population ID", Map.Display)
           ))
mappedPops$Map.Display <- NULL
mappedPops$Map.Display <- mappedPops$placeholder
mappedPops$placeholder <- NULL
str(mappedPops)
rownames(mappedPops)
mappedPops[c(1+nrow(mappedPops), 2+nrow(mappedPops)),] <- c("Insufficient data, n <= 10", "Mixed population")
mappedPops
#tryColor <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', 
#         '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8', '#800000')
#tryColor <- viridis(nrow(mappedPops), alpha = 1, option = "E", direction = -1)

#tryColor <- c(
# wesanderson::wes_palette("Rushmore1",  5, type = c("discrete", "continuous")),
#  wesanderson::wes_palette("Cavalcanti1",  5, type = c("discrete", "continuous")),
#  wesanderson::wes_palette("Darjeeling2",  5, type = c("discrete", "continuous"))
#)


tryColor <- c(
  "#26005C", "#EBA828", "#486400", "#E73800", "#221306",
  "#B7767D", "#B7767D", "#09876F", "#3773D0", "#FF472E",
  "#AAB416", "#31463C", "#248381", "#0B5C52"
)



#color_nrow <- as.data.frame(tryColor[1:nrow(as.data.frame(mappedPops))])
colorPops<-cbind(Map.Display = mappedPops, tryColor) 
#colorPops <- cbind(Map.Display = mappedPops, color_nrow) 
colnames(colorPops)[1] <- "Map.Display"
colorPops$Map.Display <- as.character(colorPops$Map.Display)
colnames(colorPops)[2] <- "Color"
colorPops
#pal3<- colorFactor(tryColor, domain = colorPops$Map.Display)
pal3<- colorFactor(tryColor, levels = colorPops$Map.Display, ordered = TRUE)
#asfda$Country <- as.character(asfda$Country)
#asfda <- as.data.frame(asfda)
#str(asfda)
#asfdacolor
asfda$Color <- pull(dplyr::left_join(x = asfda[,"Map.Display"],
                                     y = colorPops, by = "Map.Display"), Color)

#str(asfda$Color)


# allmerged
allmerged <- tigris::geo_join(world, dplyr::select(strains, Country, NAME_1,  Map.Display), by = "NAME_1", how = "inner")

rice <- read.csv("ricerecommendation.csv", header = T, stringsAsFactors = F,
                 na.strings = "")
rice$Not.enough.data.for.recommendation <- rice$"Not enough data for recommendation"


#sweets
sweetsummary <- readRDS("data/sweetsummary.rds")
sweetmap <- readRDS("data/sweetmap.rds")


# blast
blastmerged <- readRDS("data/blastmerged.rds")
blastafdsa <- readRDS("data/blastafdsa.rds")
blast <- readRDS("data/blast.rds")
genenames <- readRDS("data/genenames.rds")
