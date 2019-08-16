#file.path(outpath_prefix, "function1results.xls")

data <- "/Users/hannanguyen/Google_Drive/PathoTracer/Database/Data_test_filter/data/"
maps <- "/Users/hannanguyen/Google_Drive/PathoTracer/Database/Data_test_filter/maps/"
Modes <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x,ux)))]
}

### world 
simple_cambodia <- readRDS(file.path(maps, "simple_cambodia.rds"))
simple_vietnam <- readRDS(file.path(maps, "simple_vietnam.rds"))
simple_philippines <- readRDS(file.path(maps, "simple_philippines.rds"))
simple_pakistan <- readRDS(file.path(maps, "simple_pakistan.rds"))

simple_pakistan$NAME_1 <- NULL
simple_pakistan$NAME_1 <- simple_pakistan$NAME_2
simple_pakistan$GID_1 <- simple_pakistan$GID_2
simple_pakistan$NL_NAME_1 <- simple_pakistan$NL_NAME_2
simple_pakistan$GID_2 <- NULL
simple_pakistan$VARNAME_2
simple_pakistan$NL_NAME_2 <- NULL
simple_pakistan$NAME_2 <- NULL
simple_indonesia <- readRDS(file.path(maps, "simple_indonesia.rds"))
#simple_india <- readRDS(file.path(maps, "India.rds"))
simple_myanmar <- readRDS(file.path(maps, "Myanmar.rds"))
world <- raster::bind(simple_cambodia, simple_vietnam, simple_pakistan, simple_philippines, simple_indonesia, 
                      # simple_india, 
                      simple_myanmar)

###
geneRec <- readRDS(file.path(data, "geneRec.rds"))


##
##


forChoices <- readRDS(file.path(data, "forChoices.rds"))
Inst <- readRDS(file.path(data, "Inst.rds"))
## strains
strains <- readRDS(file.path(data, "strains.rds"))


## 
#asfda
asfda <- readRDS(file.path(data, "asfda.rds"))

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
#asfda$Country <- as.character(asfda$Country)
#asfda <- as.data.frame(asfda)

#pal3<- colorFactor(tryColor, domain = colorPops$Map.Display)
pal3<- colorFactor(tryColor, levels = colorPops$Map.Display, ordered = TRUE)

#asfdacolor
asfda$Color <- pull(dplyr::left_join(x = asfda[,"Map.Display"],
                                   y = colorPops, by = "Map.Display"), Color)

#str(asfda$Color)



# allmerged
allmerged <- tigris::geo_join(world, dplyr::select(strains, Country, NAME_1,  Map.Display), by = "NAME_1", how = "inner")

#


rice <- read.csv("/Users/hannanguyen/Google_Drive/PathoTracer/Database/Data_test_filter/ricerecommendation.csv", header = T, stringsAsFactors = F,
                 na.strings = "")
rice$Not.enough.data.for.recommendation <- rice$"Not enough data for recommendation"

#sweets

sweetsummary <- readRDS(file.path(data, "sweetsummary.rds"))
sweetmap <- readRDS(file.path(data, "sweetmap.rds"))






