dependencies <- read.csv(textConnection("
              Package, Min.Version
              shiny, 1.1.0
              rgdal, 1.3-4
              leaflet, 2.0.2
              rmapshaper, 0.4.1
              tigris, 0.7
              wesanderson, 0.3.6
              htmltools, 0.3.6
              dplyr, 0.7.8
              sp, 1.3-1
              tidyverse, 1.2.1
              here, 0.1
              spdplyr, 0.2.0
                                        
                                        "),
                         stringsAsFactors = FALSE, strip.white = TRUE)

# Import installed package versions
pkgs <- installed.packages()
rownames(pkgs) <- c()
pkgs <- data.frame(pkgs, stringsAsFactors = FALSE)

# Compare requirements to installed packages
pkgs <- merge(dependencies, pkgs, by="Package", all.x=TRUE)

# Filter out packages meeting minimum version requirement
pkgs <- pkgs[mapply(compareVersion, pkgs$Min.Version, pkgs$Version) > 0, ]

# Install missing and newer packages
cran <- pkgs[pkgs$source=='CRAN', ]
null <- lapply(cran$Package, install.packages)
github <- pkgs[pkgs$source=='github', ]
null <- lapply(cran$Package, devtools::install_github)

# Require dependencies [optional]
lapply(dependencies$Package, require, character.only=TRUE)
