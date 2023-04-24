pkgs <- c("e1071",
          "parallel",
          "preprocessCore",
          "matrixStats", 
          "pheatmap", 
          "RColorBrewer", 
          "tidyverse", 
          "cowplot",
          "ggpubr",
          "bslib",
          "ggthemes",
          "dplyr",
          "tidyr","reshape2",
          "RColorBrewer",
          "pacman",
          "ggpubr",
          "openxlsx")
lapply(pkgs, library, character.only = T)
#install.packages("xlsx")
#library(xlsx)
#library(openxlsx)