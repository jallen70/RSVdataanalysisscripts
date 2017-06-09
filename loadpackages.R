# R script to load packages
loadpackages <- function()
  
#install the packages you require
if (!require(plyr)){
    install.packages("plyr", repos="http://cran.rstudio.com/") 
    library("plyr")
}

if (!require(scales)){
  install.packages("scales", repos="http://cran.rstudio.com/") 
  library("scales")
}

if (!require("ggplot2")) {
  install.packages("ggplot2", repos="http://cran.rstudio.com/") 
  library("ggplot2")
}

if (!require("VennDiagram")) {
  install.packages("VennDiagram", repos="http://cran.rstudio.com/") 
  library("VennDiagram")
}

if (!require("rmeta")) {
  install.packages("rmeta", repos="http://cran.rstudio.com/") 
  library("rmeta")
}

if (!require("stringr")) {
  install.packages("stringr", repos="http://cran.rstudio.com/") 
  library("stringr")
}

if (!require("xtable")) {
  install.packages("xtable", repos="http://cran.rstudio.com/") 
  library("xtable")
}

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}