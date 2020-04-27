library(rmarkdown)

Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

setwd("C:\\Users\\CONNO\\OneDrive\\Documents\\GitHub\\COVID")

rmarkdown::render_site(encoding = 'UTF-8')
