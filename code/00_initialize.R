library(kevostools)
library(modelsummary)
library(sf)
library(rgdal)
library(splitstackshape)
library(tigris)
library(cowplot)
library(tidycensus)
library(tidyverse)
library(data.table)
library(ggeffects)

save <- c("cleanup", "theme_bc", "save")
options("modelsummary_format_numeric_latex" = "plain")

cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
  gc()
}