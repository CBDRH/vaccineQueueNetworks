# libraries
library(shinydashboard)
library(tidyverse)
library(queuecomputer)
library(visNetwork)
library(purrr)
library(shinyTime)
library(lubridate)
library(rintrojs)

# Define theme colors
color1 <- "aqua"
colorHex1 <- '#00c0ef'
status1 <- 'info'

color2 <- "green"
colorHex2 <- '#00a65a'
status2 <- 'success'

# Functions
source_files <- c("helperFunctions.R", "arenaModel.R", "gpModel.R")
for (source_file in source_files) {
  source(paste('R/', source_file, sep = ""))
}


# Default data
source_objects <- c("throughputP1", "utilisationP1", "processingP1", "throughputP2", "utilisationP2", "processingP2", 
                    "infoThroughPut1", "infoThroughPut2", "infoProcessTime1", "infoProcessTime2")

if (file.exists(paste('www/', source_objects[1], sep = ""))) {
  
  for (source_object in source_objects) {
    load(paste('www/', source_object, sep = ""))
  }
} else {
  source(paste('R/', 'prepDefaultData.R', sep = ""))
}


