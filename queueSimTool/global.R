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

# hint and guided tour strings

vaccinations_distributions_text <- 'This figure shows the distribution of total vaccinations throughput across the simulation runs.'

processing_times_distribtion_text <- 'This figure shows the distributions of processing times for each station, and overall, across the simulation runs.'

staff_utilisation_text <- 'This figure shows the distributions of staff utilisation for each station across the simulation runs.'

sidebar_overview__text <- 'This app allows you to run simulations for two distinct approaches to vaccine delivery. Here you can choose between two models: (i) a mass vaccination hub or (ii) a GP clinic model.'

number_of_simulations_text <- 'Here you can set the number of simulations. Higher values will result in a longer running time but even 100 simulations should take less than a minute. Smaller values are fine, especially while you are experimenting.'

queue_performance_overview_text <- 'This panel provides a visual overview of the queue performance.'

queue_performance_overview_hint_text <- 'This panel provides a visual overview of the queue performance. The three figures from left-to-right show: a) the distribution of administered vaccinations across the simulations runs; b) the distribution of processing times across the simulation runs for each stations in the queue network, and the total processing time; and c) the distribution of the staff utilisation factor across the simulations runs. Queue performance deteriates when staff utilisation exceeds 80%.'

info_boxes_text <- "This is a high level summary of the queue performance and assumed staff numbers. These figures will update automatically as you run new simulations."

median_number_of_vaccinations_text <- 'This tells you the likely number of vaccinations that could be administered over the specified clinic time.'

median_process_time_text <- 'This tells you the median time from start to finish for a person coming to be vaccinated.'

healthcare_staff_text <- 'This is the number of healthcare staff that are involved in the queue processes. This doesn\'t include other support staff neccessary to run the clinic.'

support_staff_text <- 'This is the number of additional support staff to run the clinic. This figure won\'t affect the model estimates but it is important to consider for clinic planning'

info_boxes_hint_text <- "The four boxes shown here provide a high level summary of the queue performance and assumed staff numbers. These figures will update automatically as you run new simulations. From left-to-right, the boxes show a) the likely number of vaccinations that could be administered over the specified clinic time; b) the median time from start to finish for a person coming to be vaccinated; c) the number of healthcare staff that are involved in the queue processes; d) the number of additional support staff to run the clinic."

service_times_parameters_text <- "Click each node in the network diagram to bring up a dialogue box which allows you to edit the parameters for that node."

service_times_charts_text <- "These charts show the distribution of service times at each station, as specified by the parameters you set. The vertical lines indicate the median process time."

params_tabs_overview_text <- 'This panel has three tabs allowing you to modify all of the model assumptions.'

info_tab_text <- 'To find out more about the underlying queue network models implemented in this app, check out the links here.'
