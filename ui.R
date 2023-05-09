# CODIGO MAPAS ENTREGAS KOLIFE

#install.packages("leaflet")
library(leaflet)
library(dplyr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(hms)
library(tidyr)
library(readxl)
library(kamila)
library(rpart)
library(doBy)
library(InformationValue) 
library(FSelectorRcpp)
library(cluster)
library(StatMatch)
library(caret)
library(e1071)
#install.packages("kamila")
#install.packages("InformationValue") 
library(readxl)
library(shiny)

shinyUI(fluidPage(
  fluidRow(
    leafletOutput("map",height="80vh"),
    #fluidRow( heigth="10px",
    sliderInput("date", "Fecha", min = 1, max = 31, value = 1)
)
)
)