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

marker_icon <- makeIcon(
  iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-icon.png",
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-shadow.png",
  iconWidth = 10, iconHeight = 10,
)


dfi=read_excel("Event_2022_03.xlsx")

colnames(dfi)= c("Fecha_Plan","Escenario","Deposito","Usuario",         
                 "Vehiculo",  "Nombre_Evento","Lat","Lng",             
                 "Fecha" , "Hora","Duracion","Nombre_Direccion")

events=c("Ruta de inicio","Entrega aprobada","Entrega rechazada")

df=dfi %>% filter(Nombre_Evento %in% events)
#df = df %>% filter(Fecha=="2022-03-15")

df$NumVeh=substr(df$Vehiculo,nchar(df$Vehiculo),nchar(df$Vehiculo)) %>%
  as.numeric() 
df$DiaMes=substr(df$Fecha,nchar(df$Fecha)-1,nchar(df$Fecha)) %>%
  as.numeric() #%>% as.character()
df$AMPM=substr(df$Escenario,1,2) %>% toupper()

df=df %>% arrange(Fecha,NumVeh,Hora)

veh=seq(1:7) #%>% as.character()

pal <- colorFactor(c("navy", "red","orange","darkgreen","green","blue","skyblue"),
                   domain = veh)




#  locations, color = ~pal(NumVeh))
# color = ~pal(NumVeh))

colores=c("navy", "red","orange","darkgreen","green","blue","skyblue")




#############################################################

g = function(d){
  
  
  dataday= df %>% filter(DiaMes==d)
  setampm= unique(dataday$AMPM)
  m = dataday %>% leaflet() %>%   addTiles() %>% 
    addCircleMarkers(lat=~Lat,lng=~Lng, 
                     color=~pal(NumVeh),
                     fillOpacity =1,radius=5,stroke = FALSE)
  for (v in 1:length(veh)) {
    for (ampm in setampm){
      data=dataday %>% filter( NumVeh == v & AMPM == ampm)  
      tipolinea=ifelse(data$AMPM=="PM","5,10","1")
      c=colores[v]
      print(c)
      print(v)
      m = m %>% addPolylines(data=data,
                             lat=~Lat,lng=~Lng, #weight = 3, fillOpacity = 0.5,
                             dashArray = tipolinea, color=c)
      m=m %>% setView(lat=-33.48, lng=-70.65, zoom = 11) 
    } }
  #View(df)
  output = m
}

library(shiny)
library(quantmod)

shinyServer(function(input, output) {
  output$map <- renderLeaflet({ g(input$date)})
} )


