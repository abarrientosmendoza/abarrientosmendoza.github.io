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



map= df %>% filter(DiaMes == "1") %>% leaflet() %>%   addTiles() %>% 
  addCircleMarkers(lat=~Lat,lng=~Lng, 
                   color=~pal(NumVeh),
                   fillOpacity =1,radius=5,stroke = FALSE) %>%
  addPolylines(lat=~Lat,lng=~Lng, weight = 3, fillOpacity = 0.5, 
               layerId = ~NumVeh)
               
             #  locations, color = ~pal(NumVeh))
              # color = ~pal(NumVeh))

colores=c("navy", "red","orange","darkgreen","green","blue","skyblue")


map= df %>% filter(DiaMes == "1") %>% leaflet() %>%   addTiles() %>% 
  addCircleMarkers(lat=~Lat,lng=~Lng, 
                   color=~pal(NumVeh),
                   fillOpacity =1,radius=5,stroke = FALSE)

for (v in 1:length(veh)) {
  data=df %>% filter(DiaMes == "1" & NumVeh == veh[v]) 
  map=map %>% addPolylines(data=data,lat=~Lat,lng=~Lng, #weight = 3, fillOpacity = 0.5, 
                          color=colores[v])
}
map

mapa <- ggplot() + 
  # Agregamos un mapa de fondo usando ggmap
  # Se debe obtener una clave de API de Google Maps
  # y pegarla en el espacio en blanco:
  # https://developers.google.com/maps/documentation/maps-static/get-api-key
  #ggmap::get_map(location = "santiago", zoom = 12, maptype = "roadmap", source = "google") +
  # Agregamos las rutas de cada camión
  geom_path(data = df, aes(x = Lng, y = Lat, color = NumVeh), size = 1) +
  # Agregamos leyenda y etiquetas
  scale_color_discrete(name = "Vehículo") +
  ggtitle("Rutas de Camiones")


mapa




df %>% filter(DiaMes %in%  veh) %>% leaflet() %>%   addTiles() %>% 
  addCircleMarkers(lat=~Lat,lng=~Lng, 
                   color=~pal(DiaMes),
                   fillOpacity =1,radius=5,popup=~DiaMes,stroke = FALSE) %>%
  setView(lat=-33.45, lng=-70.65, zoom = 10) 
                   

vtai=read_excel("venta_2022.xlsx",sheet="Hoja2")
vtai$Lat=as.numeric(vtai$Lat)
vtai$Long=as.numeric(vtai$Long)
colnames(vtai)[16]="FecDespacho"


vta=vtai %>% group_by (Nombre,Telefono, Lat,Long) %>% 
  summarise(Pedidos=n_distinct(FecDespacho),MtoTotal = sum(Monto),FecUltVta=max(FecDespacho))

vta=vtai %>% group_by (Telefono, Lat,Long) %>% 
  summarise(Pedidos=n_distinct(FecDespacho),MtoTotal = sum(Monto,na.rm = TRUE),
            FecUltVta=max(FecDespacho))

vta$MarkUltVta= ifelse(vta$FecUltVta >= "2022-10-01",1,2)

vta %>%  leaflet() %>%   addTiles() %>% 
  addCircleMarkers(lat=~Lat,lng=~Long, #popup=~Nombre,
                   #color=~pal(NumVeh),
                   fillOpacity =1,radius=~MtoTotal/1e6,stroke = FALSE) %>%
  setView(lat=-33.45, lng=-70.65, zoom = 10) %>%
addMarkers(clusterOptions = markerClusterOptions())   

vta %>%  leaflet() %>%   addTiles() %>% 
  addMarkers(lat=~Lat,lng=~Long, #popup=~Nombre,
                   icon=marker_icon,
                   #color=~pal(NumVeh),
                   ) %>%
  setView(lat=-33.45, lng=-70.65, zoom = 10) %>%
  addMarkers(clusterOptions = markerClusterOptions())  

  leaflet() %>%   addTiles() %>% 
  addCircleMarkers(data=vta,lat=~Lat,lng=~Long, #popup=~Nombre,
                   color=~pal(MarkUltVta),
                   fillOpacity =1,radius=3,stroke = FALSE) %>%
  setView(lat=-33.48, lng=-70.65, zoom = 11) 
  
  table(vta$MarkUl)


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
  View(df)
  output = m
}

   
ui <- fluidPage(
  fluidRow(
    leafletOutput("map",height="80vh"),
  #fluidRow( heigth="10px",
  sliderInput("date", "Fecha", min = 1, max = 31, value = 1)))

 
server <- function(input, output) {
  output$map <- renderLeaflet({ g(input$date)})
  } 


shinyApp(ui, server)

#html = shinyAppToStaticHTML(shinyApp(ui, server))

#write(html, file = "app.html")
