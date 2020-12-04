#очистка рабочего пространства
rm(list=ls())

#пакет для работы с leaflet 
if(!require(leaflet)) {install.packages("leaflet")
  require(leaflet)}

library(tidyverse)
library(readxl)
library(RColorBrewer)


#original code from
#http://seankross.com/slides/Developing_Data_Products/leaflet/leaflet.html#1
#https://rstudio.github.io/leaflet/

#создание объекта с картой и добавление подложки
my_map <- leaflet() %>% 
  addTiles()
my_map

#добавление точечных объектов на карту
#одиночный маркер
my_map %>%
addMarkers(lat=59.88517, lng=30.32059, 
           popup="Мы здесь") #при щелчке по маркеру будет появляться надпись


#подготовка данных
#чтение из файла данных и чистка таблицы
#данные взяты с https://data.gov.spb.ru/opendata/7842489089-sights/
sights<-read_excel("sights.xlsx",col_names = TRUE) %>%
  data.frame %>%
  drop_na(28:29)%>%
  transform(coord_shirota= as.numeric(coord_shirota),
            coord_dolgota= as.numeric(coord_dolgota))


#добавление объектов на карту
#здесь маркеры стандартные по умолчанию
sights %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(lat=sights$coord_shirota, #координаты берутся из таблицы
             lng = sights$coord_dolgota)%>%
  #вид задается координатами центра и увеличением
  #увеличение 10 это примерно масштаб одного города
  setView(lng = 30.3141308, lat = 59.9386292, zoom = 10) 

#создание цветовой палитры
#цвет будет зависеть от типа объекта
pal <- colorFactor(
  palette = "RdYlBu",
  domain = sights$obj_type)

#модификация карты с маркерами
#здесь маркеры будут круглые
sights %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lat=sights$coord_shirota,
                   lng = sights$coord_dolgota,
                   color = ~pal(obj_type), 
                   #контур - отсутствует
                   #прозрачность задается от 0 до 1
                   stroke = FALSE,fillOpacity = 0.9, 
                   #все объекты будут собираться в группы кластеры
                   clusterOptions = markerClusterOptions(),
                   popup = sights$name)%>% 
  setView(lng = 30.3141308, lat = 59.9386292, zoom = 10)


