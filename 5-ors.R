#очистка рабочего пространства
rm(list=ls())

# установка пакета
#так как он еще не на CRAN, то загрузка девелоперской версии
remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)

if(!require(googlePolylines)) {install.packages("googlePolylines")
  require(googlePolylines)}

library(mapview)
library(leaflet)
library(RColorBrewer)


#source https://giscience.github.io/openrouteservice-r/articles/openrouteservice.html

#регистрация API ключа
#чтобы получить ключ нужно зарегистрироваться https://openrouteservice.org/
#потом в личном кабинете dashboard получить ключ (request a token)
#сервис бесплатный, ключи тоже
#но есть ограничение по количеству запросов
key<-ors_api_key("5b3ce3597851110001cf6248644dd2f3190a442ea1abab0455004b90")

#directions для того, чтобы рассчитывать путь между точками
coordinates <- data.frame(lng = c(30.32059, 30.338713), 
                    lat = c(59.88517, 59.926691)) 
x <- ors_directions(coordinates,
                    profile = "driving-car")
#profile - это метод передвижения
#доступные варианты  
#"driving-car","driving-hgv","cycling-regular","cycling-road",
#"cycling-mountain", "cycling-electric","foot-walking","foot-hiking","wheelchair" 

leaflet() %>%
  addTiles() %>%
  setView(lng = 30.3141308, lat = 59.9386292, zoom = 10)%>%
  addGeoJSON(x,fill = NULL)


#построение изохрон
#изохроны - линия, соединяющая точки одновременных событий
#например, можно показать, куда можно добраться за 5 минут
coordinates <- data.frame(lng = c(30.32059, 30.338713), 
                          lat = c(59.88517, 59.926691)) 
# 30 минут с 5-минутными интервалами
isochrone <- ors_isochrones(coordinates, range = 1800, #максимальное значение, с
                      interval = 300,profile = "foot-walking",
                      output = "sf") 
#поменяем порядок полигонов на обратный
#так самые большие полигоны будут снизу
#дальше по мере уменьшения наложены остальные полигоны
isochrone <- isochrone[nrow(isochrone):1, "value"]
pal<-brewer.pal(6,"PiYG")
mapview(isochrone, zcol = "value", col = pal, 
        col.regions = pal,
        alpha.regions = 0.5)


#геокодирование 
#поиск координат по названию - прямое
#поиск названия по координатам - обратное
# расположение всех Санкт-Петербургов в мире
spb <- ors_geocode("Saint Petersburg")
leaflet() %>%
  addTiles() %>%
  addGeoJSON(spb) %>%
  fitBBox(spb$bbox)

#возврат определенного количества результатов
spb <- ors_geocode("Saint Petersburg", size = 1)
leaflet() %>%
  addTiles() %>%
  addGeoJSON(spb) %>%
  fitBBox(spb$bbox)

#поиск в определенной стране
spb <- ors_geocode("Saint Petersburg", boundary.country = "RU")
leaflet() %>%
  addTiles() %>%
  addGeoJSON(spb) %>%
  fitBBox(spb$bbox)

#обратное геокодирование
location <-data.frame(lng = c(30.32059), 
                      lat = c(59.88517)) 
y <- ors_geocode(location = location,  size = 1)
leaflet() %>%
  addTiles() %>%
  addGeoJSON(y) %>%
  fitBBox(y$bbox)



#оптимизация маршрутов 
#в данном случае Vehicle routing problem
#поиск оптимальных путей для набора транспортных средств, чтобы осуществить доставки покупателям
#What is the optimal set of routes for a fleet of vehicles to traverse in order to deliver to a given set of customers?
#задаем начальную точку пути
home_base <- data.frame(lng=30.32059,lat=59.88517)
#описание транспортных средств
vehicles = vehicles(
  id = 1:3, #количество 3 машин
  profile = "driving-car", #метод передвижения
  start = home_base, #начало пути
  end = home_base, #конец пути
  capacity = 4, #вместимость
  skills = list(c(1, 14), c(2, 14), c(3, 10)), #навыки, необходимые для выполнения работы
  time_window = c(28800, 43200) #рабочее время
)
#куда необходимо осуществить поездки
locations <- list(
  c(30.338713, 59.926691),
  c(30.310306, 59.956425),
  c(30.269124, 59.929584),
  c(30.361838, 59.930052),
  c(30.289337, 59.929480),
  c(30.333633, 60.066692),
  c(30.443251, 60.049870)
)

#описание работ, которые необходимо выполнить
jobs = jobs(
  id = 1:7, #количество
  service = 300, #продолжительность работы
  amount = 1, 
  location = locations,
  skills = list(1, 1, 2, 2, 14, 14,3, 10)
)

#оптимизация по заданным параметрам
res <- ors_optimization(jobs, vehicles, options = list(g = TRUE))

#сначала нужно полученные маршруты декодировать
lapply(res$routes, with, {
  list(
    geometry = googlePolylines::decode(geometry)[[1L]],
    locations = lapply(steps, with, if (type=="job") location) %>%
      do.call(rbind, .) %>% data.frame %>% setNames(c("lon", "lat"))
  )
}) -> routes

#функция для сбора набора маршрутов и их маршрутных точек
addRoutes <- function(map, routes, colors) {
  routes <- mapply(c, routes, color = colors, SIMPLIFY = FALSE)
  f <- function (map, route) {
    with(route, {
      labels <- sprintf("<b>%s</b>", 1:nrow(locations))
      markers <- awesomeIcons(markerColor = color, text = labels, fontFamily = "arial")
      map %>%
        addPolylines(data = geometry, lng = ~lon, lat = ~lat, col = ~color) %>%
        addAwesomeMarkers(data = locations, lng = ~lon, lat = ~lat, icon = markers)
    })
  }
  Reduce(f, routes, map)
}

leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data = home_base, icon = awesomeIcons("home")) %>%
  addRoutes(routes, c("purple", "green", "blue"))


#BONUS
#построение профиля пути 
library(RColorBrewer)
library(ggplot2)
library(sf)
if(!require(ggforce)){
  install.packages("ggforce")
  require(ggforce)}
if(!require(units)){
  install.packages("units")
  require(units)}

#рассчитаем путь с учетом высоты точек
#здесь путь примерно от Апатит до Кировска
coordinates <- data.frame(lng = c(33.424732, 33.647056), 
                          lat = c(67.571915, 67.654955))
x <- ors_directions(coordinates, profile = "foot-walking", 
                    elevation = TRUE, #добавление высоты точек к координатам
                    extra_info = "steepness", 
                    output = "sf")

height <- st_geometry(x)[[1]][, 3]
points <- st_cast(st_geometry(x), "POINT")

n <- length(points)
segments <- cumsum(st_distance(points[-n], points[-1], by_element = TRUE))
steepness <- x$extras$steepness$values
steepness <- rep(steepness[,3], steepness[,2]-steepness[,1])
steepness <- factor(steepness, -5:5)

palette = setNames(rev(RColorBrewer::brewer.pal(11, "RdYlBu")), levels(steepness))
units(height) <- as_units("m")
df <- data.frame(x1 = c(set_units(0, "m"), segments[-(n-1)]),
                 x2 = segments,
                 y1 = height[-n],
                 y2 = height[-1],
                 steepness)
y_ran = range(height) * c(0.9, 1.1)
n = n-1
df2 = data.frame(x = c(df$x1, df$x2, df$x2, df$x1),
                 y = c(rep(y_ran[1], 2*n), df$y2, df$y1),
                 steepness,
                 id = 1:n)

ggplot() + theme_bw() +
  geom_segment(data = df, aes(x1, y1, xend = x2, yend = y2), size = 1) +
  geom_polygon(data = df2, aes(x, y, group = id), fill = "white") +
  geom_polygon(data = df2, aes(x, y , group = id, fill = steepness)) +
  scale_fill_manual(values = alpha(palette, 0.8), drop = FALSE) +
  scale_x_unit(unit = "km", expand = c(0,0)) +
  scale_y_unit(expand = c(0,0), limits = y_ran) +
  labs(x = "Distance", y = "Height")
