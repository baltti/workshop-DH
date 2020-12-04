#source https://riatelab.github.io/ReproducibleCartography/paper/paper.html

if(!require(cartography)) {install.packages("cartography")
  require(cartography)}
library(sp)
library(sf)
library(RColorBrewer)


#подготовка данных
#контура стран загружаются с ресурса http://www.naturalearthdata.com/ 
countries<-ne_countries(scale = "medium",continent = "europe",
                        type = "countries", returnclass = "sf")  
#обрезка объектов по границам с заданными координатами
countries <- st_crop(countries, c(xmin = -10, xmax = 45, 
                                  ymin = 34, ymax = 72))
#данные индекса счастья стран читаются из excel
#data from http://happyplanetindex.org/
#эти данные присоединяются дополнительными атрибутами к имеющимся пространственным
hpi<- sp::merge(countries,read_excel("HPI.xlsx",col_names = TRUE),
                by.x="admin",by.y="Country")%>%
  select(1,64:77)%>% #убираем ненужные колонки
  st_transform(3857)


   
##карта 1 - bubble map
par(mar = c(0, 0, 2, 0)) #отступы от края
#нанесем на карту страны
plot(hpi$geometry, col = "#62abe3", border = "black")
#отобразим на карте индекс счастья 
#индекс будет отображаться в виде символов
#размер символа зависит от величины показателя
propSymbolsLayer(hpi, inches = 0.2, #размер самого большого символа
                 lwd = 0.5, #ширина границы символа
                 col="#13a82e", #цвет символов
                 var = "Happy_Planet_Index", #переменная, на основе которой наносятся символы
                 legend.pos = "topleft", #где расположена легенда
                 legend.values.rnd = 0, #количество знаков после запятой у чисел в легенде
                 legend.title.txt = "Индекс счастья", #заголовок легенды
                 legend.frame = F)
#слой макета
#задает расположение заголовка, подзаголовка, дополнительные символы
layoutLayer(title = "propSymbolsLayer()", #заголовок
            #автор и источник данных либо дополнительны сведения
            author = "SPbDHWeek2020", sources = "", 
            frame = FALSE, #рамка
            scale = NULL, #шкала масштаба
            theme = "wine.pal", #палитра цветов
            north = TRUE)  #направление на север


##карта 2 - choropleth или картограмма
par(mar = c(0, 0, 2, 0)) #отступы от края
#нанесем на карту страны
plot(hpi$geometry, col = "white", border = "black")
#еще раз нанесем на карту индекс счастья
#это простая картограмма или choropleth
#на этой карте слой картограммы сделаем "раскрашенный карандашами"
hpi_pencil<-getPencilLayer(hpi,size = 300,buffer = -2)
choroLayer(hpi_pencil, var = "Happy_Planet_Index", 
           col = brewer.pal(n = 4,name = "Pastel2"), #цветовая палитра
           method = "equal", #метод классификации
           nclass = 4, #количество групп
           border = "white", lwd = 0.5, #цвет и ширина границ
           legend.pos = "topleft", 
           legend.title.txt = "Индекс счастья", add = T)
#добавим подписи
labelLayer(hpi,txt="admin", #колонка с названиями
           col = "blue",
           overlap = FALSE, #допустимо ли наложение подписей
           cex = 0.5) #размер
layoutLayer(title = "choroLayer()", author = "SPbDHWeek2020", 
            sources = "http://happyplanetindex.org/", 
            frame = FALSE, scale = NULL, 
            theme = "green.pal", north = FALSE)


##карта 3 - комбинация картограммы и пропорциональных символов
par(mar = c(0, 0, 2, 0)) #отступы от края
#нанесем на карту страны
plot(hpi$geometry, col = "#62abe3", border = "black")
#нанесем на карту ВВП
choroLayer(hpi, var = "Happy_Planet_Index", 
           col = carto.pal(pal1 = "sand.pal",n1 = 4), method = "equal", 
           nclass = 4, border = "white", lwd = 0.5, legend.pos = "topleft", 
           legend.title.txt = "Индекс счастья", add = T)
#и индекс счастья
propSymbolsLayer(hpi, inches = 0.2, lwd = 1.25, 
                 var = "GDP", col = NA, border = "#940000", 
                 legend.pos = "bottomleft", 
                 legend.values.rnd = 0, 
                 legend.title.txt = "ВВП", 
                 legend.frame = F)
layoutLayer(title = "propSymbolsLayer() + choroLayer()", 
            author = "SPbDHWeek2020", 
            sources = "http://happyplanetindex.org/", frame = FALSE, 
            scale = NULL, theme = "sand.pal", 
            north = FALSE)


##карта 4 - bivariate map одновременное отображение двух переменных
par(mar = c(0, 0, 2, 0)) #отступы от края
#нанесем на карту страны
plot(hpi$geometry, col = "#bff2f0", border = "black")
#нарисуем карту с одновременным отображением переменных
#переменные будут отображены в виде символов
#цвет символа будет зависеть от одно переменной
#а размер от другой
propSymbolsChoroLayer(hpi, var = "GDP", inches = 0.2, 
                      var2 = "Happy_Planet_Index", 
                      col = carto.pal(pal1 = "orange.pal", n1 = 4), 
                      symbols = "square", 
                      method = "equal", nclass = 4, border = "grey50", 
                      lwd = 0.5, legend.var.pos = "topleft", 
                      legend.var.values.rnd = 0, 
                      legend.var.title.txt = "ВВП", 
                      legend.var.style = "e", legend.var2.pos = "bottomleft", 
                      legend.var2.values.rnd = 0, 
                      legend.var2.title.txt = "Индекс счастья")
# layout
layoutLayer(title = "propSymbolsChoroLayer()", 
            author = "SPbDHWeek2020", 
            sources = "http://happyplanetindex.org/", 
            frame = FALSE, theme = "blue.pal", 
            scale = 500, north = FALSE)

##карта 5 - грид
#грид - сетка, как правило, либо с квадратными ячейками, либо с шестиугольными
#сначала нужно создать грид
grid <- getGridLayer(x = hpi, 
  cellsize = median(as.numeric(st_area(hpi)))/2,  #размер ячейки
  var = "Population",type = "hexagonal")
par(mar = c(0, 0, 2, 0)) #отступы от края
#нанесем на карту страны
plot(hpi$geometry, col = "#bff2f0", border = "black")
# Plot the population density
choroLayer(x = grid, var = "Population", method = "quantile", nclass=5, 
           col = carto.pal(pal1 = "turquoise.pal", n1 = 5), border = "grey80", 
           lwd = 0.5, legend.pos = "topright", add = TRUE,
           legend.title.txt = "Население",
           legend.values.rnd =0) 
layoutLayer(title = "", 
            author = paste0("SPbDHWeek2020"), 
            frame = FALSE, north = FALSE, 
            theme = "turquoise.pal")

##карта 6 - hexbin map and 2d histogram map
#в предыдущей карте полигоны разбивались на ячейки грида
#сделаем грид другого типа
#каждый полигон буде преобразован в 1 ячейку
#пакет для создания грида
if(!require(geogrid)) {install.packages("geogrid")
  require(geogrid)}
#сначала лучше предварительно рассчитать, чтобы понять лучшую конфигурацию
#грид из шестиугольников
#сразу просчитываются возможные конфигурации
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  #функция преобразования полигонов в шестиугольники
  new_cells <- calculate_grid(shape = hpi, grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}
#грид из квадратов
#сразу просчитываются возможные конфигурации
par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))
for (i in 1:6) {
  #преобразование полигонов в квадраты
  new_cells <- calculate_grid(shape = hpi, grid_type = "regular", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}
#расчет грида
europe_hex <- calculate_grid(shape = hpi, grid_type = "hexagonal", seed = 4)
#присоединение атрибутов
resulthex <- assign_polygons(hpi, europe_hex)
#расчет грида
europe_reg <- calculate_grid(shape = hpi, grid_type = "regular", seed = 4)
#присоединение атрибутов
resultreg <- assign_polygons(hpi, europe_reg)

par(mar = c(0, 0, 2, 0))
plot(hpi$geometry, col = "#62abe3", border = "black")
#нанесем на карту ВВП
choroLayer(hpi, var = "Happy_Planet_Index", 
           col = carto.pal(pal1 = "sand.pal",n1 = 4), method = "equal", 
           nclass = 4, border = "white", lwd = 0.5, legend.pos = "topleft", 
           legend.title.txt = "Индекс счастья", add = T)

plot(resulthex$geometry, col = "#62abe3", border = "black")
#нанесем на карту ВВП
choroLayer(resulthex, var = "Happy_Planet_Index", 
           col = carto.pal(pal1 = "sand.pal",n1 = 4), method = "equal", 
           nclass = 4, border = "white", lwd = 0.5, legend.pos = "topleft", 
           legend.title.txt = "Индекс счастья", add = T)
labelLayer(resulthex,txt="admin", #колонка с названиями
           col = "blue",
           overlap = TRUE, #допустимо ли наложение подписей
           cex = 0.7) 

plot(resultreg$geometry, col = "#62abe3", border = "black")
#нанесем на карту ВВП
choroLayer(resultreg, var = "Happy_Planet_Index", 
           col = carto.pal(pal1 = "sand.pal",n1 = 4), method = "equal", 
           nclass = 4, border = "white", lwd = 0.5, legend.pos = "topleft", 
           legend.title.txt = "Индекс счастья", add = T)
labelLayer(resultreg,txt="admin", #колонка с названиями
           col = "blue",
           overlap = TRUE, #допустимо ли наложение подписей
           cex = 0.7) 