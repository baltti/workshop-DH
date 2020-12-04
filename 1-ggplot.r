
#загрузка нужных пакетов. 
#Условие нужно для того, чтобы при отсутствии пакета он был установлен и подключен
#основной пакет работы с данными
#нам нужен для работы с таблицей dplyr и графикой через ggplot
if(!require(tidyverse)) {install.packages("tidyverse")
  require(tidyverse)}
#пакет для чтения таблиц excel
if(!require(readxl)) {install.packages("readxl")
  require(readxl)}
#пакет для того, чтобы загрузить данные с http://www.naturalearthdata.com/
#загрузка в формате shape файлов
#нужен ьудет, чтобы загрузить границы стран
if(!require(rnaturalearth)) {install.packages("rnaturalearth")
  require(rnaturalearth)}
#пакет для работы с пространственными данными
if(!require(sp)) {install.packages("sp")
  require(sp)}
#пакет для работы с векторными пространсвенными данными
#позволяет перепроецировать данные
if(!require(sf)) {install.packages("sf")
  require(sf)}
#сборник цветовых палитр
if(!require(RColorBrewer)) {install.packages("RColorBrewer")
  require(RColorBrewer)}
#графический пакет для добавления элементов в графики ggplot
if(!require(cowplot)) {install.packages("cowplot")
  require(cowplot)}
#пакет для работы с пространственными данными в ggplot
if(!require(ggspatial)) {install.packages("ggspatial")
  require(ggspatial)}
#пакет для создания интерактивной визуализации на базе leaflet и mapbox
if(!require(mapview)) {install.packages("mapview")
  require(mapview)}
#пакет для создания трехмерных моделей
#может создавать модели на основе DEM или ggplot объектов
if(!require(rayshader)) {install.packages("rayshader")
  require(rayshader)}



  #подготовка данных
#контура стран загружаются с ресурса http://www.naturalearthdata.com/ 
countries<-ne_countries(scale = "medium",continent = "europe",
                        type = "countries", returnclass = "sf")  
#обрезка объектов по границам с заданными координатами
#так карта станет более читаемой
countries <- st_crop(countries, c(xmin = -10, xmax = 45, 
                                  ymin = 34, ymax = 72))
#данные индекса счастья стран читаются из excel
#data from http://happyplanetindex.org/
#эти данные присоединяются дополнительными атрибутами к имеющимся пространственным
hpi<- sp::merge(countries,read_excel("HPI.xlsx",col_names = TRUE),
        by.x="admin",by.y="Country")%>%
  select(1,64:77)%>% #убираем ненужные колонки
  st_transform(3857) #проекция трансформируется в стандартную проекцию google maps
 
  
  
#базовая отрисовка контуров стран
hpi %>%
  ggplot()+
  geom_sf()
  
#заливка контуров белым цветом и замена темы графика
hpi %>%
  ggplot()+ #создание объекта ggplot
  geom_sf(fill="white")+ #заливка объектов цветом
  theme_minimal()

#градиентная заливка в зависимости от величины показателя
hpi %>%
  ggplot()+ #создание объекта ggplot
  #добавление объектов с градиентной заливкой в зависимости от параметра
  geom_sf(aes(fill=Happy_Planet_Index))+
  theme_minimal()

  
#original code https://timogrossenbacher.ch/2016/12/beautiful-thematic-maps-with-ggplot2-only/
 
 
  ##choropleth map или картограмма
#предварительно нужно подготовить данные
#разобьем все страны на несколько групп
no_classes <- 4 #количество групп может быть любым
labels <- c() #это будет вектор со значениями для легенды
#здесь будет разбивка по квантилям
#то есть в каждую группу попадет одинаковое число стран
quantiles <- quantile(hpi$Happy_Planet_Index, 
                      probs = seq(0, 1, length.out = no_classes + 1))
#округление границ 
labels <- c() 
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 0), 
                             " – ", 
                             round(quantiles[idx + 1], 0)))}
#удаление последнего элемента 
#потому что он будет подобным "66.62 - NA"
labels <- labels[1:length(labels)-1]
#дополнительная переменная с номером квантиля
hpi$hpi_quant <- cut(hpi$Happy_Planet_Index, 
                                     breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)

#отрисуем карту с новой палитрой и разбивкой стран на группы 
hpi %>%
  ggplot() + #создание объекта ggplot
  #добавление объектов с цветовой заливкой в зависимости от параметра
  geom_sf(aes(fill=hpi_quant))+ 
  #палитра
  scale_color_brewer(type = "div",palette="RdYlGn",  
                     aesthetics = "fill",
                     name = "Индекс счастья", #заголовок легенды
                     #параметры легенды
                     guide = guide_legend(keyheight = unit(5, units = "mm"),
                       title.position = 'top', #расположение заголовка
                       reverse = T))+ #обратить последовательность 
  theme_minimal()+
  labs(caption = "source: http://happyplanetindex.org/")
  
#построим ту же самую карту, но с другой разбивкой на группы
#разбивка на равные интервалы
equal.interval <- seq(min(hpi$Happy_Planet_Index), 
                     max(hpi$Happy_Planet_Index), 
                     by = (max(hpi$Happy_Planet_Index)-min(hpi$Happy_Planet_Index))/4)
labels <- c() 
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " – ", 
                             round(quantiles[idx + 1], 2)))}
#удаление последнего элемента 
#потому что он будет подобным "66.62 - NA"
labels <- labels[1:length(labels)-1]
hpi$hpi.equal <- cut(hpi$Happy_Planet_Index, breaks=equal.interval, 
                     labels=labels,include.lowest = TRUE)
hpi %>%
  ggplot() +
  annotation_map_tile(type="osm")+ #можно присоединить подложку ОSМ
  geom_sf(aes(fill=hpi.equal))+
  scale_color_brewer(type = "div",palette="RdYlGn",
                     aesthetics = "fill",
                     name = "Индекс счастья",
                     guide = guide_legend(
                       keyheight = unit(5, units = "mm"),
                       title.position = 'top',
                       reverse = T))+
  theme_bw()+
  #удаление сетки, подписей координат по осям
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),axis.text =element_blank(),
        legend.position = "none")+
  labs(caption = "source: http://happyplanetindex.org/")


  
##bivariate map - способ отображения одновременно двух переменных на тематической карте
  #переменные могут быть закодированы цветами или символами  
#переменные будут ожидаемая продолжительность жизни и ВВП (GDP)
#original code here https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
#создаем 3 класса для ВВП
quantiles_gdp <- hpi %>%
  pull(GDP) %>%
  quantile(probs = seq(0, 1, length.out = 4))
# и 3 класса для ожидаемой продолжительности жизни
quantiles_hpi <- hpi %>%
  pull(Happy_Planet_Index) %>%
  quantile(probs = seq(0, 1, length.out = 4))

# сначала нужно создать цветовую схему для кодировки переменных
#цветовую схему можно подобрать здесь https://observablehq.com/@benjaminadk/bivariate-choropleth-color-generator
# желтый цвет - ожидаемая продолжительность
# синий - ВВП
#каждой комбинации будет соответствовать свой цвет 
#цвета заданы в кодировке HEX - шестнадцатеричное представление RGB
bivariate_color_scale <- tibble(
  "3 - 3" = "#0d8000", # высокий ВВП, высокий индекс счастья
  "2 - 3" = "#81b700",
  "1 - 3" = "#e8ca00", # низкий ВВП, высокий индекс счастья 
  "3 - 2" = "#0d808b",
  "2 - 2" = "#81b78b", # средний ВВП, средний индекс счастья
  "1 - 2" = "#e8dc8b",
  "3 - 1" = "#0d80d9", # высокий ВВП, низкий индекс счастья
  "2 - 1" = "#81b7e1",
  "1 - 1" = "#e8e8e8" # низкий ВВП, низкий индекс счастья
) %>%
  gather("group", "fill")
# делим объекты на группы и присоединяем отдельным атрибутом 
hpi %<>%
  mutate(gdp_quantiles = cut(GDP,
      breaks = quantiles_gdp,
      include.lowest = TRUE),
    hpi_quantiles = cut(Happy_Planet_Index,
      breaks = quantiles_hpi,
      include.lowest = TRUE),
    # вносим номера групп по обоим переменным в один столбец
    # это нужно, чтобы присоединить цветовую кодировку
    group = paste(as.numeric(gdp_quantiles), "-",
      as.numeric(hpi_quantiles))
    ) %>%
  # теперь присоединяем цветовую кодировку
  # теперь у каждой страны есть свой цветовой код на основе значений
  #ВВП и ожидаемой продолжительности жизни
  left_join(bivariate_color_scale, by = "group")

#создание карты
map<-hpi%>%
  ggplot() +
  geom_sf(aes(fill=fill))+ 
  scale_fill_identity() +
  #подписи осей и заголовок
  labs(x = NULL,
       y = NULL,
       title = "Индекс счастья и ВВП в Европе")+
  theme_minimal()+
  labs(caption = "source: http://happyplanetindex.org/")


#для создания легенды нужно выделить отдельно ВВП и продолжительность жизни
#так как при подготовке цветовой схемы каждый цвет был соотнесен с группой
#группы закодированы в виде "1-2" 
bivariate_color_scale %<>%
  separate(group, into = c("GDP", "Happy_Planet_Index"), sep = " - ") %>%
  mutate(GDP = as.integer(GDP),
         Happy_Planet_Index = as.integer(Happy_Planet_Index))

#подготовка легенды
legend <- ggplot() +
  #
  geom_tile(data = bivariate_color_scale,
    mapping = aes(
      x = GDP,
      y = Happy_Planet_Index,
      fill = fill)) +
  scale_fill_identity() +
  labs(x = "Повышение ВВП ⟶️",
       y = "Увеличение счастья ⟶️") +
  theme_minimal() +
  # размер шрифта
  theme(axis.title = element_text(size = 8)) +
  # квадратные ячейки
  coord_fixed()
  
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)


#создание трехмерной модели из объекта ggplot
plot_gg(map, width = 5, height = 4, scale = 300, 
        multicore = TRUE, windowsize = c(1000, 800))



  #cartogram или анаморфоза
  #на такой карте размер объектов не соответствует реальному
  #размеры пересчитываются на основе значения какого-либо параметра
  #чем больше значение параметра, тем больше будет объект на карте
if(!require(cartogram)) {install.packages("cartogram")
    require(cartogram)}

eu_cartogram<-cartogram_cont(hpi,"Happy_Planet_Index",
                             itermax = 7)

eu_cartogram %>%
  ggplot() +
  geom_sf(aes(fill=hpi.equal))+
  scale_color_brewer(type = "div",palette="RdYlGn",
                     aesthetics = "fill",
                     name = "Индекс счастья",
                     guide = guide_legend(
                       keyheight = unit(5, units = "mm"),
                       title.position = 'top',
                       reverse = T))+
  theme_bw()



#интерактивная карта
#source https://r-spatial.github.io/mapview/index.html
#цветовая палитра
pal<-brewer.pal(4,"Spectral")

mapview(hpi,zcol="Happy_Planet_Index", #набор данных и отображаемая переменная
        at=equal.interval, #разбивка на интервалы
        legend=TRUE, #отображение легенды
        col.regions=pal, #цвета заливки полигонов
        alpha.regions=0.4) #прозрачность

