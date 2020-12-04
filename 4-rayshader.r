#очистка рабочего пространства
rm(list=ls())

#пакет для создания трехмерных моделей
#может создавать модели на основе DEM или ggplot объектов
if(!require(rayshader)) {install.packages("rayshader")
  require(rayshader)}
#пакет для работы с растровыми изображениями
if(!require(raster)) {install.packages("raster")
  require(raster)}
#пакеты для создания gif
if(!require(magick)) {install.packages("magick")
  require(magick)}
if(!require(gifski)) {install.packages("gifski")
  require(gifski)}
#пакет для трехмерной визуализации
if(!require(rgl)) {install.packages("rgl")
  require(rgl)}

#original code from
#https://www.rayshader.com/
#https://www.tylermw.com/making-beautiful-maps/
#https://wcmbishop.github.io/rayshader-demo/


#
#DEM (digital elevation model) - цифровая модель рельефа
#трехмерное представление поверхности на основе данных о высоте
#данные о высоте как правило хранятся в растровом виде
#DEM from https://www.pgc.umn.edu/data/arcticdem/

  #загрузка данных о высотах из растрового файла
elev_img<- raster::raster("dem.tiff")
  #подготовка матрицы высот
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img)
)

# расчет слоев
#модель затенения, собственная тень
ambmat <- ambient_shade(elev_matrix, zscale = 50)
#карта отбрасываемых теней
raymat <- ray_shade(elev_matrix, zscale = 50, lambert = TRUE)


# двумерное представление DEM
elev_matrix %>%
  #расчет цвета точек поверхности
  sphere_shade(texture = "imhof4") %>%
  #добавление теней собственных и отбрасываемых
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  plot_map()

#трехмерная модель
#соотношение между x,y координатами и высотой z
zscale <- 100
rgl::clear3d()
elev_matrix %>% 
  # расчет цвета точек поверхности
  sphere_shade(texture = "imhof4") %>% 
  # добавление теней собственных и отбрасываемых
  add_shadow(raymat, max_darken = 0.5) %>%
  add_shadow(ambmat, max_darken = 0.5) %>%
  # theta - угол поворота вокруг оси z
  # phi - угол наклона камеры
  # zoom - увеличение
  # fov - поле зрения
  plot_3d(elev_matrix, zscale = zscale, windowsize = c(1200, 1000),
          theta = 75, phi = 30, zoom = 0.3, fov = 90)
#снимок модели с заданными параметрами
render_snapshot()
#снимок трехмерной модели с заданным фокусом
render_depth(focus = 0.75, focallength = 200)

#сделаем гифку для полученной трехмерной модели
#задаем количество изображений в гифке
n_frames <- 30
#углы поворота
thetavalues <- -90 + 45 * cos(seq(0, 2*pi, length.out = n_frames))

#создаем и сохраняем исходные изображения
img_frames <- paste0("drain", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  elev_matrix %>%
    sphere_shade(texture = "imhof4") %>%
    add_shadow(ambmat, 0.5) %>%
    add_shadow(raymat, 0.5) %>%
    plot_3d(elev_matrix, solid = TRUE, shadow = TRUE, zscale = zscale, 
            theta = thetavalues[i], phi = 45)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# собираем полученные изображения в гифку
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "dem.gif", 
                        delay = 6/n_frames)
