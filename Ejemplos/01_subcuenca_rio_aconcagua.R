library(terra)
library(sf)
library(tidyverse)

land <- rast(list.files('~/Descargas',pattern = 'cov.*tif$',full.names = TRUE))
cuencas <- st_read('~/Descargas/SubsubcuencasBNA') |> 
  st_transform(crs(land)) |>   
  filter(COD_SSUBC == "05426")

land <- crop(land, cuencas)
land <- mask(land,cuencas )

legend <- read_csv('~/Descargas/leyenda_mapbiomas.csv') |> 
  mutate(Color =toupper(Color)) |> 
  set_names(c('clase','value','color')) |> 
  mutate(value = as.integer(value))

legend <- legend |> filter(value %in% unique(values(land))) |> arrange(value)

coltab(land) <- legend[,2:3]

pal <- legend$color
names(pal) <- legend$value

library(tmap)
tmap_mode('view')

tm_shape(land) + 
  tm_raster(style = 'cat',title = 'Uso de Suelo',palette = pal,labels = legend$clase) +
  tm_layout(legend.outside = TRUE)

land_re <- classify(land, legend[,2:1])
coltab(land)
subst(land, )
values(land)

plot(land)
library(landscapemetrics)
check_landscape(land)
land |> freq()

show_patches(land,class = 3)

perimet <- lsm_p_perim(land)
