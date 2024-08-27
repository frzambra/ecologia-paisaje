library(terra)
library(sf)
library(tidyverse)
library(landscapemetrics)

land <- rast(list.files('Ejemplos/data',pattern = 'cov.*tif$',full.names = TRUE))
cuencas <- st_read('Ejemplos/data/SubsubcuencasBNA') |> 
  st_transform(crs(land)) |>   
  filter(COD_SSUBC == "05426")

land <- crop(land, cuencas)
land <- mask(land,cuencas ) #|> project(crs("EPSG:32719"))
#values(land) <- as.integer(values(land))
plot(land)

legend <- read_csv('~/Descargas/leyenda_mapbiomas.csv') |> 
  mutate(Color =toupper(Color)) |> 
  set_names(c('clase','value','color')) |> 
  mutate(value = as.integer(value))

legend <- legend |> filter(value %in% unique(values(land))) |> arrange(value)

#coltab(land) <- legend[,2:3]

pal <- legend$color
names(pal) <- legend$value

library(tmap)
tmap_mode('view')

tm_shape(land) + 
  tm_raster(style = 'cat',title = 'Uso de Suelo',palette = pal,labels = legend$clase) +
  tm_layout(legend.outside = TRUE) +
  tm_facets(ncol =2)

# Analisis de métricas de paisaje
check_landscape(land)

# landscape metrics

# 1. metrica de forma de los parches perimetro / sqrt(area), valores = 1 indican forma cuadrada.

mean_shape_c <- lsm_c_shape_mn(land[[1]])
sd_shape_l <- lsm_l_shape_sd(land)

patches <- show_patches(land,class = 21)

# 2. Calcular el promedio ponderado por parche de le metrica de forma 
# calculate required metric for each patch (e.g. lsm_p_shape)

metric_patch <- lsm_p_shape(land)
metric_patch

# calculate area for each patch
area_patch <- lsm_p_area(land)
area_patch

metric_wght_mean <- dplyr::left_join(x = metric_patch, y = area_patch, 
                                     by = c("layer", "level", "class", "id")) |>
  dplyr::mutate(value.w = value.x * value.y) |>
  dplyr::group_by(class,layer) |>
  dplyr::summarise(value.am = sum(value.w) / sum(value.y)) |> 
  pivot_wider(names_from = layer,values_from = value.am)

metric_wght_mean

# Analizar fragmentacion por clase de uso de suelo

lsm_l_pd(land)

number_patches <- lsm_c_np(land) |> 
  pivot_wider(names_from = layer,values_from = value)

area_patches <- lsm_c_area_mn(land) |> 
  pivot_wider(names_from = layer,values_from = value)

lpi_patches <- lsm_c_lpi(land) |> 
  pivot_wider(names_from = layer,values_from = value)

# densidad de parches
pd_patches <- lsm_c_pd(land) |> 
  mutate(value = value * 1e-9) |> 
  pivot_wider(names_from = layer,values_from = value)

# spliting index
split_patches <- lsm_c_split(land) |> 
  pivot_wider(names_from = layer,values_from = value)

