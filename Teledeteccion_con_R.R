# ---
# title: "Teledetección con R"
# author: "Jerónimo Carranza Carranza - jeronimo.carranza@asterionat.com"
# date: "2019-11-12"
# ---


library(raster)

## Creamos una lista de ficheros de imágenes con extensión tiff
lista_tiff = list.files('data', pattern = '.tiff',full.names = TRUE)

## Creamos un rasterLayer con la primera de las imágenes 
(L8B1 = raster(lista_tiff[1]))

## Creamos un reasterStack con la lista de imágenes
(L8RS = stack(lista_tiff))

## Creamos un rasterBrick a partir del rasterStack anterior
(L8RB = brick(L8RS))

## Salvamos a disco en fichero tif multicapa
writeRaster(L8RB,'data/20190927_L8.tif',format="GTiff", options=c("COMPRESS=DEFLATE"), overwrite=TRUE)
(L8RB <- brick('data/20190927_L8.tif')) # Leemos desde disco
removeTmpFiles(h=0.01) # limpieza de temporales
L8RB

## Ploteo simple
plot(L8RB)

## Composiciones RGB
### Color natural
plotRGB(L8RB,4,3,2,stretch='lin') 

### r=SWIR-2, g=SWIR-1, b=Rojo -> (Falso color)
plotRGB(L8RB,7,6,4,stretch='lin') 

### r=NIR, g=Rojo, b=Verde
plotRGB(L8RB,5,4,3,stretch='lin')  

### r=SWIR-2, g=SWIR-1, b=NIR -> 
plotRGB(L8RB,7,6,5,stretch='lin') 

### r=SWIR-1, g=NIR-1, b=Azul
plotRGB(L8RB,6,5,2,stretch='lin') 

### r=NIR, g=SWIR-1, b=Azul
plotRGB(L8RB,5,6,2,stretch='lin') 

###############################################################################
### Componentes principales
cp = princomp(as.matrix(L8RB))
summary(cp)

### Puntuación de los casos en las 3 primeras componentes, 
### nuevo ráster con las puntuaciones
(L8RBcp = predict(L8RB,cp,index=1:3))

### Composición RGB con las 3 primeras componentes principales
### r=cp3, g=cp1, b=cp2
plotRGB(L8RBcp,3,1,2,stretch='lin') 


###############################################################################
### Clasificación no supervisada con k-means, 10 clases
(km = kmeans(as.data.frame(L8RB),10))

### raster base 
L8RBkm = raster(L8RB,1)
### asignamos valores del resultado km
values(L8RBkm) = km$cluster
names(L8RBkm) = 'Clase'
L8RBkm

### Mapa con resultados de la clasificación
plot(L8RBkm, col=c(terrain.colors(2),cm.colors(2),heat.colors(3),gray.colors(1),topo.colors(2)))



###############################################################################
# gdalcubes
###############################################################################
if (!file.exists("L8_Amazon.zip")) {
  download.file("https://uni-muenster.sciebo.de/s/e5yUZmYGX0bo4u9/download", destfile = "data/L8_Amazon.zip")
  unzip("data/L8_Amazon.zip", exdir = "data/L8_Amazon")
}

files = list.files("data/L8_Amazon", recursive = TRUE, full.names = TRUE, pattern = ".tif")
length(files)

library(gdalcubes)

## Lista tipos de colecciones predefinidas
collection_formats()

## Crea colección
L8.col = create_image_collection(files, "L8_SR", "L8.db")
L8.col

## Crea una vista del cubo para chequeo a baja resolución 
v.overview = cube_view(extent=L8.col, dt="P1Y", dx=1000, dy=1000, srs="EPSG:3857", 
                       aggregation = "median", resampling = "bilinear")
raster_cube(L8.col, v.overview)

## Crea una vista del cubo de una region más pequeña con paso mensual
v.subarea = cube_view(extent=list(left=-6320000, right=-6220000, bottom=-600000, top=-500000, 
                                  t0="2014-01-01", t1="2018-12-31"), dt="P1M", dx=50, dy=50, srs="EPSG:3857", 
                      aggregation = "median", resampling = "bilinear")
raster_cube(L8.col, v.subarea)

library(magrittr)
library(viridis)

## Máximo valor NDVI en la región y periodo
raster_cube(L8.col, v.subarea) %>%
  select_bands(c("B04","B05")) %>%
  apply_pixel(c("(B05-B04)/(B05+B04)"), names="NDVI") %>%
  reduce_time("max(NDVI)") %>%
  plot(zlim=c(-0.2,1), col=viridis, key.pos=1)

###############################################################################
# sen2r
###############################################################################
library(sen2r)

# Preparando sen2r_options.json
sen2r()

