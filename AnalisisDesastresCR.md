Visualización de la base de datos de desastres en Costa Rica
================
Guillermo Durán Sanabria
11/16/2018

El objetivo de este tutorial es hacer un análisis temporal y geográfico
de los desastres relacionados a eventos hidrometeorológicos y
deslizamientos en Costa Rica. El análisis se llevará a cabo utilizando
datos publicos generados por distintas instituciones gubernamentales de
Costa Rica.

### Procedencia de los datos

Los datos se extrayeron del pdf de la 2nda edición del documento
*Histórico de desastres en Costa Rica. Febrero 1723 - Abril 2017*
elaborado por la **Comisión nacional de prevención de riesgos y atención
de emergencias (CNE) de Costa Rica** utilizando la herramienta gratuita
Tabula <https://tabula.technology>

El documento pdf puede ser descargado en el siguiente enlace:
<https://www.cne.go.cr/index.php/documentacienuprincipal-96/historico-de-desastres-en-costa-rica>

Para el análisis se extrayeron del documento las tablas sobre los
eventos hidrometeorológicos y deslizamientos. Se seleccionaron
únicamente estos dos tipos de desastres por ser los que guardan una
relación directa a variabilidad y cambio climático.

Este tutorial se llevará a cabo utilizando R junto con los paquetes del
**tidyverse** para la manipulación de los datos y análisis de texto,
**lubridate** para las series de tiempo, y los paquetes **sf** y
**tmap** para el análisis espacial y generación de mapas.

``` r
library(tidyverse)
library(lubridate)
library(sf)
library(tmap)
```

1.  Preparación de la tabla general, utilizando las tablas generadas en
    Tabula (notese que la columna de FECHA se está importando como tipo
    *date* y se están eliminando los encabezados de cada tabla
    individual)

<!-- end list -->

``` r
tablahidro <- read_csv("datos/tabula-historico_desastres_hidrometeo.csv", 
    col_types = cols(FECHA = col_date(format = "%Y/%m/%d"))) %>% 
  na.omit() %>% 
  mutate(TIPO = "Hidrometeorológicos")

tablades <- read_csv("datos/tabula-historico_desastres_deslizamientos.csv", 
    col_types = cols(FECHA = col_date(format = "%Y/%m/%d"))) %>% 
  na.omit() %>% 
  mutate(TIPO = "Deslizamientos")

names(tablahidro) <- names(tablades) #hacemos que los encabezados de ambas tablas sean iguales 
tablaGeneral <- rbind(tablades, tablahidro) #juntamos ambas tablas en una tabla general
```

Cronología con el número de eventos por cada tipo (Hidrometeorológicos y
Deslizamientos) por año.

``` r
ggplot(tablaGeneral %>% 
         mutate(aNo = year(FECHA)) %>% 
         group_by(aNo, TIPO) %>% 
         summarise(Cantidad = n()), 
       aes(x = aNo, y = Cantidad)) +
  geom_col() +
  facet_grid(rows = vars(TIPO)) +
  labs(x = "Año")
```

![](AnalisisDesastresCR_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

2.  Asignación geográfica a los eventos.

Viendo la tabla, nos damos cuenta que en muchos casos la última palabra
de la columna de *TÍTULO DEL EVENTO* menciona la provincia donde ocurrió
el desastre. Tomando esto como guía podemos extraer la última palabra de
esta columna y asignarla a una nueva columna llamada
*PROVINCIA*.

``` r
provincias <- c("Alajuela", "José", "Guanacaste", "Limón", "Heredia", "Puntarenas", "Cartago")

tablaGeneral_mod <- tablaGeneral %>% 
  mutate(`TÍTULO DEL EVENTO` = str_replace_all(`TÍTULO DEL EVENTO`, "\\r", " "),
         PROVINCIA = str_replace(word(`TÍTULO DEL EVENTO`, -1), "\\.", ""),
         PROVINCIA = case_when(PROVINCIA %in% provincias ~ PROVINCIA),
         PROVINCIA = str_replace(PROVINCIA, "José", "San José"))
```

Con el chunk anterior también corroboramos que el valor en la nueva
columna corresponda con el nombre de alguna provincia, generando de esta
manera entradas vacías (donde la última palabra de la descripción no
encajaba con el nombre de una provincia).

El número de entradas sin nombre de provincias es de:

``` r
sum(is.na(tablaGeneral_mod$PROVINCIA))
```

    ## [1] 54

Lo que significa que a la mitad de nuestra tabla de datos no pudimos
asignarle una provincia de forma automática.

Ahora, preliminarmente, podemos hacer una tabla resumen del número de
eventos por provincia.

``` r
table(tablaGeneral_mod$PROVINCIA, tablaGeneral_mod$TIPO)
```

    ##             
    ##              Deslizamientos Hidrometeorológicos
    ##   Alajuela                1                   5
    ##   Cartago                 4                  13
    ##   Guanacaste              0                   5
    ##   Heredia                 0                   2
    ##   Limón                   1                   7
    ##   Puntarenas              3                   9
    ##   San José                3                   3

Ahora deberemos de asignar manualmente la provincia a las entradas en
que no se pudo asignar automáticamente. Vale mencionar que hay varios
casos en que según la descripción (huracanes, por ejemplo) la afectación
fue nacional.

La asignación de las provincias podría hacerse fácilmente en un programa
de hoja de cálculo (Excel, por ejemplo), pero también lo podemos hacer
desde R.

``` r
#filas asignadas a cada provincia
Cartago <- c(8, 49, 76, 79, 85, 86)
SJ <- c(14, 15, 50, 77, 78, 85, 88)
Heredia <- c(91)
Guanacaste <- c(29, 32, 72, 80, 82, 83, 88, 90)
Puntarenas <- c(33, 40, 46, 56, 59, 64, 72, 73, 75, 80, 81, 82, 83, 88, 90)
Alajuela <- c(51, 66, 78, 82, 84, 85, 91)
Limon <- c(55, 60, 62, 63, 66, 67, 76, 79, 84, 85) 
nacional <- c(89)
```

Descargar datos geográficos de límites provinciales y toponimia.

``` r
library(gdalUtils)
```

    ## 
    ## Attaching package: 'gdalUtils'

    ## The following object is masked from 'package:sf':
    ## 
    ##     gdal_rasterize

``` r
library(rgdal)
```

    ## Loading required package: sp

    ## rgdal: version: 1.3-6, (SVN revision 773)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.1.3, released 2017/20/01
    ##  Path to GDAL shared files: /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rgdal/gdal
    ##  GDAL binary built with GEOS: FALSE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rgdal/proj
    ##  Linking to sp version: 1.3-1

``` r
library(rmapshaper)

dsn_prov <- "WFS:http://geos.snitcr.go.cr/be/IGN_5/wfs?"
ogrListLayers(dsn_prov) #lista de capas en ese WFS
```

    ## [1] "IGN_5:indice_5000"         "IGN_5:curvas_5000"        
    ## [3] "IGN_5:hidrografia_5000"    "IGN_5:limitecantonal_5k"  
    ## [5] "IGN_5:limitedistrital_5k"  "IGN_5:limiteprovincial_5k"
    ## [7] "IGN_5:linea_costa_5000"    "IGN_5:urbano_5000"        
    ## [9] "IGN_5:vias_5000"          
    ## attr(,"driver")
    ## [1] "WFS"
    ## attr(,"nlayers")
    ## [1] 9

``` r
provincias_geo <- st_read(dsn_prov, "IGN_5:limiteprovincial_5k")
```

    ## Reading layer `IGN_5:limiteprovincial_5k' from data source `WFS:http://geos.snitcr.go.cr/be/IGN_5/wfs?' using driver `WFS'
    ## Simple feature collection with 7 features and 25 fields
    ## geometry type:  MULTIPOLYGON
    ## dimension:      XY
    ## bbox:           xmin: 156152 ymin: 608833.8 xmax: 658879.5 ymax: 1241118
    ## epsg (SRID):    5367
    ## proj4string:    +proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs

``` r
provincias_geo <- ms_simplify(ms_simplify(provincias_geo)) #simplificación de borde de los polígonos
  
dsn_pob <- "WFS:http://geos.snitcr.go.cr/be/IGN_NG/wms?"
ogrListLayers(dsn_pob) 
```

    ## [1] "IGN_NG:accidentescosteros_25k"        
    ## [2] "IGN_NG:ng_190508_25k"                 
    ## [3] "IGN_NG:ng_190505_25k"                 
    ## [4] "IGN_NG:ng_190211_25k"                 
    ## [5] "IGN_NG:edificacionesobraspublicas_25k"
    ## [6] "IGN_NG:hidronimos_25k"                
    ## [7] "IGN_NG:nombresgeograficos_25k"        
    ## [8] "IGN_NG:oronimos_25k"                  
    ## [9] "IGN_NG:toponimos_25k"                 
    ## attr(,"driver")
    ## [1] "WFS"
    ## attr(,"nlayers")
    ## [1] 9

``` r
poblados_geo <- st_read(dsn_pob, "IGN_NG:toponimos_25k")
```

    ## Reading layer `IGN_NG:toponimos_25k' from data source `WFS:http://geos.snitcr.go.cr/be/IGN_NG/wms?' using driver `WFS'
    ## Simple feature collection with 9785 features and 5 fields
    ## geometry type:  POINT
    ## dimension:      XY
    ## bbox:           xmin: 161715.1 ymin: 612663.7 xmax: 657704 ymax: 1240833
    ## epsg (SRID):    5367
    ## proj4string:    +proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs
