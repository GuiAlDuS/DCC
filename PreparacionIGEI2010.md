Importación y limpieza de tabla de Inventario de GEI 2010
================

Este es un tutorial/reporte de cómo realizar la limpieza de la tabla
resumen del Inventario de Gases de Efecto Invernadero de Costa Rica del
2010, tomando los datos directamente del documento original en pdf.

Paquetes a
    utilizar:

``` r
library(tidyverse)
```

    ## ── Attaching packages ──────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.1.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.7
    ## ✔ tidyr   0.8.2     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ─────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
tabla2010_raw <- read_csv("datos/TablaGeneral_InvGEI2010.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Tipo = col_character(),
    ##   `CO2 neto` = col_character(),
    ##   CH4 = col_character(),
    ##   N2O = col_character(),
    ##   HFC = col_character(),
    ##   PFC = col_character(),
    ##   SF6 = col_character(),
    ##   NOx = col_character(),
    ##   CO = col_character(),
    ##   NMVOC = col_character(),
    ##   SO2 = col_character()
    ## )

Cosas necesarias para limpiar la tabla: - Eliminar puntos como
separadores de miles y cambiar comas a puntos para separadores de
decimales - Cambiar tipos de datos de todas las columnas menos la
primera a números. - Hacer categorización según columna *Tipo*.

``` r
tabla2010 <- tabla2010_raw %>% 
  slice(2:104) %>% 
  mutate_at(vars(-Tipo), funs(str_remove(., "\\."))) %>% 
  mutate_at(vars(-Tipo), funs(str_replace(., ',', '.'))) %>% 
  mutate_at(vars(-Tipo), funs(as.numeric(.))) %>% 
  mutate(Cat = case_when(str_detect(Tipo, "^1") ~ "Energía",
                         str_detect(Tipo, "^2") ~ "Procesos industriales y uso de productos",
                         str_detect(Tipo, "^3") ~ "Agricultura, silvicultura y otros usos de la tierra",
                         str_detect(Tipo, "^4") ~ "Desechos",
                         str_detect(Tipo, "^5") ~ "Otros")) %>% 
  filter(!str_detect(Tipo, "^[1-5] ")) %>% 
  mutate(Cat2_cod = str_sub(Tipo, 1, 3))
```

    ## Warning in evalq(as.numeric(`CO2 neto`), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(CH4), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(N2O), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(HFC), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(PFC), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(SF6), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(NOx), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(CO), <environment>): NAs introduced by coercion

    ## Warning in evalq(as.numeric(NMVOC), <environment>): NAs introduced by
    ## coercion

    ## Warning in evalq(as.numeric(SO2), <environment>): NAs introduced by
    ## coercion

Creación de tabla nueva con nombres de subcategorias:

``` r
 tabla2010subc <- tabla2010 %>% 
  filter(str_detect(Tipo, "^[1-5]A |B |C |D |E |F |G |H "),
         !str_detect(Tipo, "^2G2")) %>% 
  mutate(Cat2_txt = str_sub(Tipo, 4,),
         Cat2_cod = str_sub(Cat2_cod, 1, 2)) %>% 
  select(Cat2_cod, Cat2_txt)
```

Union de tablas para tener columna con nombres de subcategorías:

``` r
tabla2010 <- tabla2010 %>% 
  mutate(Cat2 = Cat2_cod,
         Cat2_cod = str_sub(Cat2_cod, 1, 2)) %>% 
  left_join(tabla2010subc, by = "Cat2_cod")
```

Eliminación de filas con acumulado de subcategorías: Notese que las
subcategorias 4 y 5 no tienen niveles inferiores, por lo que los
acumulados de estas se dejan en la tabla.

``` r
tabla_4_5 <- tabla2010 %>% 
  slice(87:n()) %>% 
  mutate(Cat = replace_na(Cat, "Otros"),
         Cat2_txt = coalesce(Cat2_txt, Tipo))
```

``` r
tabla2010_2 <- tabla2010 %>% 
  mutate(Cat2 = str_remove(Cat2, " ")) %>% 
  filter(str_length(Cat2) == 3) %>% 
  slice(1:71)
```

Union de tablas:

``` r
tabla2010_limpia <- rbind(tabla2010_2, tabla_4_5) 
```
