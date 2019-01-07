#############################
#### ANÁLISIS DE PRECIOS ####
#############################

library(tidyverse)
library(magrittr)
library(lubridate)
library(lfe)

# Carga la base
prods <- readr::read_rds(path = "precios_mensuales.rds")

# Crea variable T_i la cual tomoa valor:
#     0 si la fecha de reporte es anterior a la fusión
#     1 si la fecha de reporte es posterior a la fusión
prods <- mutate(prods, 
       Day = 1L,
       Date = lubridate::make_date(year = Year, month = Month, day = Day),
       T_i = if_else(Date < ymd("2013-02-01"), 0, 1)) %>%
      select(-Day)

prods %>%
      ungroup() %>%
      filter(Product %in% c(1:9)) %>%
      dplyr::select(Product, Date, moda, menor.500) %>%
      group_by(Product, Date, menor.500) %>%
      summarise(promedio = mean(moda)) %>%
      ggplot() +
      geom_line(aes(Date, promedio)) +
      facet_wrap(~Product)

################################
#### FIN DE LA PROGRAMACIÓN ####
################################