#############################
#### ANÁLISIS DE PRECIOS ####
#############################

library(tidyverse)
library(magrittr)
library(lubridate)
library(lfe)
library(ggthemes)

# Carga la base
prods <- readr::read_rds(path = "precios_mensuales.rds")

# Crea variable T_i la cual tomoa valor:
#     0 si la fecha de reporte es anterior a la fusión
#     1 si la fecha de reporte es posterior a la fusión
prods <- mutate(prods, 
       Day = 1L,
       Date = lubridate::make_date(year = Year, month = Month, day = Day),
       T_i = if_else(Date < ymd("2013-02-01"), 0, 1)) %>%
      select(-Day) %>%
      filter(is.na(menor.500) == FALSE)

ungroup(prods) %>%
      filter(Product %in% c(1:4)) %>%
      dplyr::select(Product, Date, moda, menor.500, Super) %>%
      group_by(Product, Date, menor.500) %>%
      summarise(promedio = mean(moda)) %>%
      ggplot() +
      geom_line(aes(Date, promedio, color = as.factor(menor.500))) +
      geom_vline(aes(xintercept = make_date(year = 2013L, month = 2L, day = 1L)),
                     linetype = 2) +
      facet_wrap(~ as.factor(Product), scales = "free_y") +
      labs(x = NULL, y = "Mode mean", color = NULL) +
      scale_color_discrete(labels = c("Control","Treatment")) +
      ggthemes::theme_economist() +
      theme(legend.position = "bottom")

summary(lm(moda ~ menor.500*T_i, data = prods))


################################
#### FIN DE LA PROGRAMACIÓN ####
################################