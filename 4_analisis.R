#############################
#### ANÁLISIS DE PRECIOS ####
#############################

library(caret)
library(lubridate)
library(tidyverse)
library(magrittr)
library(lfe)
library(ggthemes)

# Carga la base de productos
prods <- readr::read_rds(path = "precios_mensuales.rds") %>%
      filter(is.na(menor.500) == FALSE)

# Convierte Product en un factor 
# Genera la variable Date
# Crea variable P_t la cual toma valor:
#     0 si la fecha de reporte es anterior a la fusión
#     1 si la fecha de reporte es posterior a la fusión
prods <- mutate(
      prods,
      Product = as.factor(Product),
      Day = 1L,
      Date = lubridate::make_date(year = Year, month = Month, day = Day),
      P_t = if_else(Date < ymd("2013-02-01"), 0, 1)) %>%
      dplyr::select(-Day)

# No alcanza la memoria
# dummies <- predict(dummyVars(~ as.character(Product), data = prods), newdata = prods)
# head(dummies)
# summary(lm(moda ~ Product + Date, data = prods))
did <- felm(moda ~ P_t*menor.500 + ingresomedio | Product, data = prods)
summary(did)

prods <- prods %>%
      count(Year, Month) %>%
      mutate(trend = seq(1, 123, 1)) %>%
      dplyr::select(-n) %>%
      right_join(prods, trend, by = c("Year", "Month")) %>%
      select(Year, Month, trend, everything())

did2 <- felm(moda ~ trend*menor.500 + ingresomedio | Product, data = prods)
summary(did2)

ungroup(prods) %>%
      filter(Product %in% sample(unique(prods$Product), 4)) %>%
      dplyr::select(Product, Super, Date, moda, menor.500) %>%
      group_by(Product, Date, menor.500) %>%
      summarise(promedio = mean(moda)) %>%
      ggplot() +
      geom_line(aes(Date, promedio, color = menor.500)) +
      geom_vline(aes(xintercept = make_date(year = 2013L, month = 2L, day = 1L)),
                     linetype = 2) +
      facet_wrap(~ as.factor(Product), scales = "free_y") +
      labs(x = NULL, y = "Mean mode", color = NULL) +
      scale_color_discrete(labels = c("Control","Treatment")) +
      ggthemes::theme_economist() +
      theme(legend.position = "bottom",
            axis.title = element_text(face = "bold"))

################################
#### FIN DE LA PROGRAMACIÓN ####
################################