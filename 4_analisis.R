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

# Crea variable P_t la cual tomoa valor:
#     0 si la fecha de reporte es anterior a la fusión
#     1 si la fecha de reporte es posterior a la fusión
# Crea variable T_i la cual tomoa valor:
#     0 si la cadena no es Tata o Multiahorro
#     1 si la cadena es Tata o Multiahorro
# Convierte Product en un factor y genera la variable Date
prods <- mutate(
      prods,
      Product = as.factor(Product),
      Day = 1L,
      Date = lubridate::make_date(year = Year, month = Month, day = Day),
      P_t = if_else(Date < ymd("2013-02-01"), 0, 1),
      T_i = if_else(chain == "Ta - Ta" | chain == "Multi Ahorro", 1, 0)) %>%
      select(-Day)

# No alcanza la memoria
# dummies <- predict(dummyVars(~ as.character(Product), data = prods), newdata = prods)
# head(dummies)
summary(lm(moda ~ Product + Date, data = prods))
summary(felm(moda ~ Product + P_t*T_i | Product, data = prods))

ungroup(prods) %>%
      filter(Product %in% sample(unique(prods$Product), 4)) %>%
      dplyr::select(Product, Super, Date, moda, menor.500, T_i) %>%
      group_by(Product, Date, T_i) %>%
      summarise(promedio = mean(moda)) %>%
      ggplot() +
      geom_line(aes(Date, promedio, color = as.factor(T_i))) +
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