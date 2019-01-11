#############################
#### ANÁLISIS DE PRECIOS ####
#############################

library(caret)
library(lubridate)
library(tidyverse)
library(magrittr)
library(lfe)
library(ggthemes)

# Carga la base
prods <- readr::read_rds(path = "precios_mensuales.rds") %>%
      filter(is.na(menor.500) == FALSE)

# Crea variable T_i la cual tomoa valor:
#     0 si la fecha de reporte es anterior a la fusión
#     1 si la fecha de reporte es posterior a la fusión
prods <- mutate(
      prods,
      Product = as.factor(Product),
      Day = 1L,
      Date = lubridate::make_date(year = Year, month = Month, day = Day),
      P_t = if_else(Date < ymd("2013-02-01"), 0, 1),
      T_i = if_else(chain == "Ta - Ta" | chain == "Multi Ahorro", 1, 0)) %>%
      select(-Day)

dummies <- predict(dummyVars(~ as.character(Product), data = prods), newdata = prods)
head(dummies)

summary(lm(moda ~ Product + Date, data = prods))
felm(moda ~ P_t + | Product + Date)


ungroup(prods) %>%
      filter(Product %in% sample(unique(prods$Product), 4)) %>%
      dplyr::select(Product, Super, Date, moda, menor.500) %>%
      group_by(Product, Date, menor.500) %>%
      summarise(promedio = mean(moda)) %>%
      ggplot() +
      geom_line(aes(Date, promedio, color = as.factor(menor.500))) +
      geom_vline(aes(xintercept = make_date(year = 2013L, month = 2L, day = 1L)),
                     linetype = 2) +
      facet_wrap(~ as.factor(Product), scales = "free_y") +
      labs(x = NULL, y = "Mean mode", color = NULL) +
      scale_color_discrete(labels = c("Control","Treatment")) +
      ggthemes::theme_economist() +
      theme(legend.position = "bottom",
            axis.title = element_text(face = "bold"))

a <- felm(moda ~ P_t*T_i + menor.500, data = prods)
summary(a)



##### felm example

oldopts <- options(lfe.threads = 1)
## create covariates
x <- rnorm(1000)
x2 <- rnorm(length(x))

## individual and firm
id <- factor(sample(20, length(x), replace = TRUE))
firm <- factor(sample(13, length(x), replace = TRUE))

## effects for them
id.eff <- rnorm(nlevels(id))
firm.eff <- rnorm(nlevels(firm))

## left hand side
u <- rnorm(length(x))
y <- x + 0.5*x2 + id.eff[id] + firm.eff[firm] + u

## estimate and print result
summary(felm(y ~ x + x2 | id + firm))
summary(lm(y ~ x + x2 + id + firm - 1))


library(foreign)
mydata = read.dta("http://dss.princeton.edu/training/Panel101.dta")
ggplot(mydata) +
      geom_line(aes(make_date(year, 1L, 1L), y, color = country))

################################
#### FIN DE LA PROGRAMACIÓN ####
################################