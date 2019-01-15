##############################
#### PRECIOS MODA MENSUAL ####
##############################

library(tidyverse)
library(magrittr)
library(foreign)

# Creates a unique data.frame with all the data by super, prod, year and month
prods <- read.csv("Bases TATA/P_All_25_clean.csv") %>% as_tibble()
prods <- prods %>%
      filter(!is.na(Price)) %>%
      group_by(Product, Year, Month, Super) %>%
      summarise(obs = n(), 
                moda = as.numeric(names(sort(table(Price), decreasing = T)))[1L])

for (i in seq(50, 150, 25)) {
      path <- paste0("Bases TATA/P_All_", i, "_clean.csv")
      prods <- read.csv(file = path) %>%
            as_tibble() %>%
            filter(!is.na(Price)) %>%
            group_by(Product, Year, Month, Super) %>%
            summarise(obs = n(), 
                      moda = as.numeric(names(sort(table(Price), decreasing = T)))[1L]) %>%
            bind_rows(prods, .)
}

prods <- ungroup(prods)
stores <- read_rds("Establecimientos.rds")
prods <- left_join(prods, stores, by = "Super")

# Carga la base de ingresos medios
ingmed <- read.dta("Ingreso medio 6ta reunion.dta") %>% as_tibble()
ingmed <- ingmed %>%
      rename(depto = departamento) %>%
      mutate(depto = str_to_title(depto),
             depto = if_else(depto == "Paysandu", "Paysandú",
                     if_else(depto == "Río Negro", "Río Negro",
                     if_else(depto == "San Jose", "San José",
                     if_else(depto == "Tacuarembo", "Tacuarembó", depto)))))
prods <- left_join(prods, ingmed, by = c("Year", "Month", "depto", "ccz")) %>%
      select(Year, Month, depto, ccz, ingresomedio, Product, everything())

readr::write_rds(prods, path = "precios_mensuales.rds")

#################################
#### FIN DE LA PROGRAMAACIÓN ####
#################################