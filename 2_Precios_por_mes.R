##############################
#### PRECIOS MODA MENSUAL ####
##############################

library(tidyverse)
library(magrittr)

# Creates a unique data.frame with all the data by super, prod, year and month
prods <- read.csv("Bases TATA/P_All_25_clean.csv") %>% as_tibble()
prods <- prods %>%
      filter(!is.na(Price)) %>%
      group_by(Product, Year, Month, Super) %>%
      summarise(obs = n(), 
                moda = as.numeric(names(sort(table(Price), decreasing=T)))[1L])

for (i in seq(50, 150, 25)) {
      path <- paste0("Bases TATA/P_All_", i, "_clean.csv")
      prods <- read.csv(file=path) %>%
            as_tibble() %>%
            filter(!is.na(Price)) %>%
            group_by(Product, Year, Month, Super) %>%
            summarise(obs = n(), 
                      moda = as.numeric(names(sort(table(Price), decreasing=T)))[1L]) %>%
            bind_rows(prods, .)
}

stores <- read_rds("Establecimientos.rds")
stores <- select(stores, Super, starts_with("menor."))
prods <- left_join(prods, stores, by="Super")

readr::write_rds(prods, path="precios_mensuales.rds")

#################################
#### FIN DE LA PROGRAMAACIÓN ####
#################################