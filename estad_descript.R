###################################
#### ESTADíSTICAS DESCRIPTIVAS ####
###################################

library(tidyverse)
library(magrittr)

#########################
#### PRODS POR SUPER ####
#########################

# Número de productos
# Creates a unique data.frame with all the data by super, prod, year and month
prods <- readr::read_csv("Bases TATA/P_All_25_clean.csv")
prods <- prods %>%
      filter(!is.na(Price)) %>%
      group_by(Product, Year, Month, Super) %>%
      summarise(obs = n(), 
                moda = as.numeric(names(sort(table(Price), decreasing=TRUE)))[1L])
      
for (i in seq(50, 150, 25)) {
      path <- paste0("Bases TATA/P_All_", i, "_clean.csv")
      prods <- readr::read_csv(file=path) %>%
            filter(!is.na(Price)) %>%
            group_by(Product, Year, Month, Super) %>%
            summarise(obs = n(), 
                      moda = as.numeric(names(sort(table(Price), decreasing=T)))[1L]) %>%
            bind_rows(prods, .)
}

# Number of goods per store
prods_per_store <- prods %>%
      group_by(Super) %>%
      summarise(obs = n(),
                num_prods = length(unique(Product))) %>%
      arrange(desc(num_prods))

################
#### STORES ####
################

#### SOLAPAMIENTO DE TIENDAS ####

# Los supermercados para los que no hay productos en la base de prods son descartados
stores <- read_rds("Establecimientos.rds")
stores <- filter(stores, Super %in% unique(num_prods$Super)) %>% droplevels()

# No existen Multiahorros con un TaTa a 500 mts o menos
stores %>%
      filter(chain == "Multi Ahorro") %>%
      droplevels() %>%
      select(chain, menor.500) %$%
      table(.)

# Hay un Multiahorro con un Tata a 1 km o menos
stores %>%
      filter(chain == "Multi Ahorro") %>%
      droplevels() %>%
      select(chain, menor.1) %$%
      table(.)

# Hay 6 Multiahorro con un Tata a 2 kms o menos
stores %>%
      filter(chain == "Multi Ahorro") %>%
      droplevels() %>%
      select(chain, menor.2) %$%
      table(.)

#### Tamamaño promedio de supers ####
group_by(stores, chain) %>%
      summarise(obs = n(), cajas.prom = mean(Cashiers))

#### Localización de los supers ####
stores %>%
      mutate(interior = if_else(depto != "Montevideo", 1, 0)) %>%
      group_by(chain) %>%
      summarise(interior = sum(interior), montevideo = n() - interior)

#################################
#### FIN DE LA PROGRAMAACIÓN ####
#################################