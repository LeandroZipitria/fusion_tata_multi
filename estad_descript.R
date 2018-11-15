###################################
#### ESTADíSTICAS DESCRIPTIVAS ####
###################################

library(tidyverse)
library(magrittr)

#########################
#### PRODS POR SUPER ####
#########################

prods <- readr::write_rds(prods, path="precios_mensuales.rds")

# Number of products per store
prods_per_store <- prods %>%
      group_by(Super) %>%
      summarise(obs = n(),
                num_prods = length(unique(Product))) %>%
      arrange(desc(num_prods))

################
#### STORES ####
################

stores <- read_rds("Establecimientos.rds")

#### SOLAPAMIENTO DE TIENDAS ####

# Los supermercados para los que no hay productos en la base de prods son descartados
stores <- filter(stores, Super %in% unique(prods_per_store$Super)) %>% droplevels()
length(unique(stores$Super)) # 550 tiendas
length(unique(stores$chain)) # 24 cadenas

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