###################################
#### ESTADíSTICAS DESCRIPTIVAS ####
###################################

library(tidyverse)
library(magrittr)
library(ggthemes)

#########################
#### PRODS POR SUPER ####
#########################

prods <- readr::read_rds(path = "precios_mensuales.rds")

# Number of products per store
prods_per_store <- prods %>%
      group_by(Super) %>%
      summarise(obs = n(),
                num_prods = length(unique(Product))) %>%
      arrange(desc(num_prods))

ggplot(prods_per_store) +
      geom_histogram(aes(num_prods)) +
      labs(x = "Cantidad de productos distintos por tienda", y = NULL) +
      ggthemes::theme_economist()

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
      summarise(obs = n(), cajas.prom = mean(Cashiers)) %>%
      ggplot() +
      geom_bar(aes(x = fct_reorder(chain, cajas.prom, .desc = FALSE), y = cajas.prom), 
               stat = "identity") +
      labs(x = NULL, y = "Average number of cashiers by chain") +
      coord_flip() +
      ggthemes::theme_economist()

#### Localización de los supers ####
stores %>%
      mutate(Location = as.factor(if_else(depto != "Montevideo", "Interior", "Montevideo"))) %>%
      ggplot() +
      geom_bar(aes(x = fct_reorder(chain, as.numeric(Location), .fun = mean), fill = Location), 
               position = "fill") +
      coord_flip() +
      labs(x = NULL, y = "Store spatial distribution by chain", fill = NULL) +
      ggthemes::theme_economist() +
      theme(legend.position = "right")

#################################
#### FIN DE LA PROGRAMAACIÓN ####
#################################