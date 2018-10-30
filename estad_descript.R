###################################
#### ESTADíSTICAS DESCRIPTIVAS ####
###################################

library(tidyverse)
library(magrittr)

base <- read_csv("Bases TATA/P_All_150_clean.csv")

base %>%
      filter(Product %in% c(1,2)) %>%
      ggplot() +
      geom_line(aes(Date, Price, group=Product))

# Separar muestra en dos:
#     más de un km 
#     menos de un km

      
#################################
#### FIN DE LA PROGRAMAACIÓN ####
#################################