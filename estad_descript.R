###################################
#### ESTADíSTICAS DESCRIPTIVAS ####
###################################

library(tidyverse)
library(magrittr)

base <- read_csv("Bases TATA/P_All_150_clean.csv")

base %>%
      filter(Product %in% c(126,127)) %>%
      ggplot() +
      geom_line(aes(Date, Price, color=Product))

# Separar muestra en dos:
#     más de un km 
#     menos de un km

# tamaño promedio del super (en cajas) 
# super location

# Tratamiento; ser otro super que está a menos de un km de un tata


# feb 2010 - feb 2013 (fusión) - feb 2016
# feb 2009 - feb 2013 (fusión) - feb 2017


#################################
#### FIN DE LA PROGRAMAACIÓN ####
#################################