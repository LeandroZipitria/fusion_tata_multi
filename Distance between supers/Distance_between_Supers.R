#################################
#### DISTANCE BETWEEN SUPERS ####
#################################

library(tidyverse)

####################################################
#### PART 1 - Euclidean distance between supers ####
####################################################

# Calculates de distance (in kms) between all supermarkets in the data set. The result is an NxN matrix with the distance between all supers.

## Load Database
Establecimientos <- read.csv("Distance between supers/2017.Establecimientos 30-08.csv") %>% as_tibble()

## Calculate euclidian distance
df <- Establecimientos[,c(1,9,10)] %>% as_tibble() # Just supermarket and X, Y UTM

# Need to add those numbers that are not in the supermarket database in order for rows to coincide with supermarkets
x <- c(1:max(Establecimientos$Super))
x <- as.data.frame(x)
colnames(x) <- "Super"
df <- merge.data.frame(df, x, by="Super", all.y=TRUE) %>% as_tibble()

dst <- as.matrix(dist(df[-1])/1000) ; ## Calculate the distance in kilometers
diag(dst) <- NA
dst[lower.tri(dst)] <- NA

###################################################
#### PART 2 - Calculate distance less than ... ####
###################################################

# Calculates which supers are at x or less distance from each other. Different values of x are implemented.
# For each distance value creates a vector. Each vector value represents the amount of supermarkets at less than xKms from a given supermarket
# This is not the relevant question for this study

# calculates how many supers each super has at a distance less than ...
less.than.5k <- apply(dst, 1, function(x) length(which(x <= 0.5))) # 500 meters
less.than.1k <- apply(dst, 1, function(x) length(which(x <= 1)))  # 1 km
less.than.15k <- apply(dst, 1, function(x) length(which(x <= 1.5))) # 1.5 kms
less.than.20k <- apply(dst, 1, function(x) length(which(x <= 2))) # 2 kms
y <- as.data.frame(cbind(x, less.than.5k, less.than.1k, less.than.15k, less.than.20k)) %>% as_tibble()

###################################
#### PART 3 - Distance to Tata ####
###################################

# Calculates which supers have a supermarket of chain Tata at less than 0.5Km
# Will create a data.frame:
#     variable 1: supermarket number
#     variable 2: logical. TRUE=super has a Tata at less than 0.5Km

# 1) Computes an NxN matrix, Each cell takes a TRUE value if the two supers are at a distance less than 500 meters.
# 2) Reshapes the matrix into a data frame that contains chain names.
# 3) Checks for lines where Chain2 is Tata and Chain1 and Chain2 are at less than 500 meters
# 4) Groups by Super1
# 5) Computes the summation of all flags from step 3), if summation is greater than zero (that is, if there's at least one TRUE), it flags it as TRUE, otherwise, as FALSE
# Result is a dataframe which lists all stores by number and name and flags (TRUE) which of them have a tata store less than 500 meters away.
menor.500 <- (dst <= 0.5) %>% 
      reshape2::melt(na.rm=TRUE) %>% 
      as_tibble() %>% 
      dplyr::rename(Super1 = Var1, Super2 = Var2, Less = value) %>%
      left_join(dplyr::select(Establecimientos, Super, chain), by=c("Super1" = "Super")) %>% 
      dplyr::rename(Chain1 = chain) %>%
      left_join(dplyr::select(Establecimientos, Super, chain), by=c("Super2" = "Super")) %>% 
      dplyr::rename(Chain2 = chain) %>%
      dplyr::select(Super1, Chain1, Super2, Chain2, Less) %>%
      dplyr::arrange(Super1) %>%
      mutate(haytata = if_else((Chain2 == "Ta - Ta") & (Less == TRUE), TRUE, FALSE)) %>%
      group_by(Super1, Chain1) %>%
      summarise(tata.menos.500 = if_else(sum(haytata) > 0, TRUE, FALSE))

############################
#### END OF PROGRAMMING ####
############################