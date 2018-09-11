#################################
#### DISTANCE BETWEEN SUPERS ####
#################################

library(tidyverse)

## que lindo quedo !

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

# Computes an NxN matriz. Each cell takes a TRUE value if the two supers are at a distance less than 500 meters
menor.500 <- (dst <= 0.5) %>% 
      reshape2::melt(na.rm=TRUE) %>% 
      as_tibble() %>% 
      dplyr::rename(Super1 = Var1, Super2 = Var2, Less = value) %>%
      left_join(dplyr::select(Establecimientos, Super, chain), by=c("Super1" = "Super")) %>% 
      dplyr::rename(Chain1 = chain) %>%
      left_join(dplyr::select(Establecimientos, Super, chain), by=c("Super2" = "Super")) %>% 
      dplyr::rename(Chain2 = chain) %>%
      dplyr::select(Super1, Chain1, Super2, Chain2, Less) %>%
      dplyr::arrange(Super1)
# Up to this point, dataframe contains all supermarkets. Now we filter only the obersvations where Tata is involved.
a <- menor.500 %>%
      group_by(Super1) %>%
      mutate(haytata = if_else((Chain2 == "Ta - Ta")&(Less == TRUE), TRUE, FALSE))

table((filter(a, Super1 == 3))$haytata)

      group_by(Super1)
      summarise(less = sum(haytata))
table(.Last.value$haytata)

menor.500 <- dplyr::filter(menor.500, Chain1 == "Ta - Ta")




################
################
#### Delete everything below this point once part 3 is finished #### 

# Extract the number of super
md1 <- apply(dst, 1, function(x) {if (all(is.na(x))) {NA}  else {which.min(x)} })
y <- na.omit(as.data.frame(matrix(unlist(md1), nrow = length(md1))))

# Extract the distance to the next supermarket
md2 <- apply(dst, 1, function(x) {if (all(is.na(x))) {NA}  else {min(x, na.rm = TRUE)} })
y$dist <- na.omit(as.data.frame(matrix(unlist(md2), nrow = length(md2))))

# Now delete the extra rows added earlier
y[,2] <- x # Add numbers

# Load again super base, just supers needed
df <- as.data.frame(Establecimientos[,c(1,6)]) # Just the number of supermarkets & chain
colnames(df) <- c("Super", "Cadena")
y <- merge.data.frame(y, df, by="Super", all.y=TRUE)

# Delete auxiliary bases
rm(dst,Establecimientos,df,less.than.5k,x)

# Save database
write.csv(y, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/A_menos_de_mediok.csv", row.names = FALSE)

write.csv(y, file="C:/Users/usuario/Dropbox/2017.Seba/Bases/SupComp.csv", row.names=FALSE)     ## salvo base


### Stores at less than... #### (para calcular los supers que estan a una distancia menor a x kilometros)

less <- as.vector(apply(dst, 1, function(x) paste(which(x <= 0.5), collapse = ",")))
y <- as.data.frame(cbind(x, less))
supers <- as.data.frame(Establecimientos$Super)
names(supers) <- c("Super")
z <- merge.data.frame(y,supers, by = "Super", all.y = TRUE)

# Save database
write.csv(z, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/A_menos_de_mediok.csv", row.names = FALSE)

############################
#### END OF PROGRAMMING ####
############################