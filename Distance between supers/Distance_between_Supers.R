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
Establecimientos <- read.csv("Distance between supers/2017.Establecimientos 30-08.csv")

## Calculate euclidian distance
df <- Establecimientos[,c(1,9,10)] %>% as_tibble() # Just supermarket and X, Y UTM

# Need to add those numbers that are not in the supermarket database in order for rows to coincide with supermarkets
x <- c(1:max(Establecimientos$Super))
x <- as.data.frame(x)
colnames(x) <- "Super"

df <- merge.data.frame(df, x, by="Super", all.y=TRUE) %>% as_tibble()

dst <- as.matrix(dist(df[-1])/1000) ; ## Calculate the distance in kilometers
diag(dst) <- NA
# dst <- as.data.frame(round(dst,2)) %>% as_tibble()

###################################################
#### PART 2 - Calculate distance less than ... ####
###################################################

# Calculates which supers are at x or less distance from each other. Different values of x are implemented.

# Calculate distance for those supermarkets that are less than 1 kilometer
less.than.5k <- apply(dst, 1, function(x) length(which(x <= 0.5)))
less.than.1k <- apply(dst, 1, function(x) length(which(x <= 1))) 
less.than.15k <- apply(dst, 1, function(x) length(which(x <= 1.5))) 
less.than.20k <- apply(dst, 1, function(x) length(which(x <= 2))) 
y <- as.data.frame(cbind(x, less.than.5k, less.than.1k, less.than.15k, less.than.20k))


########################################
#### Calculate minimum distance between two stores ####

## Calculate minimum distance between each supermarket

# extract the number of super
md1 <- apply(dst, 1, function(x) {if (all(is.na(x))) {NA}  else {which.min(x)} })
y <- na.omit(as.data.frame(matrix(unlist(md1), nrow = length(md1))))
# extract the distance to the next supermarket
md2 <- apply(dst, 1, function(x) {if (all(is.na(x))) {NA}  else {min(x, na.rm = TRUE)} })
y$dist <- na.omit(as.data.frame(matrix(unlist(md2), nrow = length(md2))))

###################################################

# Now delete the extra rows added earlier
y[,2] <- x # Add numbers
# Load again super base, just supers needed
Establecimientos <- read.csv("~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/Establecimientos.csv")
df <- as.data.frame(Establecimientos[,c(1,6)]) # Just the number of supermarkets & chain
colnames(df) <- c("Super", "Cadena")
y <- merge.data.frame(y,df, by = "Super", all.y = TRUE)

# Delete auxiliary bases
rm(dst,Establecimientos,df,less.than.5k,x)

# Save database
write.csv(y, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/A_menos_de_mediok.csv", row.names = FALSE)

write.csv(y, file="C:/Users/usuario/Dropbox/2017.Seba/Bases/SupComp.csv", row.names=FALSE)     ## salvo base


#############################
###### End #######

### Stores at less than... #### (para calcular los supers que estan a una distancia menor a x kilometros)

less <- as.vector(apply(dst, 1, function(x) paste(which(x <= 0.5), collapse = ",")))
y <- as.data.frame(cbind(x, less))
supers <- as.data.frame(Establecimientos$Super)
names(supers) <- c("Super")
z <- merge.data.frame(y,supers, by = "Super", all.y = TRUE)

# Save database
write.csv(z, "~/Dropbox/Docs/Investigacion/2016.Distance and quality/Bases/A_menos_de_mediok.csv", row.names = FALSE)



