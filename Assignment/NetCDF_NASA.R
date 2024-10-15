#Commands to set the directory:
getwd()
path <- "C:/Users/Davide/Desktop/Alma Mater/SECOND YEAR/GIS/Assignment"
setwd(path)

#libraries
library(sf)
library(spData)
library(tidyverse) 
library(ggplot2)
library(readxl)
library(dplyr)
library(viridis)
library(ggimage)

# ------------------------------netCDF_NASA assignment -------------------------
#download the paper’s data [here], combine with world and transportation data (available at NaturalEarth) to:
# 1 Generate an sf MULTIPOINT feature of markets across Africa and plot it;
# 2 Calculate minimum distance of each market to the (i) nearest road, and (ii) nearest port, (iii) nearest airport;
# 3 Produce 3 scatter plots (each for every infrastructure considered) relating market average prices to the minimum distances.

world <- world
Roads <- read_sf("data/Transportations/Roads/ne_10m_roads.shp")
Ports <- read_sf("data/Transportations/Ports/ne_50m_ports.shp")
Airports <- read_sf("data/Transportations/Airports/ne_50m_airports.shp")
Prices <-read_excel("data/NetCDF/PriceMaster4GAMS.xlsx")
Markets <-read_excel("data/NetCDF/MktCoords.xlsx")
sf_markets <- Markets %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ----------------------- AFRICA selection and data cleaning -------------------

world <- world %>%
  filter(continent == "Africa")
Roads <- Roads %>%
  filter(continent == "Africa")
Ports <- Ports %>%
  st_filter(world)
# Ports_fil <- semi_join(Ports, Prices, by = c("name" = "Market"))
Airports <-Airports %>% 
  st_filter(world)  #only the parts that are in common remain

# The main focus of the paper is on Sub-Saharan Africa
sub_saharan <- world %>%
  filter(subregion %in% c("Eastern Africa", "Middle Africa", "Southern Africa", "Western Africa")|
           name_long == "Sudan" & name_long != "Madagascar")
sub_saharan_borders <- st_union(sub_saharan)

ggplot() +
  geom_sf(data = world)+
  geom_sf(data = Ports, aes(colour = NULL), shape = 2, size = 5, color = "red")+
  geom_sf(data = Airports, aes(colour = NULL), shape = 8, size = 5, color = "blue")+
  theme_minimal()+
  labs(title = "Africans and Sub-Sahara transports")

# ----------------------- sf MULTIPOINT feature of markets ---------------------

Mrk_multipoints <- st_combine(sf_markets)

ggplot()+
  geom_sf(data = world,aes(colour = NULL))+
  geom_sf(data = Mrk_multipoints, shape= 2, size=1,color = "red")+
  geom_sf(data=Roads,aes(colour = scalerank), alpha= 0.2)+
  geom_sf(data = Ports, aes(colour = NULL), shape = 2, size = 5, color = "black")+
  geom_sf(data = Airports, aes(colour = NULL), shape = 8, size = 5, color = "blue")+
  geom_sf(data = sub_saharan_borders, fill = NA, color = "red")+
  theme_minimal()
  

# --------------------- minimum distance of each market to: --------------------

#(i) nearest road:
Mrk_Rds<- st_distance(sf_markets,Roads,by_element = F)
# Trova la distanza minima per ogni mercato
Min_Dis_Mrk_Rds <- apply(Mrk_Rds, 1, min) #mi sono prima calcolato la distanza tra
#ogni mercato e tutte le strade. poi ho preso la minima distanzaq tra ogni mercato
#e le strade perchè l'assuzione è che su un'impotetica matrice i mercati sono sulle 
#righe e sulle colonne ci sono le distanze tra quel mercato ed ogni strada

#(ii) nearest airport;
Mrk_Prt<- st_distance(sf_markets,Ports,by_element = F)
Min_Dis_Mrk_Prt <- apply(Mrk_Prt, 1, min)

#(iii) nearest port;
Mrk_Air<- st_distance(sf_markets,Airports,by_element = F)
Min_Dis_Mrk_Air <- apply(Mrk_Air, 1, min)


# -------------- Market average prices vs. the minimum distances ---------------

harvest_means <- apply(Prices[sapply(Prices, is.numeric)], 1, function(x) {
  non_zero_values <- x[x != 0]  # Select non-zero values
  if (length(non_zero_values) == 0) {
    return(NA)  # Return NA if the row contains only zeros
  } else {
    return(mean(non_zero_values))  # Compute mean of non-zero values
  }
})

Prices$serial_means <- harvest_means

#Prices <- Prices %>%
  #select(mktcode, country, market, crop, serial_means)

Prices <- Prices %>%
  select(mktcode, country, market, crop, serial_means)%>%
  group_by(market) %>%
  mutate(avg_mrk_price = mean(serial_means, na.rm = TRUE)) %>%
  distinct(market, .keep_all = TRUE) %>%  # Mantieni solo una riga per ogni mercato
  select(-crop, -serial_means)  # Rimuovi le colonne 'crop' e 'serial_mean'

#just the "agricultural/harvest" price
#Prices <- Prices %>%
  #mutate(avg_price= (Prices$Maize + Prices$Millet + Prices$Rice + Prices$Sorghum + Prices$Wheat)/5) #to check
#Market_price <- sf_markets %>%
  #left_join(Prices, by = "mktcode" )
Prices_fil <- Prices %>%
  semi_join(sf_markets, by = "mktcode")


# Aggiungi il vettore Min_Dis_Mrk_... al dataframe
Prices_fil$Min_Dis_Mrk_Rds <- Min_Dis_Mrk_Rds #tocca da vede se so giusti i mecari. probabilmente no quindi bisognerà mergare da qualche parte all'inizio
Prices_fil$Min_Dis_Mrk_Prt <- Min_Dis_Mrk_Prt
Prices_fil$Min_Dis_Mrk_Air <- Min_Dis_Mrk_Air

Market_price <- Prices_fil %>%
  left_join(sf_markets, by = "mktcode")

#par(mfrow = c(3, 1))

# Ports with ggplot
#ggplot(Market_price, aes(x = avg_mrk_price, y = Min_Dis_Mrk_Prt)) +
  #geom_point() +
  #labs(title = "scatter prices-ports", x = "average price", y = "minimum ports distance") +
  #theme_minimal()
# Ports with plot
plot(Market_price$avg_mrk_price, Market_price$Min_Dis_Mrk_Prt,
     main = "Scatterplot",  
     xlab = "price",  
     ylab = "distance_mrk_Prt",             
     pch = 19,                    
     col = "blue")

# Roads with ggplot
#ggplot(Market_price, aes(x = avg_mrk_price, y = Min_Dis_Mrk_Rds)) +
  #geom_point() +
  #labs(title = "scatter prices-ports", x = "average price", y = "minimum ports distance") +
  #theme_minimal()
# Roads with plot
plot(Market_price$avg_mrk_price, Market_price$Min_Dis_Mrk_Rds,
     main = "Scatterplot",  
     xlab = "price",  
     ylab = "distance_mrk_Rds",             
     pch = 19,                    
     col = "blue")

# Airports with ggplot
#ggplot(Market_price, aes(x = avg_mrk_price, y = Min_Dis_Mrk_Air)) +
  #geom_point() +
  #labs(title = "scatter prices-ports", x = "average price", y = "minimum ports distance") +
  #theme_minimal()
# Airports with plot
plot(Market_price$avg_mrk_price, Market_price$Min_Dis_Mrk_Air,
     main = "Scatterplot",  
     xlab = "price",  
     ylab = "distance_mrk_Air",             
     pch = 19,                    
     col = "blue")

