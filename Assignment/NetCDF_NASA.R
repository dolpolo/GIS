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

# ------------------------------netCDF_NASA assignment -------------------------
#download the paper’s data [here], combine with world and transportation data (available at NaturalEarth) to:
# 1 Generate an sf MULTIPOINT feature of markets across Africa and plot it;
# 2 Calculate minimum distance of each market to the (i) nearest road, and (ii) nearest port, (iii) nearest airport;
# 3 Produce 3 scatter plots (each for every infrastructure considered) relating market average prices to the minimum distances.

Roads <- read_sf("data/Transportations/Roads/ne_10m_roads.shp")
Ports <- read_sf("data/Transportations/Ports/ne_50m_ports.shp")
Airports <- read_sf("data/Transportations/Airports/ne_50m_airports.shp")
Prices <-read_excel("data/NetCDF/AllocPCT.xlsx")
world <- world
Markets <-read_excel("data/NetCDF/MktCoords.xlsx")
sf_markets <- Markets %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ----------------------- AFRICA selection and data cleaning -------------------

world <- world %>%
  filter(continent == "Africa")
Roads <- Roads %>%
  filter(continent == "Africa")
Prices <- Prices %>% 
  select(-Teff) # likely to be a wrong operation, or to be justified fo NA data

# ----------------------- sf MULTIPOINT feature of markets ---------------------

Mrk_multipoints <- st_combine(sf_markets)
plot(Mrk_multipoints)

ggplot()+
  geom_sf(data = world,aes(colour = NULL))+
  geom_sf(data = Mrk_multipoints, shape= 2, size=1)+
  #geom_sf(data=Roads,aes(colour = type))+
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

#just the "agricultural/harvest" price
Prices <- Prices %>%
  mutate(avg_price= (Prices$Maize + Prices$Millet + Prices$Rice + Prices$Sorghum + Prices$Wheat)/5) #to check


# Aggiungi il vettore y al dataframe
Prices$Min_Dis_Mrk_Rds <- Min_Dis_Mrk_Rds #tocca da vede se so giusti i mecari. probabilmente no quindi bisognerà mergare da qualche parte all'inizio
Prices$Min_Dis_Mrk_Prt <- Min_Dis_Mrk_Prt
Prices$Min_Dis_Mrk_Air <- Min_Dis_Mrk_Air

# Creazione dello scatterplot
ggplot(Prices, aes(x = avg_price, y = Min_Dis_Mrk_Prt)) +
  geom_point() +
  labs(title = "scatter prices-ports", x = "average price", y = "minimum ports distance") +
  theme_minimal()


# Creazione dello scatterplot
plot(Prices$avg_price, Min_Dis_Mrk_Rds,
     main = "Scatterplot",  
     xlab = "price",  
     ylab = "prt",             
     pch = 19,                    
     col = "blue")

#Palesemente va mergato 
