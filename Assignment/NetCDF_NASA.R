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
library(sfnetworks)
library(tidygraph)
library(nngeo)

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
  geom_sf(data = world, fill = "#FFFFCC")+
  geom_sf(data = Mrk_multipoints, shape= 2, size=1,color = "darkgreen")+
  geom_sf(data=Roads,aes(colour = NULL), alpha= 0.2)+
  geom_sf(data = Ports, aes(colour = NULL), shape = 2, size = 5, color = "red")+
  geom_sf(data = Airports, aes(colour = NULL), shape = 8, size = 5, color = "blue")+
  geom_sf(data = sub_saharan_borders, fill = NA, color = "brown")+
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(title = "Sub-Saharan African Markets, Ports, and Airports")

ggplot() +
  geom_sf(data = world, fill = "#FFFFCC") +
  geom_sf(data = Mrk_multipoints, shape = 2, size = 1, color = "darkgreen", show.legend = TRUE) +
  geom_sf(data = Roads, alpha = 0.2, color = "gray", show.legend = TRUE) +
  geom_sf(data = Ports, shape = 2, size = 5, color = "blue", show.legend = TRUE) +
  geom_sf(data = Airports, shape = 8, size = 5, color = "red", show.legend = TRUE) +
  geom_sf(data = sub_saharan_borders, fill = NA, color = "brown", show.legend = FALSE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(title = "Sub-Saharan African Markets, Ports, and Airports") +
  guides(
    color = guide_legend(
      override.aes = list(
        shape = c(2, NA, 2, 8),
        color = c("darkgreen", "gray", "blue", "red"),
        size = c(1, 0.2, 5, 5)
      ),
      title = "Legend",
      labels = c("Markets (green)", "Roads (gray)", "Ports (blue)", "Airports (red)")
    )
  )

# ---- tentativi per leggenda
ggplot() +
  geom_sf(data = world, fill = "#FFFFCC") +
  geom_sf(data = Mrk_multipoints, aes(color = "Markets"), size = 1.5, show.legend = TRUE) +
  geom_sf(data = Roads, aes(color = "Roads"), alpha = 0.3, show.legend = TRUE) + 
  geom_sf(data = Ports, aes(color = "Ports"), size = 1.5, show.legend = TRUE) +
  geom_sf(data = Airports, aes(color = "Airports"), size = 1.5, show.legend = TRUE) +
  geom_sf(data = sub_saharan_borders, fill = NA, color = "brown", show.legend = FALSE) +
  scale_color_manual(
    name = "Legend", 
    values = c("Markets" = "darkgreen", "Roads" = "grey", "Ports" = "blue", "Airports" = "red"),
    labels = c("Markets", "Roads", "Ports", "Airports")  
  ) +
  labs(title = "Sub-Saharan African Markets, Ports, and Airports") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
# Il migliore per ora
ggplot() +
  geom_sf(data = world, fill = "#FFFFCC") +
  geom_sf(data = sf_markets, aes(color = "Markets"), size = 1.5) +
  geom_sf(data = Roads, aes(color = "Roads"), alpha = 0.2) + 
  geom_sf(data = Ports, aes(color = "Ports"), size = 5, shape = 2) +
  geom_sf(data = Airports, aes(color = "Airports"), size = 5, shape = 8) +
  geom_sf(data = sub_saharan_borders, fill = NA, color = "brown", show.legend = FALSE) +
  scale_color_manual(
    values = c("Markets" = "darkgreen", "Roads"= "black", "Ports" = "blue", "Airports" = "red"),
    name = "Legend:"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(title = "Sub-Saharan African Markets, Ports, and Airports")

# ---- Marco's contribution ----

sf.Sub_Saharan_Africa <- sf.world %>% 
  filter(continent %in% c('Africa') & !(iso_a2 %in% c('EG','LY','TN','DZ','MA', 'MG', 'EH')) ) 

# Plot Sub Saharan Africa with markets
ggplot()+
  geom_sf(data=sf.Sub_Saharan_Africa, fill = "#FFFFCC") +
  geom_sf(data = sf.markets, col = "black", size = 0.8) +
  theme_minimal() +
  theme( panel.grid.major = element_blank(),  
         panel.grid.minor = element_blank(),   
         axis.text.x = element_blank(),  # Remove x-axis (longitude) text
         axis.text.y = element_blank(),  # Remove y-axis (latitude) text
  ) +
  labs( title = "Sub-Saharan African Markets")

# Plot sub-saharan african ports and airports 
ports <- ports %>%
  st_filter(sf.Sub_Saharan_Africa)
airports <- airports %>% 
  st_filter(sf.Sub_Saharan_Africa)  
roads <- roads %>% 
  st_filter(sf.Sub_Saharan_Africa)

ggplot() +
  geom_sf(data = sf.Sub_Saharan_Africa, fill = "#FFFFCC") +
  geom_sf(data = sf.markets, aes(color = "Markets"), size = 0.8) +
  geom_sf(data = ports, aes(color = "Ports"), size = 1, shape = 2) +
  geom_sf(data = airports, aes(color = "Airports"), size = 1, shape = 8) +
  scale_color_manual(
    values = c("Markets" = "black", "Ports" = "red", "Airports" = "blue"),
    name = "Legend:"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(title = "Sub-Saharan African Markets, Ports, and Airports")


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

# ---- Marco's contribution ----

#(i) Find the nearest distance between markets and roads
mkt_rds<- st_distance(sf_markets, Roads,)
min_dis_mkt_rds <- apply(mkt_rds, 1, min) 

# Add the minimum distances as a new column in the sf.markets data
sf_markets$min_distance_to_road <- min_dis_mkt_rds

# View the first few rows to check the results
print(sf_markets)

sf_Sub_Saharan_Africa <- world %>% 
  filter(continent %in% c('Africa') & !(iso_a2 %in% c('EG','LY','TN','DZ','MA', 'MG', 'EH')) ) 

# Plot the results
ggplot() +
  geom_sf(data = sf_Sub_Saharan_Africa, fill = "#FFFFCC") +
  geom_sf(data = Roads, color = "lightgrey") +
  geom_sf(data = sf_markets, aes(color = min_distance_to_road), size = 0.8) +
  scale_color_gradient(
    low = "blue", high = "red",  
    name = "Distance to Road (m)",
    breaks = seq(0, max(sf_markets$min_distance_to_road), by = 5000)  
  ) +
  labs(title = "Distance from Markets to Nearest Road") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) 

#(ii) Find the nearest distance between markets and airports
mkt_airprt<- st_distance(sf_markets, Airports)
min_dis_mkt_airprt <- apply(mkt_rds, 1, min)
print(min_dis_mkt_airprt)

# Add the minimum distances as a new column in the sf.markets data
sf_markets$min_distance_to_airprt <- min_dis_mkt_airprt

ggplot() +
  geom_sf(data = sf_Sub_Saharan_Africa, fill = "#FFFFCC") +
  geom_sf(data = Airports, size = 1, shape = 8) +
  geom_sf(data = sf_markets, aes(color = min_dis_mkt_airprt), size = 0.8) +
  scale_color_gradient(
    low = "blue", high = "red",  
    name = "Distance to Airport(m)",
    breaks = seq(0, max(sf_markets$min_distance_to_airprt), by = 5000)) +
  labs(title = "Distance from Markets to Nearest Airport") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",  # Adjust legend position
    legend.title = element_text(size = 12),  # Customize legend title size
    legend.text = element_text(size = 10)  # Customize legend text size
  ) 

#(iii) Find the nearest distance between markets and ports
mkt_prt<- st_distance(sf_markets, Ports)
min_dis_mkt_prt <- apply(mkt_airprt, 1, min)
sf_markets$min_distance_to_prt <- min_dis_mkt_prt

ggplot() +
  geom_sf(data = sf_Sub_Saharan_Africa, fill = "#FFFFCC") +
  geom_sf(data = Ports, size = 1, shape = 8) +
  geom_sf(data = sf_markets, aes(color = min_distance_to_prt), size = 0.8) +
  scale_color_gradient(
    low = "blue", high = "red",  
    name = "Distance to Port (km)",
    breaks = c(0, 50000, 100000, 200000, 500000, max(sf_markets$min_distance_to_prt)),
    labels = c("0", "50 km", "100 km", "200 km", "500 km", "Max")
  ) +
  labs(title = "Distance from Markets to Nearest Port") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

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

plot(Market_price$avg_mrk_price, log(Market_price$Min_Dis_Mrk_Rds),
     main = "Scatterplot",  
     xlab = "price",  
     ylab = "log(distance_mrk_Rds)",             
     pch = 19,                    
     col = "blue")

plot(Market_price$avg_mrk_price, sqrt(Market_price$Min_Dis_Mrk_Rds),
     main = "Scatterplot",  
     xlab = "price",  
     ylab = "sqrt(distance_mrk_Rds)",             
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

