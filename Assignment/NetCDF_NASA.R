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

# Convert Markets to sf
sf_markets <- Markets %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# ----------------------- AFRICA selection and data cleaning -------------------

# Restrict the geography to Africa
world <- world %>%
  filter(continent == "Africa")
Roads <- Roads %>%
  filter(continent == "Africa")
Ports <- Ports %>%
  st_filter(world)
Airports <-Airports %>% 
  st_filter(world)  

# The main focus of the paper is on Sub-Saharan Africa
sub_saharan <- world %>%
  filter(subregion %in% c("Eastern Africa", "Middle Africa", "Southern Africa", 
                          "Western Africa")|
           name_long == "Sudan" & name_long != "Madagascar")
sub_saharan_borders <- st_union(sub_saharan)

# ----------------------- # FIRST TASK: MARKET ACROSS AFRICA ---------------------

Mrk_multipoints <- st_combine(sf_markets)

ggplot() +
  geom_sf(data = world, fill = "#FFFFCC") +
  geom_sf(data = sf_markets, aes(color = "Markets"), size = 1.5) +
  geom_sf(data = sub_saharan_borders, fill = NA, color = "brown",
          show.legend = FALSE) +
  scale_color_manual(
    values = c("Markets" = "darkgreen"),
    name = "Legend:"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(title = "Sub-Saharan African Markets")


# Plot the African's markets, roads, Ports, and Airports
ggplot() +
  geom_sf(data = world, fill = "#FFFFCC") +
  geom_sf(data = sf_markets, aes(color = "Markets"), size = 1.5) +
  geom_sf(data = Roads, aes(color = "Roads"), alpha = 0.2) + 
  geom_sf(data = Ports, aes(color = "Ports"), size = 3, shape = 2) +
  geom_sf(data = Airports, aes(color = "Airports"), size = 3, shape = 8) +
  geom_sf(data = sub_saharan_borders, fill = NA, color = "brown",
          show.legend = FALSE) +
  scale_color_manual(
    values = c("Markets" = "darkgreen", "Roads"= "black", "Ports" = "blue",
               "Airports" = "red"),
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

# Note that in both plots the Sub-Saharan Africa is enlightened with a red line. 
# This because in the following task we are going to consider only this region.

# --------------------- # SECOND TASK: COMPUTATION OF DISTANCES --------------------

#  First, let's select only the Sub-Saharan African's countries
sub_saharan <- world %>% 
  filter(!(iso_a2 %in% c('EG','LY','TN','DZ','MA', 'MG', 'EH')) ) 

Airports <- Airports %>% 
  st_filter(sub_saharan) 
Roads <- Roads %>% 
  st_filter(sub_saharan)
Ports <- Ports %>% 
  st_filter(sub_saharan)


# We now compute the minimum distances between each market and the closest:
# 1) ROADS
Mrk_Rds<- st_distance(sf_markets,Roads,by_element = F)
Min_Dis_Mrk_Rds <- apply(Mrk_Rds, 1, min)

# Add the minimum distances as a new column in the sf.markets data
sf_markets$min_distance_to_road <- Min_Dis_Mrk_Rds

# Plot of the distance between each market and the closest road: first define custom 
# breaks and labels for the discrete scale
breaks_roads <- c(0, 500, 1000, 2000, 5000, max(sf_markets$min_distance_to_road))
labels_roads <- c("0-500 m", "500-1000 m", "1000-2000 m", "2000-5000 m", ">5000 m")

# Plot with discrete scale for distance to airports
ggplot() +
  geom_sf(data = sub_saharan, fill = "#FFFFCC") +
  geom_sf(data = Roads, color = "lightgrey") +
  geom_sf(data = sf_markets, aes(color = cut(min_distance_to_road, breaks = breaks_roads, labels = labels_roads)), size = 2.3) +
  scale_color_viridis_d(
    name = "Distance to Roads",
    direction = -1,
    option = "H"
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

# 2) PORTS

Mrk_Prt <- st_distance(sf_markets,Ports,by_element = F)
Min_Dis_Mrk_Prt <- apply(Mrk_Prt, 1, min)

sf_markets$min_distance_to_prt <- Min_Dis_Mrk_Prt

# Plot of the distance between each market and the closest port
# Define custom breaks and labels for the discrete scale
breaks_port <- c(0, 50000, 100000, 200000, 500000, max(sf_markets$min_distance_to_prt))
labels_port <- c("0-50 km", "50-100 km", "100-200 km", "200-500 km", ">500 km")

# Plot of the distance between each market and the closest port
ggplot() +
  geom_sf(data = sub_saharan, fill = "#FFFFCC") +
  geom_sf(data = Ports, size = 5, shape = 2) +
  geom_sf(data = sf_markets, aes(color = cut(Min_Dis_Mrk_Prt, breaks = breaks_port, labels = labels_port)), size = 2.3) +
  scale_color_viridis_d(
    name = "Distance to Port",
    direction = -1,
    option = "H"
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


#3) AIRPORT
Mrk_Air<- st_distance(sf_markets,Airports,by_element = F)
Min_Dis_Mrk_Air <- apply(Mrk_Air, 1, min)

sf_markets$min_distance_to_airprt <- Min_Dis_Mrk_Air

# Plot of the distance between each market and the closest airport
breaks_airport <- c(0, 500, 1000, 2000, 5000, max(sf_markets$min_distance_to_airprt))
labels_airport <- c("0-500 m", "500-1000 m", "1000-2000 m", "2000-5000 m", ">5000 m")

# Plot of the distance between each market and the closest airport
ggplot() +
  geom_sf(data = sub_saharan, fill = "#FFFFCC") +
  geom_sf(data = Airports, size = 3, shape = 8) +
  geom_sf(data = sf_markets, aes(color = cut(Min_Dis_Mrk_Air, breaks = breaks_airport, labels = labels_airport)), size = 2.3) +
  scale_color_viridis_d(
    name = "Distance to Airport",
    direction = -1,
    option = "H"
  ) +
  labs(title = "Distance from Markets to Nearest Airport") +
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

# ---- # THIRD TASK: RELATION BETWEEN MARKET AVERAGE PRICE AND MINIMUM DISTANCES --------
# The average price for each market good in the time series
harvest_means <- apply(Prices[sapply(Prices, is.numeric)], 1, function(x) {
  non_zero_values <- x[x != 0]  
  if (length(non_zero_values) == 0) {
    return(NA)  
  } else {
    return(mean(non_zero_values))  
  }
})

# Add the resulted prices in the main Price data set
Prices$serial_means <- harvest_means

# Take the average price in each market for all groups and drop the duplicates
Prices <- Prices %>%
  select(mktcode, country, market, crop, serial_means)%>%
  group_by(market) %>%
  mutate(avg_mrk_price = mean(serial_means, na.rm = TRUE)) %>%
  distinct(market, .keep_all = TRUE) %>%  # eliminate duplicates
  select(-crop, -serial_means)

# Merge the datasets on Prices and Markets and the minimum distances to get prepared to the analysis
Prices_fil <- Prices %>%
  semi_join(sf_markets, by = "mktcode")

Prices_fil$Min_Dis_Mrk_Rds <- Min_Dis_Mrk_Rds
Prices_fil$Min_Dis_Mrk_Prt <- Min_Dis_Mrk_Prt
Prices_fil$Min_Dis_Mrk_Air <- Min_Dis_Mrk_Air

Market_price <- Prices_fil %>%
  left_join(sf_markets, by = "mktcode")

# ------------------------------ FINAL RESULTS ---------------------------------


#1) ROADS
# Identify the outliers (avg_mrk_price > 30) and create a base
outliers_roads <- Market_price$avg_mrk_price > 30

ggplot(Market_price, aes(x = log(Min_Dis_Mrk_Rds), y = avg_mrk_price)) + 
  geom_point(color = "blue") +  
  labs(title = "Scatterplot Roads (Log Transformed)", 
       x = "Log(distance_mrk_Rds)", 
       y = "price") +
  geom_text(data = Market_price[outliers_roads, ], 
            aes(label = market.x, 
                x = log(Min_Dis_Mrk_Rds),  
                y =  avg_mrk_price), 
            vjust = -0.5,  
            color = "red", 
            size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal()


#2)PORTS
# Identify the outliers (avg_mrk_price > 30) and create a base plot
outliers_ports <- Market_price$avg_mrk_price > 30

ggplot(Market_price, aes(x = Min_Dis_Mrk_Prt , y = avg_mrk_price)) + 
  geom_point(color = "blue") +  
  labs(title = "Scatterplot Ports", x = "distance_mrk_Prt", y = "price") +
  geom_text(data = Market_price[outliers_ports, ], 
            aes(label = market.x), 
            vjust = -0.5,  
            color = "red", 
            size = 3) + 
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal() 


#3) AIRPORTS
# Identify the outliers (avg_mrk_price > 30) and create a base plot
outliers_airports <- Market_price$avg_mrk_price > 30

ggplot(Market_price, aes(x = Min_Dis_Mrk_Air, y = avg_mrk_price)) + 
  geom_point(color = "blue") +  
  labs(title = "Scatterplot Airports", x = "distance_mrk_Air", y ="price" ) +
  geom_text(data = Market_price[outliers_airports, ], 
            aes(label = market.x), 
            vjust = -0.5,  
            color = "red",
            size = 3) +  
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_minimal()

