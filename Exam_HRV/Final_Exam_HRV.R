################################################################################
###############                 CHIARA PERUGINI                  ###############
###############                   ID: 1130360                    ###############
###############           Alma Mater Studiorum - LMEC^2          ###############
###############                  Subject : GIS                  ###############
################################################################################

# Set directory
path <- "C:/Users/david/Desktop/University/GIS/Exam_HRV"
setwd(path)
getwd()

# Install Packages 
# install.packages(c("sf", "spData", "tidyverse", "ggplot2", "osmdata", 
#                   "rnaturalearth", "rnaturalearthdata", "grid", "readxl", 
#                   "dplyr", "terra", "raster", "exactextractr", 
#                   "spdep", "spatialreg"))

# Load libraries
library(sf)
library(spData)
library(tidyverse) 
library(ggplot2)
library(osmdata)
library(rnaturalearth)
library(rnaturalearthdata)
library(grid)
library(readxl)
library(dplyr)
library(terra)
library(raster)
library(exactextractr)
library(spdep)
library(spatialreg)
library(conflicted)
conflict_prefer("extract", "terra")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# ----------------------------- Final Exam Structure ------------------------- #

# Section A: 
# Research Proposal

# Section B: 
# Mandatory tasks (task_1 : task_19)
# Not Mandatory tasks: 
# First not mandatory task
# Second not mandatory task
# Third not mandatory task


# -------------------------------- Section B: -------------------------------- #

# ******************************** Mandatory Tasks *************************** #

##################################  Task 1  ####################################


world <- ne_download(scale = 10, type = "countries", category = "cultural", returnclass = "sf")
shp <- world %>% filter(SOVEREIGNT == "Croatia")
plot(st_geometry(shp), main = "Croatia at 1:10m resolution")

# Load NUTS3 data & keep the NUTS 3 of Croatia
nuts_path <- "data/Natural_Earth/NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp"
nuts3 <- st_read(nuts_path)
shp3 <- nuts3 %>% filter(CNTR_CODE == "HR")

# Create a boundary box containing the shapefile of Croatia with a small margin.
box <- st_bbox(shp)
box <- box+c(-.5,-.5,.5,.5)
box_sf <- st_as_sfc(box)

# Plotting della figura con il bounding box
ggplot() +
  geom_sf(data = shp, fill = "lightblue", color = "grey40", size = 0.2) +
  geom_sf(data = shp3, fill = "palegreen", color = "grey30", size = 0.2) + 
  geom_sf(data = box_sf, fill = NA, color = "red", linetype = "dashed",
          size = 0.8) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue", color = NA),
    panel.grid = element_blank(), # Rimuove la griglia
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic") 
  ) +
  labs(
    title = " NUTS 3 Croatia",
    caption = "Reference: Natural Earth"
  )  


##################################  Task 2  ####################################

#Load SPEI data: Standardized Precipitation Evapotranspiration Index
spei_path <- "data/Climate_data/spei01.nc"
r.spei <- rast(spei_path)
r.spei 

#Convert bbox to sf object for cropping
bbox_HRV <- st_as_sf(box_sf) 
#Crop the SPEI data around the boundary box of Croatia
cropped_spei <- crop(r.spei, bbox_HRV)

# Keep the last six layers
reduced_spei <- cropped_spei[[1435:1440]]


##################################  Task 3  ####################################

#Set seed for reproducibility
set.seed(360)

# Generate a vector of 6 numbers from a uniform distribution (min = 0, max = 1)
random_vector <- runif(6, min = 0, max = 1)

# Multiply the 6 SPEI layers by those random numbers
for (i in 1:6) {
  reduced_spei[[i]] <- reduced_spei[[i]] * random_vector[i]
}

##################################  Task 4  ####################################

# Extract the grid points of the SPEI raster data. 
points <- as.points(reduced_spei)
points
points_sf <- st_as_sf(points)

# Generate the plot of points and overlap this onto the Croatian NUTS3 geometry
ggplot()+
  geom_sf(data = shp3, fill = "palegreen", color = "black") + 
  geom_sf(data = points_sf, color = "red", size = 0.5) +  # Plot the grid points
  theme_minimal() +
  labs(title = "Grid Points of the SPEI Raster Data in Croatian NUTS 3")

#Generate the plot of points and overlap this onto the Croatian geometry
ggplot() +
  geom_sf(data = shp, fill = "lightblue", color = "black") +  # Croatia boundary
  geom_sf(data = points_sf, color = "red", size = 0.5) +  # Plot the grid points
  theme_minimal() +
  labs(title = "Grid Points of the SPEI Raster Data in Croatia")

##################################  Task 5  ####################################

#Load PRE data: Climatic Research Unit Time series
pre_path <- "data/Climate_data/cru_ts4.08.2011.2020.pre.dat.nc"
r.pre <- rast(pre_path)
r.pre 

#Crop the SPEI data around the boundary box of Croatia
cropped_pre <- crop(r.pre, bbox_HRV)

# Keep the last six layers
reduced_pre <- cropped_pre[[475:480]]
plot(reduced_pre)

# Estrarre le date associate ai livelli del raster
time_vals <- time(r.pre)

# Filtrare solo gli anni dal 2015 al 2020
indices <- which(format(time_vals, "%Y") %in% as.character(2015:2020))

# Selezionare il raster con i livelli filtrati
r.pre_2015_2020 <- r.pre[[indices]]

# Creare una variabile con gli anni corrispondenti
years <- format(time_vals[indices], "%Y")

# Calcolare la media annuale usando `tapp()` per aggregare per anno
r.pre_mean_annual <- tapp(r.pre_2015_2020, index = years, fun = mean)

# Stampare il risultato
print(r.pre_mean_annual)

names(r.pre_mean_annual)
names(r.pre_mean_annual) <- unique(format(time_vals[indices], "%Y"))
#Crop the SPEI data around the boundary box of Croatia
cropped_pre <- crop(r.pre_mean_annual, bbox_HRV)
plot(cropped_pre)


##################################  Task 6  ####################################

#Load Acqueduct 4.0 shapefile
aqueduct_path <- "data/Aqueduct_data/Aqueduct_baseline.shp"
aqueduct_shapefile <- st_read(aqueduct_path)


##################################  Task 7  ####################################

set.seed(360)

#Check the number of elements in the column 'bws_raw' (382)
length(aqueduct_shapefile$bws_raw)

#Assume 'bws_raw' is the water stress index of each watershed in 2015
water_stress_2015 <- aqueduct_shapefile$bws_raw 

#Add this vector to the Aqueduct shapefile
aqueduct_shapefile$water_stress_2015 <- water_stress_2015

# Generate a matrix of random multipliers for 5 years (2016 to 2020) for 382 elements
multipliers <- matrix(rnorm(5 * 382, mean = 1, sd = 1), nrow = 382, ncol = 5)

# Calculate the annual values by multiplying the 2015 values by the random multipliers
water_stress_ts <- water_stress_2015 * multipliers

# Add those attributes to the Aqueduct shapefile
aqueduct_shapefile$water_stress_2016 <- water_stress_ts[, 1] # Values for 2016
aqueduct_shapefile$water_stress_2017 <- water_stress_ts[, 2] # Values for 2017
aqueduct_shapefile$water_stress_2018 <- water_stress_ts[, 3] # Values for 2018
aqueduct_shapefile$water_stress_2019 <- water_stress_ts[, 4] # Values for 2019
aqueduct_shapefile$water_stress_2020 <- water_stress_ts[, 5] # Values for 2020

##################################  Task 8  ####################################

# Definire il bounding box e la lista anni
bbox_extent <- ext(vect(box_sf))
years <- 2015:2020
rasters <- list()

# Loop per creare e rasterizzare ogni anno
for (year in years) {
  raster <- rast(bbox_extent, ncol = 250, nrow = 250)
  field_name <- paste0("water_stress_", year)
  raster <- rasterize(vect(aqueduct_shapefile), raster, field = field_name)
  
  # Crop e mask per limitare ai confini della Croazia
  raster <- crop(raster, vect(shp))  # Ritaglia al confine della Croazia
  raster <- mask(raster, vect(shp))  # Maschera per mantenere solo i dati interni
  
  rasters[[as.character(year)]] <- raster
}

# Plot dei raster corretti (limitati ai confini della Croazia)
par(mfrow = c(2, 3))
for (year in years) {
  plot(rasters[[as.character(year)]], main = paste("Water Stress in Croatia -", year))
}
par(mfrow = c(1, 1))

#Extract raster values to Serbia shapefile
water_stress_2020_serbia <- exact_extract(rasters[[6]], shp, 'mean')

#Extract raster values to Serbian NUTS3 shapefile
water_stress_2020_nuts3 <- exact_extract(rasters[[6]], shp3, 'mean')

# What is the average water stress level in Serbia in 2020? Is this value equal 
# to the simple average of water stress in Serbian NUTS 3 districts?

#Calculate average water stress in Serbia for 2020: 0.42
avg_water_stress_2020_serbia <- mean(water_stress_2020_serbia, na.rm = TRUE)
avg_water_stress_2020_serbia


#Calculate average water stress in Serbian NUTS3 regions for 2020: 0.41
avg_water_stress_2020_nuts3 <- mean(water_stress_2020_nuts3, na.rm = TRUE)
avg_water_stress_2020_nuts3

#Extract raster values to Serbian NUTS3 shapefile and stor it into columns
water_stress_2015_nuts3 <- exact_extract(rasters[[1]], shp3, 'mean')
water_stress_2016_nuts3 <- exact_extract(rasters[[2]], shp3, 'mean')
water_stress_2017_nuts3 <- exact_extract(rasters[[3]], shp3, 'mean')
water_stress_2018_nuts3 <- exact_extract(rasters[[4]], shp3, 'mean')
water_stress_2019_nuts3 <- exact_extract(rasters[[5]], shp3, 'mean')

# Initialize an empty list to store water stress values for each year
years <- 2015:2020
water_stress_list <- list()

# Loop through each year to calculate mean water stress and store in the list
for (i in seq_along(years)) {
  water_stress_list[[i]] <- exact_extract(rasters[[i]], shp3, 'mean')
}

# Combine the list into a matrix or data frame, with each column named by the respective year
water_stress_matrix <- do.call(cbind, water_stress_list)
colnames(water_stress_matrix) <- paste0("water_stress_", years) 


##################################  Task 9  ####################################

#Load the population density data for 2015
pop_path <- "data/Population_data/gpw_v4_population_density_rev11_2015_15_min.asc"
r.pop <- rast(pop_path)
r.pop

#Crop the population density data to the bounding box of Croatia
cropped_pop <- crop(r.pop, box_sf)

#Calculate the average population density in Croatia (before resampling): 94.56521
avg_pop_density_before <- exact_extract(cropped_pop, shp, 'mean')
avg_pop_density_before <- mean(avg_pop_density_before, na.rm = TRUE)
avg_pop_density_before  

#Resample the population data to match the resolution of SPEI data using bilinear method
resampled_pop <- resample(cropped_pop, reduced_spei[[1]], method = "bilinear")

# Calculate the average population density in Croatia (after resampling): 95.99534
avg_pop_density_after <- exact_extract(resampled_pop, shp, 'mean')
avg_pop_density_after <- mean(avg_pop_density_after, na.rm = TRUE)
avg_pop_density_after 

#Plot the original and resampled population density data
plot(cropped_pop, main = "Population Density in Croatia - 2015 (Original)")
plot(shp3$geometry,add = T)

plot(resampled_pop, main = "Population Density in Croatia - 2015 (Resampled)")
plot(shp3$geometry,add = T)


#Is the Croatian average population density changed? Discuss this shortly (a few lines).
#   Before resampling the avarage population density in Croatia amounts to 75.59355. Resampling 
#   using the bilinear method, decrised slightly to 75.38249 the avarage population. The
#   bilinear interpolation, changing the resolution, laverages more the more common grids.


# -------------------------------- Section B: -------------------------------- #

# ************************* First Not Mandatory Tasks **************************


# create a population-weighted version of SPEI and water stress data using the 
# gridded data of population density. 

# Calculate the average population density in Serbia
pop_density_Croatia <- exact_extract(resampled_pop, shp, 'mean')
avg_pop_density_Croatia <- mean(pop_density_Croatia, na.rm = TRUE)

# Create the population density ratio for each grid point
pop_density_ratio <- resampled_pop / avg_pop_density_Croatia

# Initialize a list to store population-weighted SPEI values and plot them all together
years <- 2015:2020
pop_weighted_spei <- list()

# Loop through each year to calculate and plot the population-weighted SPEI
par(mfrow = c(2, 3))
for (i in seq_along(years)) {
  pop_weighted_spei[[i]] <- reduced_spei[[i]] * pop_density_ratio
  plot(pop_weighted_spei[[i]], main = paste("Population-weighted SPEI for", years[i]))
}
par(mfrow = c(1, 1))


##################################  Task 10  ###################################

# Is it relevant to use population-weighted climate variables when analyzing GDP data? Are they relevant when investigating Agricultural GVA or Agricultural yields? And what about Industrial GVA? Discuss this shortly
# Using population-weighted climate variables can be relevant in analyzing GDP data, particularly for understanding climate's impact across different sectors. By adjusting climate data based on population distribution, we can better assess how climate affects economic activity, especially in densely populated regions.
# In the agricultural sector, where production is often concentrated in specific areas, population-weighting can provide valuable insights into how climatic variations influence productivity and economic outcomes. For instance, understanding how temperature and precipitation affect crop yields in regions with higher population density can inform agricultural policies and economic forecasts.
# In contrast, the relevance of population-weighted climate variables in industrial GVA may be less pronounced. Industrial activities often occur in diverse geographical areas that do not necessarily align with population densities, making climate impacts more complex. Industries can be located in regions with varying climatic conditions, thus diminishing the utility of population-weighted analysis.
# Overall, while population-weighted climate variables are crucial for understanding agricultural impacts, their relevance for industrial analysis is limited due to the nature of industrial location and climate interaction.

##################################  Task 11  ###################################

# While higher resolution in population density offers more detail, it risks overfitting 
# when misaligned with climate data. Reducing SPEI resolution to match population density 
# is preferred, as it averages values over relevant spatial units, yielding a more accurate
# population-weighted SPEI index that reflects actual climate impacts on populated areas



##################################  Task 12  ###################################

Hours_worked_path <- "data/Urban_data/Hours_Worked.csv"
Croatian_Hours_worked <- read_csv(Hours_worked_path, show_col_types = FALSE)
Croatian_Hours_worked <- Croatian_Hours_worked %>%
  filter(SECTOR == "F") %>%
  select(TERRITORY_ID, SECTOR, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`) %>%
  rename(NUTS_ID = TERRITORY_ID) %>%
  rename_with(~ paste0("HW_", .), starts_with("20"))

#Load Croatian gva sector a data (Sheet 2)
Croatiangva_path <- "data/Urban_data/ARDECO_SUVGZ.csv"
Croatian_GVA <- read_csv(Croatiangva_path,show_col_types = FALSE)
Croatian_GVA <- Croatian_GVA %>%
  select(TERRITORY_ID, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`) %>%
  rename(NUTS_ID = TERRITORY_ID)%>%
  rename_with(~ paste0("agr_gva_", .), starts_with("20"))

#Merge shp3 and Croatian_GVA
shp3_merged = shp3 %>%
  left_join(Croatian_Hours_worked, by = "NUTS_ID") %>%
  left_join(Croatian_GVA, by = "NUTS_ID")

shp3_merged = shp3_merged[!duplicated(shp3_merged$NUTS_ID), ]


##################################  Task 13  ###################################


# Generate a new simple feature object called “final_sf” whose features are the 
# Serbian NUTS 3 polygons and whose fields are the average values of SPEI,
# water stress, and Construction  GVA in each considered year. 

# Extraxt spei data from "shp": just Serbia
extracted_spei_shp <- extract(reduced_spei, shp, fun=mean, na.rm=TRUE)
# Extraxt spei data from "shp3": all regions in Serbia
extracted_spei_shp3 <- extract(reduced_spei, shp3, fun=mean, na.rm=TRUE)

final_sf <- shp3_merged %>%
  select(NUTS_ID, agr_gva_2015,agr_gva_2016,agr_gva_2017,agr_gva_2018,
         agr_gva_2019, agr_gva_2020,geometry) %>%
  cbind(extracted_spei_shp3, water_stress_matrix) %>%
  select(!ID)

final_sf <- final_sf %>%
  rename(
    SPEI_2015 = spei_1435,
    SPEI_2016 = spei_1436,
    SPEI_2017 = spei_1437,
    SPEI_2018 = spei_1438,
    SPEI_2019 = spei_1439,
    SPEI_2020 = spei_1440
  )

# compute the avereges for each variable
final_sf_avg <- final_sf %>%
  summarise(dplyr::across(where(is.numeric), mean, na.rm = TRUE))


##################################  Task 14  ###################################

# Plot the time series of SPEI and water stress in Serbia from 2015 to 2020.

# Reshape in long form the dataset
final_SPEI_sf_long_avg <- final_sf_avg %>%
  pivot_longer(cols = starts_with("SPEI_"), 
               names_to = "year",             
               values_to = "spei")  

# Remove "SPEI_"from the year name and leave just the number
final_SPEI_sf_long_avg <- final_SPEI_sf_long_avg %>%
  mutate(year = as.numeric(gsub("SPEI_", "", year)))

# generate the graph for the time series of SPEI from 2015 to 2020
SPEI_avg_time_series <- ggplot(final_SPEI_sf_long_avg, aes(x = year, y = spei)) +
  geom_line(color = "blue") +        
  geom_point(color = "red") +       
  labs(title = "Time Series of SPEI (2015-2020)",
       x = "Year",
       y = "SPEI") +
  theme_minimal()
SPEI_avg_time_series

final_water_sf_long_avg <- final_sf_avg %>%
  select(water_stress_2015, water_stress_2016, water_stress_2017, water_stress_2018, 
         water_stress_2019, water_stress_2020) %>%
  pivot_longer(cols = starts_with("water_stress"),  
               names_to = "year",   
               values_to = "water_stress")

# Remove "water_stress_" from the year name and leave just the number
final_water_sf_long_avg <- final_water_sf_long_avg %>%
  mutate(year = as.numeric(gsub("water_stress_", "", year)))

# generate the graph for the time series of water stress from 2015 to 2020
water_avg_time_series <- ggplot(final_water_sf_long_avg, aes(x = year, y = water_stress)) +
  geom_line(color = "blue") +  
  geom_point(color = "red") + 
  labs(title = "Time Series of water stress (2015-2020)",
       x = "Year",
       y = "water stress") +
  theme_minimal()
water_avg_time_series

# merge the two datasets
final_combined_long_avg <- final_SPEI_sf_long_avg %>%
  select(year, spei) %>%
  rename(value = spei) %>%
  mutate(variable = "SPEI") %>%
  bind_rows(
    final_water_sf_long_avg %>%
      select(year, water_stress) %>%
      rename(value = water_stress) %>%
      mutate(variable = "Water Stress")
  )

# Generate a combined plot
ggplot(final_combined_long_avg, aes(x = year, y = value, color = variable)) +
  geom_line() +     
  geom_point() +      
  labs(title = "Time Series of SPEI and Water Stress (2015-2020)",
       x = "Year",
       y = "Value") +
  theme_minimal()


##################################  Task 15  ###################################

# Plot the time series of SPEI, water stress, and Agricultural GVA of each Croatian NUTS 3 region.

# Reshape in long form the dataset without avereging by regiones
final_SPEI_sf_long <- final_sf %>%
  select(SPEI_2015, SPEI_2016, SPEI_2017, SPEI_2018, 
         SPEI_2019, SPEI_2020, NUTS_ID) %>%
  pivot_longer(cols = starts_with("SPEI_"), 
               names_to = "year",           
               values_to = "spei") 

# Remove "SPEI_" from the year name and leave just the number
final_SPEI_sf_long <- final_SPEI_sf_long %>%
  mutate(year = as.numeric(gsub("SPEI_", "", year)))

final_water_sf_long <- final_sf %>%
  select(water_stress_2015, water_stress_2016, water_stress_2017, water_stress_2018, 
         water_stress_2019, water_stress_2020, NUTS_ID) %>%
  pivot_longer(cols = starts_with("water_stress_"),  
               names_to = "year",   
               values_to = "water_stress")

# Remove "water_stress_" from the year name and leave just the number
final_water_sf_long <- final_water_sf_long %>%
  mutate(year = as.numeric(gsub("water_stress_", "", year)))

final_agr_sf_long <- final_sf %>%
  select(agr_gva_2015, agr_gva_2016, agr_gva_2017, agr_gva_2018, 
         agr_gva_2019, agr_gva_2020, NUTS_ID) %>%
  pivot_longer(cols = starts_with("agr_gva_"),
               names_to = "year",
               values_to = "agr_gva")

# Remove "agr_gva_" from the year name and leave just the number
final_agr_sf_long <- final_agr_sf_long %>%
  mutate(year = as.numeric(gsub("agr_gva_", "", year)))

# Merge the two datasets while preserving NUTS_ID
final_combined_long <- final_SPEI_sf_long %>%
  select(NUTS_ID, year, spei) %>% 
  rename(value = spei) %>%
  mutate(variable = "SPEI") %>%
  bind_rows(
    final_water_sf_long %>%
      select(NUTS_ID, year, water_stress) %>% 
      rename(value = water_stress) %>%
      mutate(variable = "water stress")) %>%
  bind_rows(
    final_agr_sf_long %>%
      select(NUTS_ID, year, agr_gva) %>%
      rename(value = agr_gva) %>%
      mutate(variable = "agr_gva")) %>%
  arrange(NUTS_ID)

# Generate a combined plot
ggplot(final_combined_long, aes(x = year, y = value, color = variable)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ NUTS_ID, scales = "free_y") + 
  labs(title = "Grafici per NUTS_ID",
       x = "Year",
       y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

# SPEI and water stress overlaps

##################################  Task 16  ###################################

# Plot a map showing the growth rate of SPEI from 2015 to 2020 of each Serbian
# NUTS 3 region, in CRS 4326. 

final_sf <- final_sf %>%
  mutate(speigr = (SPEI_2020 - SPEI_2015) / SPEI_2015 * 100) 

ggplot(data = final_sf) +
  geom_sf(aes(fill = speigr), color = "black") +  
  scale_fill_viridis_c(option = "magma", name = "SPEI Growth Rate (%)") + 
  labs(title = "Growth Rate of SPEI from 2015 to 2020 in Serbian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")


# Plot a map showing the growth rate of Water stess from 2015 to 2020 of each Serbian
# NUTS 3 region, in CRS 4326. 

final_sf <- final_sf %>%
  mutate(wsgr = (water_stress_2020 - water_stress_2015) / water_stress_2015)

# Plot water stress growth rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = wsgr), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "green", high = "red", 
    midpoint = 0, na.value = "grey50", 
    name = "Growth Rate of water stress"
  ) +
  labs(
    title = "Growth Rate of water stress (2015 to 2020) in Croatian NUTS 3 Regions"  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(crs = 32634)

##################################  Task 17  ###################################
# BISOGNA METTERE IL PRECIPITATION INDEX


##################################  Task 18  ###################################

# Plot a map showing the growth rate of Agricultural GVA from 2015 to 2020 of each Serbian NUTS 3
# region, in CRS 3035. 

final_sf_3035 <- final_sf %>%
  st_transform(crs = 3035) %>%  # Imposta il CRS a 3035
  mutate(agrgvagr = ( agr_gva_2020 - agr_gva_2015) / agr_gva_2015 * 100) 

ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = agrgvagr), color = "black") +  
  scale_fill_viridis_c(option = "viridis", name = "Agricultural GVA Growth Rate (%)") + 
  labs(title = "Growth Rate of Agricultural GVA from 2015 to 2020 in Serbian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")

#  plot the map again using blue for regions experiencing a negative growth, lightblue
#  for those having a growth rate between 0% and 5%, and white for regions with a growth rate >5%.

# divide growth rates in cathegories
final_sf_3035 <- final_sf_3035 %>%
  mutate(agrvagr_category = case_when(
    agrgvagr < 0 ~ "Negative Growth",
    agrgvagr >= 0 & agrgvagr <= 5 ~ "0-5% Growth",
    agrgvagr > 5 ~ ">5% Growth"
  ))

# generate the same grapth with the bin colours setted above
ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = agrvagr_category), color = "black") +  # Colora in base alla categoria
  scale_fill_manual(values = c("Negative Growth" = "blue", 
                               "0-5% Growth" = "lightblue", 
                               ">5% Growth" = "white"),
                    name = "Agricultural GVA Growth Rate") +
  labs(title = "Growth Rate of Agricultural GVA from 2015 to 2020 in Serbian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")  


##################################  Task 19  ###################################

# Yes, it could be affected by neighbors country investment and a way to check it is through 
# spatial autocorrelation tests such as Moran I test and generally speaking 
# apply a spatial econometric approach that accounts for potential spatial dependencies over time. 


# -------------------------------- Section B: ----------------------------------

# ******************************** Second Not Mandatory Tasks *******************

# Using the “queen” criterion

# compute the Moran’s I Test for SPEI in 2015, and for Agricultural GVA in 2015

final_sf <- st_make_valid(final_sf) # Useful to correct geometries!
final_sf <- st_transform(final_sf, crs = 4326)

# Spatial Lags
k <- poly2nb(final_sf, row.names = final_sf$NUTS_ID)# liste di vicinanza tra ogni poligono
k_weights <- nb2listw(k, style = "W") # does not work. Why? some island

print(summary(k))

# create centroids
coords <- st_coordinates(st_centroid(st_geometry(final_sf))) 
p <- plot(k, coords)

# create Spatial Lags variable
index_lag <- lag.listw(k_weights, final_sf$agr_gva_2015)


# Is population spatially correlated across Europe?

# Moran's I Test
moran1_agr2015 <- moran.test(final_sf$agr_gva_2015,k_weights)
print(moran1_agr2015) #statistically significant with a practical value of 0.58 of autocorrelation


# create a spatial autocorrelation scatterplot for the above-mentioned variables

# Precompute scaled values
final_sf <- final_sf %>%
  mutate(agr_gva_2015_scaled = scale(agr_gva_2015),
         index_lag_scaled = scale(index_lag))

# Compute mean values for horizontal and vertical lines
mean_agr_gva_2015_scaled <- mean(final_sf$agr_gva_2015_scaled, na.rm = TRUE)
mean_index_lag_scaled <- mean(final_sf$index_lag_scaled, na.rm = TRUE)

# Create the plot
ggplot <- ggplot(final_sf) +
  geom_point(aes(x = agr_gva_2015_scaled, y = index_lag_scaled, fill = NUTS_ID), shape = 22, size = 3, alpha = 0.8) +
  geom_hline(yintercept = mean_index_lag_scaled, linetype = "dashed") +
  geom_vline(xintercept = mean_agr_gva_2015_scaled, linetype = "dashed") +
  geom_smooth(aes(x = agr_gva_2015_scaled, y = index_lag_scaled), method = "lm", color = "black") +
  labs(x = "agr_gva_2015 (scaled)", y = "Lagged Index (scaled)", fill = "Country") +
  theme_classic()
ggplot

# Moran’s I doesn’t directly test the causal impact of a lagged variable in neighboring regions .
# To explore the effect of lagged investment in neighboring regions on GDP, a spatial lag model (SLM) 
# or a spatial Durbin model (SDM) would be more appropriate. These models incorporate
# spatially lagged independent variables, allowing for analysis of the potential 
# impact of investment at t−1 in region j on GDP at t in region i.

# -------------------------------- Section B: ----------------------------------

# ******************************** Third Not Mandatory Task: *******************

#Load the .gdb file
gdb_path <- "data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb"

#List layers in the GDB file
gdb_layers <- st_layers(gdb_path)
print(gdb_layers)

#Read the desired layer from the GDB file (replace 'desired_layer' with the correct layer name)
# Assuming 'desired_layer' contains water risk data and "gid_0" represents countries
aqua_sf <- st_read(gdb_path, layer = "baseline_annual")

#Filter the data for Croatia using 'gid_0' column (assuming Croatia is labeled as "SRB" in 'gid_0')
Croatia_aqua_sf <- aqua_sf %>%
  filter(gid_0 == "HRV")  # Adjust "SRB" if Croatia has a different code in the dataset

#Remove any rows with missing data to reduce computational burden
Croatia_aqua_sf <- Croatia_aqua_sf %>%
  drop_na()

#Crop the Aqueduct data to Croatia's NUTS 3 shapefile shp3
Croatia_aqua_sf <- st_make_valid(Croatia_aqua_sf)
shp3 <- st_make_valid(shp3)

# Now perform the intersection
Croatia_aqua_cropped <- st_intersection(Croatia_aqua_sf, shp3)
#Plot data
plot(st_geometry(Croatia_aqua_cropped))

#Save the cropped data to a shapefile
#st_write(Croatia_aqua_cropped, "Croatia_aqueduct_data.shp", delete_layer = TRUE)

# Simplify the geometries to remove tiny, complex shapes
Croatia_aqua_cropped <- st_simplify(Croatia_aqua_cropped, dTolerance = 0.01)  # Adjust tolerance as needed

# Plot simplified geometries
plot(st_geometry(Croatia_aqua_cropped))

# VANNO TOLTE LE CURVE FASTIDIOSE



