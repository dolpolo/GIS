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

# ----------------------------- Final Exam Structure ------------------------- #

# Section A: 
# Research Proposal

# Section B: 
# Mandatory tasks (task_1 : task_19)
# Not Mandatory tasks: 
# First not mandatory task
# Second not mandatory task
# Third not mandatory task

library(conflicted)
conflict_prefer("extract", "terra")
conflict_prefer("filter", "dplyr")
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
plot(r.pre_mean_annual[[as.character(2019)]])

#Crop the SPEI data around the boundary box of Croatia
cropped_pre <- crop(r.pre_mean_annual, bbox_HRV)
plot(cropped_pre)

#----
#Extract SPEI data for each polygon in 'shp' keeping all considered years(2015-20)
spei_Croatia_all_years <- exact_extract(reduced_spei, shp, 'mean')
spei_Croatia_all_years
#Extract SPEI data for each polygon in 'shp3' keeping all considered years(2015-20)
spei_nuts3_all_years <- exact_extract(reduced_spei, shp3, 'mean')
spei_nuts3_all_years
#Average SPEI level in Croatia in 2015 (corresponding to the first layer):0.08480766
spei_Croatia_all_years[[1]]  

#Average SPEI level in Croatia in 2020 (corresponding to the sixth layer):0.3083895
spei_Croatia_all_years[[6]]  

#Average SPEI level in Croatia regions (NUTS3) in 2015 (0.0850251) & 2020 (0.3005431)
spei_2015_nuts3 <- spei_nuts3_all_years[[1]]
avg_spei_2015_nuts3 <- mean(spei_2015_nuts3, na.rm = TRUE)
avg_spei_2015_nuts3
spei_2020_nuts3 <- spei_nuts3_all_years[[6]]
avg_spei_2020_nuts3 <- mean(spei_2020_nuts3, na.rm = TRUE)
avg_spei_2020_nuts3

#----

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

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!   NOT FINISHED    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Definire il bounding box e la lista anni
bbox_extent <- ext(vect(box_sf))
years <- 2015:2020
rasters <- list()

# Loop per creare e rasterizzare ogni anno
for (year in years) {
  raster <- rast(bbox_extent, ncol = 1000, nrow = 1000)
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

#----
## Step 1: calculate the average pop.density in Croatia.
pop_density_Croatia <- exact_extract(resampled_pop, shp, 'mean')
pop_density_Croatia
# Calculate the average population density for Croatia
avg_pop_density_Croatia <- mean(pop_density_Croatia, na.rm = TRUE)
avg_pop_density_Croatia
## Step 2: Create the ratio for each grid point
## pop.density in point i / average pop.density in Croatia

# Create the ratio by dividing the resampled population density raster by the average population density
pop_density_ratio <- resampled_pop / avg_pop_density_Croatia
pop_density_ratio
# Check the result by printing or plotting the ratio
print(pop_density_ratio)
plot(pop_density_ratio, main = "Population Density Ratio (Grid Point / Avg Pop Density in Croatia)")

## Step 3: Multiply the SPEI value in 2015 by the population density ratio

# Multiply the SPEI value in 2015 by the population density ratio
pop_weighted_spei_2015 <- reduced_spei[[1]] * pop_density_ratio

# Check the result by printing or plotting the population-weighted SPEI for 2015
print(pop_weighted_spei_2015)
plot(pop_weighted_spei_2015, main = "Population-weighted SPEI for 2015")

##Step 4: Multiply the SPEI value in 2016 by the population density ratio
# SPEI value in 2016 is in reduced_spei[[2]]

# Multiply the SPEI value in 2016 by the population density ratio
pop_weighted_spei_2016 <- reduced_spei[[2]] * pop_density_ratio

# Check the result by printing or plotting the population-weighted SPEI for 2016
print(pop_weighted_spei_2016)
plot(pop_weighted_spei_2016, main = "Population-weighted SPEI for 2016")

##Step 5: Multiply the SPEI values for the remaining years (2017-2020) by the population density ratio

# SPEI for 2017
pop_weighted_spei_2017 <- reduced_spei[[3]] * pop_density_ratio
plot(pop_weighted_spei_2017, main = "Population-weighted SPEI for 2017")

# SPEI for 2018
pop_weighted_spei_2018 <- reduced_spei[[4]] * pop_density_ratio
plot(pop_weighted_spei_2018, main = "Population-weighted SPEI for 2018")

# SPEI for 2019
pop_weighted_spei_2019 <- reduced_spei[[5]] * pop_density_ratio
plot(pop_weighted_spei_2019, main = "Population-weighted SPEI for 2019")

# SPEI for 2020
pop_weighted_spei_2020 <- reduced_spei[[6]] * pop_density_ratio
plot(pop_weighted_spei_2020, main = "Population-weighted SPEI for 2020")

##Step 6: Resample the population density ratio raster to match the water stress raster's resolution and extent
pop_density_ratio_resampled <- resample(pop_density_ratio, raster_2015, method = "bilinear")

# Now, you can perform the population-weighted calculations for water stress
# Water stress for 2015
pop_weighted_water_stress_2015 <- raster_2015 * pop_density_ratio_resampled
plot(pop_weighted_water_stress_2015, main = "Population-weighted Water Stress for 2015")

# Water stress for 2016
pop_weighted_water_stress_2016 <- raster_2016 * pop_density_ratio_resampled
plot(pop_weighted_water_stress_2016, main = "Population-weighted Water Stress for 2016")

# Water stress for 2017
pop_weighted_water_stress_2017 <- raster_2017 * pop_density_ratio_resampled
plot(pop_weighted_water_stress_2017, main = "Population-weighted Water Stress for 2017")

# Water stress for 2018
pop_weighted_water_stress_2018 <- raster_2018 * pop_density_ratio_resampled
plot(pop_weighted_water_stress_2018, main = "Population-weighted Water Stress for 2018")

# Water stress for 2019
pop_weighted_water_stress_2019 <- raster_2019 * pop_density_ratio_resampled
plot(pop_weighted_water_stress_2019, main = "Population-weighted Water Stress for 2019")

# Water stress for 2020
pop_weighted_water_stress_2020 <- raster_2020 * pop_density_ratio_resampled
plot(pop_weighted_water_stress_2020, main = "Population-weighted Water Stress for 2020")

#----


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
  dplyr::select(TERRITORY_ID, SECTOR, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`) %>%
  rename(NUTS_ID = TERRITORY_ID)


#Load Croatian gva sector a data (Sheet 2)
Croatiangva_path <- "data/Urban_data/ARDECO_SUVGZ.csv"
Croatian_GVA <- read_csv(Croatiangva_path,show_col_types = FALSE)
Croatian_GVA <- Croatian_GVA %>%
  as_tibble() %>% 
  filter(SECTOR == "F") %>%
  dplyr::select(TERRITORY_ID, SECTOR, `2015`, `2016`, `2017`, `2018`, `2019`, `2020`) %>%
  rename(NUTS_ID = TERRITORY_ID)

#Merge shp3 and Croatian_GVA
shp3_gva_merged = shp3 %>%
  left_join(Croatian_GVA, by = "NUTS_ID") %>%
  left_join(Croatian_Hours_worked, by = "NUTS_ID")

#############
# POINT 13  #
#############

#Extract the average value of SPEI for each NUTS in each year
spei_avg_nuts3 <- shp3 %>%
  mutate(
    spei_2015 = exact_extract(pop_weighted_spei_2015, shp3, 'mean'),
    spei_2016 = exact_extract(pop_weighted_spei_2016, shp3, 'mean'),
    spei_2017 = exact_extract(pop_weighted_spei_2017, shp3, 'mean'),
    spei_2018 = exact_extract(pop_weighted_spei_2018, shp3, 'mean'),
    spei_2019 = exact_extract(pop_weighted_spei_2019, shp3, 'mean'),
    spei_2020 = exact_extract(pop_weighted_spei_2020, shp3, 'mean')
  )

#Extract the average water stress for each NUTS in each year
water_stress_avg_nuts3 <- spei_avg_nuts3 %>%
  mutate(
    water_stress_2015 = exact_extract(pop_weighted_water_stress_2015, shp3, 'mean'),
    water_stress_2016 = exact_extract(pop_weighted_water_stress_2016, shp3, 'mean'),
    water_stress_2017 = exact_extract(pop_weighted_water_stress_2017, shp3, 'mean'),
    water_stress_2018 = exact_extract(pop_weighted_water_stress_2018, shp3, 'mean'),
    water_stress_2019 = exact_extract(pop_weighted_water_stress_2019, shp3, 'mean'),
    water_stress_2020 = exact_extract(pop_weighted_water_stress_2020, shp3, 'mean')
  )

#Merge with Croatian_GVA_filtered
final_sf <- water_stress_avg_nuts3 %>%
  left_join(Croatian_GVA, by = "NUTS_ID") %>%
  rename_with(.cols = starts_with("20"), .fn = ~ paste0("agr_gva_", .x))

#############
# POINT 14  #
#############

# Calculate the averages for each year 
spei_averages <- final_sf %>%
  summarize(across(starts_with("spei_"), mean, na.rm = TRUE))
spei_averages
water_stress_averages <- final_sf %>%
  summarize(across(starts_with("water_stress_"), mean, na.rm = TRUE))
water_stress_averages
spei_averages = st_drop_geometry(spei_averages)
water_stress_averages = st_drop_geometry(water_stress_averages)

#Combine averages into a single data frame
averages_df <- data.frame(
  Year = 2015:2020,
  SPEI = as.numeric(spei_averages),
  Water_Stress = as.numeric(water_stress_averages)
)
averages_df

#Plot time series for SPEI and water stress

ggplot(averages_df, aes(x = Year)) +
  geom_line(aes(y = SPEI, color = "SPEI"), size = 1.2) +
  geom_line(aes(y = Water_Stress, color = "Water Stress"), size = 1.2) +
  labs(title = "SPEI e Water Stress in Croatia (2015-2020)",
       x = "Anno",
       y = "Media") +
  scale_color_manual("", values = c("SPEI" = "blue", "Water Stress" = "red")) +
  theme_minimal() +
  theme(legend.position = "top")

#############
# POINT 15  #  
#############

#Convert data in long form
final_long <- final_sf %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = starts_with("spei_") | starts_with("water_stress_") | starts_with("agr_gva_"),
    names_to = c("Indicator", "Year"),
    names_pattern = "(.*)_(\\d+)",
    values_to = "Value"
  ) %>%
  mutate(Year = as.numeric(Year))

#Filter data for each indicator and plot

#SPEI plot
ggplot(final_long %>% filter(Indicator == "spei"), aes(x = Year, y = Value, color = NUTS_ID, group = NUTS_ID)) +
  geom_line(alpha = 0.6) +
  geom_smooth(aes(group = 1), color = "black", size = 1, method = "loess") +
  labs(title = "Regional Time Series for SPEI", x = "Year", y = "SPEI") +
  theme_minimal() +
  theme(legend.position = "right")

# Water Stress plot
ggplot(final_long %>% filter(Indicator == "water_stress"), aes(x = Year, y = Value, color = NUTS_ID, group = NUTS_ID)) +
  geom_line(alpha = 0.8) +
  geom_smooth(aes(group = 1), color = "black", size = 1, method = "loess") +
  labs(title = "Regional Time Series for Water Stress", x = "Year", y = "Water Stress") +
  theme_minimal() +
  theme(legend.position = "right")

# Agricultural GVA plot
ggplot(final_long %>% filter(Indicator == "agr_gva"), aes(x = Year, y = Value, color = NUTS_ID, group = NUTS_ID)) +
  geom_line(alpha = 0.6) +
  geom_smooth(aes(group = 1), color = "black", size = 1, method = "loess") +
  labs(title = "Regional Time Series for Agricultural GVA", x = "Year", y = "Agricultural GVA") +
  theme_minimal() +
  theme(legend.position = "right")

#############
# POINT 16  #
#############

#SPEI growth rate from 2015 to 2020
final_sf <- final_sf %>%
  mutate(speigr = (spei_2020 - spei_2015) / spei_2015)

# Plot SPEI growth rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = speigr), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "green", high = "red", 
    midpoint = 0, na.value = "grey50", 
    name = "Growth Rate of SPEI"
  ) +
  labs(
    title = "Growth Rate of SPEI (2015 to 2020) in Croatian NUTS 3 Regions"  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(crs = 4326)

#############
# POINT 17  #
#############

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

#############
# POINT 18  #
#############

final_sf <- final_sf %>%
  mutate(agrgvagr = (agr_gva_2020 - agr_gva_2015) / agr_gva_2015)

#Plot agricultural GVA growth rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = wsgr), color = "white", size = 0.1) +
  scale_fill_gradient2(
    low = "blue", mid = "green", high = "red", 
    midpoint = 0, na.value = "grey50", 
    name = "Growth Rate of agricultural GVA"
  ) +
  labs(
    title = "Growth Rate of agricultural GVA (2015 to 2020) in Croatian NUTS 3 Regions"  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(crs = 3035)

# Plot agricultural GVA growth rate with color distinctions for different values of growth rates
ggplot(data = final_sf) +
  geom_sf(aes(fill = cut(agrgvagr, breaks = c(-Inf, 0, 0.05, Inf), labels = c("Negative Growth", "0-5% Growth", ">5% Growth"))),
          color = "white", size = 0.1) +
  scale_fill_manual(
    values = c("Negative Growth" = "blue", "0-5% Growth" = "lightblue", ">5% Growth" = "white"),
    name = "Growth Rate of Agricultural GVA"
  ) +
  labs(
    title = "Agricultural GVA Growth Rate (2015-2020) in Croatian NUTS 3 Regions",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(crs = 3035)

#############
# POINT 19  #
#############


#################
# NON MANDATORY #
#################

#Ensure `final_sf` is in the correct CRS
final_sf <- st_transform(final_sf, crs = 4326)

#Create a spatial weights matrix using the queen criterion
queen_weights <- poly2nb(final_sf, queen = TRUE)
queen_listw <- nb2listw(queen_weights, style = "W")

#SPEI 2015 Moran's I Test: null rejected
spei_moran <- moran.test(final_sf$spei_2015, queen_listw)
print(spei_moran)

#Agricultural GVA 2015 Moran's I Test: null rejected
agrgva_moran <- moran.test(final_sf$agr_gva_2015, queen_listw)
print(agrgva_moran)

#Calculate the spatial lag for each variable
final_sf <- final_sf %>%
  mutate(
    spei_2015_lag = lag.listw(queen_listw, spei_2015),
    agrgva_2015_lag = lag.listw(queen_listw, agr_gva_2015)
  )

#Standardize values for plotting
final_sf <- final_sf %>%
  mutate(
    spei_2015_scaled = scale(spei_2015),
    spei_2015_lag_scaled = scale(spei_2015_lag),
    agrgva_2015_scaled = scale(agr_gva_2015),
    agrgva_2015_lag_scaled = scale(agrgva_2015_lag)
  )

#Moran scatterplot for SPEI 2015
ggplot(final_sf) +
  geom_point(aes(x = spei_2015_scaled, y = spei_2015_lag_scaled), color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(aes(x = spei_2015_scaled, y = spei_2015_lag_scaled), method = "lm", color = "black") +
  labs(
    x = "SPEI 2015 (standardized)",
    y = "Spatial Lag of SPEI 2015 (standardized)",
    title = "Moran's I Scatterplot for SPEI 2015"
  ) +
  theme_minimal()

# Moran scatterplot for Agricultural GVA 2015
ggplot(final_sf) +
  geom_point(aes(x = agrgva_2015_scaled, y = agrgva_2015_lag_scaled), color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_smooth(aes(x = agrgva_2015_scaled, y = agrgva_2015_lag_scaled), method = "lm", color = "black") +
  labs(
    x = "Agricultural GVA 2015 (standardized)",
    y = "Spatial Lag of Agricultural GVA 2015 (standardized)",
    title = "Moran's I Scatterplot for Agricultural GVA 2015"
  ) +
  theme_minimal()


#################
# NON MANDATORY #
#################

#Load the .gdb file
gdb_path <- "data/aqueduct-4-0-water-risk-data/GDB/Aq40_Y2023D07M05.gdb"

#List layers in the GDB file
gdb_layers <- st_layers(gdb_path)
print(gdb_layers)

#Read the desired layer from the GDB file (replace 'desired_layer' with the correct layer name)
# Assuming 'desired_layer' contains water risk data and "gid_0" represents countries
aqua_sf <- st_read(gdb_path, layer = "baseline_annual")

#Filter the data for Croatia using 'gid_0' column (assuming Croatia is labeled as "SRB" in 'gid_0')
Croatia_aqua_sf <- aqua_sf %>%
  filter(gid_0 == "SRB")  # Adjust "SRB" if Croatia has a different code in the dataset

#Remove any rows with missing data to reduce computational burden
Croatia_aqua_sf <- Croatia_aqua_sf %>%
  drop_na()



#Crop the Aqueduct data to Croatia's NUTS 3 shapefile shp3
Croatia_aqua_cropped <- st_intersection(Croatia_aqua_sf, shp3)
#Plot data
plot(st_geometry(Croatia_aqua_cropped))

#Save the cropped data to a shapefile
#st_write(Croatia_aqua_cropped, "Croatia_aqueduct_data.shp", delete_layer = TRUE)






