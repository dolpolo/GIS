#Commands to set the directory:
getwd()
path <- "C:/Users/Davide/Desktop/Alma Mater/SECOND YEAR/GIS/Exam"
setwd(path)

#libraries
# Data Wrangling and Transformation
library(dplyr)
library(tidyverse) 
library(readxl)

# Spatial Data Handling and Manipulation
library(sf) # for Vector Data
library(sfnetworks)
library(nngeo)
library(spData)
library(terra) # for Raster Data
library(ncdf4)
library(exactextractr)

# Data Visualization
library(ggplot2)
library(viridis)
library(ggimage)

# conflicts in terra
library(conflicted)
conflict_prefer("extract", "terra")
conflict_prefer("filter", "dplyr") 

# ------------------------------- Final Exam Tasks -----------------------------

# Section A: Research Proposal
# Section B: Task replication + non mandatory tasks


# -------------------------------- Section B: ---------------------------------
# ******************************** Mandatory Tasks **************************** #

##################################  Task 1  ####################################

# Load the shapefile of countries, keep Serbia, and call it “shp”. 
shp <- read_sf("data/Countries/ne_10m_admin_0_countries.shp") 
shp <- shp %>%
  filter( SU_A3 == "SRB")

# Load the shapefile of European NUTS 3 regions, keep the NUTS 3 of Serbia, and call it “shp3”
shp3 <- read_sf("data/NUTS_RG_01M_2021_4326_LEVL_3/NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp")
# codice RS perché questo è il codice ISO 3166-1 alpha-2 assegnato alla Serbia
shp3 <- shp3 %>%
  filter( CNTR_CODE == "RS")

# Create a boundary box containing the shapefile of Serbia with a small margin.
box <- st_bbox(shp)
box <- box+c(-.5,-.5,.5,.5)

# Creazione di un oggetto sf a partire dal bounding box
box_sf <- st_as_sfc(box)

# Plotting della figura con il bounding box
ggplot() +
  geom_sf(data = shp) +              # Plotta il tuo shapefile
  geom_sf(data = shp3) +
  geom_sf(data = box_sf, fill = NA, color = "red") +  # Plotta il bounding box in rosso
  theme_minimal()         



##################################  Task 2  ####################################

# Load SPEI data and crop it around the box using crop(). 
spei_data <- rast("data/spei01.nc")
cropped_spei <- crop(spei_data, box)

# Keep the last six layers: SPEI values from 2015 to 2020.

# Check how many layers are in the SPEI data
n_layers <- nlyr(cropped_spei)

# Extract the last six layers (assuming they correspond to 2015-2020)
reduced_spei <- cropped_spei[[ (n_layers-5):n_layers ]]

# Rename these layers sequetinally from 2015 to 2020
names(reduced_spei)
names(reduced_spei) <- c("SPEI_2015", "SPEI_2016", "SPEI_2017", "SPEI_2018", "SPEI_2019", "SPEI_2020")

# Imposta il layout per i grafici and reset itr at the end. 
par(mfrow = c(2, 3))

# Cicla su ogni layer di reduced_spei per creare i grafici
for (layer_index in 1:nlyr(reduced_spei)) {
  plot(reduced_spei[[layer_index]], main = names(reduced_spei)[layer_index])
  plot(st_geometry(shp3), add = TRUE, border = "black")
}
par(mfrow = c(1, 1))



##################################  Task 3  ####################################

# Fix a seed
set.seed(595)

# Generate a vector of 6 numbers from a uniform distribution (min = 0, max = 1)
random_vector <- runif(6, min = 0, max = 1)

# Multiply the 6 SPEI layers by those random numbers
for (i in 1:6) {
  reduced_spei[[i]] <- reduced_spei[[i]] * random_vector[i]
}

# The same was possible manually:
# reduced_spei[[1]] <- reduced_spei[[1]] * random_vector[1]
# reduced_spei[[2]] <- reduced_spei[[2]] * random_vector[2]
# ...



##################################  Task 4  ####################################

# Extract the grid points of the SPEI raster data. 
points <- as.points(reduced_spei)
points
points_sf <- st_as_sf(points)

ggplot()+
  geom_sf(data = points_sf)



##################################  Task 5  ####################################

# Extract the SPEI data for each polygon in “shp” and “shp3”. Keep all considered years. 
# Methods A and B are both admissible

# A- TO KEEP ALL YEARS IN A RAW
# Extraxt spei data from "shp": just Serbia
extracted_spei_shp <- extract(reduced_spei, shp, fun=mean, na.rm=TRUE)
# Extraxt spei data from "shp3": all regions in Serbia
extracted_spei_shp3 <- extract(reduced_spei, shp3, fun=mean, na.rm=TRUE)


# B- KEEP JUST YEARS 2015 AND 2016
spei_2015 <- reduced_spei[["SPEI_2015"]]
spei_2020 <- reduced_spei[["SPEI_2020"]]

# Extraxt spei data from "shp": just Serbia in 2015 and 2020
extracted_spei_shp_2015 <- extract(spei_2015, shp, fun = mean, na.rm = TRUE)
extracted_spei_shp_2020 <- extract(spei_2020, shp, fun = mean, na.rm = TRUE)

# Extraxt spei data from "3": all regiones in Serbia in 2015 and 2020
extracted_spei_shp3_2015 <- extract(spei_2015, shp3, fun = mean, na.rm = TRUE)
extracted_spei_shp3_2020 <- extract(spei_2020, shp3, fun = mean, na.rm = TRUE)

# Average SPEI level in Serbia in 2015 and in 2020
mean_spei_serbia_2015 <- mean(c(extracted_spei_shp_2015$SPEI_2015), na.rm = TRUE)
mean_spei_serbia_2020 <- mean(c(extracted_spei_shp_2020$SPEI_2020), na.rm = TRUE)
mean_spei_NUTSserbia_2015 <- mean(c(extracted_spei_shp3_2015$SPEI_2015), na.rm = TRUE)
mean_spei_NUTSserbia_2020 <- mean(c(extracted_spei_shp3_2020$SPEI_2020), na.rm = TRUE)

# To visualize results
cat("Average SPEI in Serbia in 2015:", mean_spei_serbia_2015, "\n")
cat("Average SPEI in Serbia in 2020:", mean_spei_serbia_2020, "\n")
cat("Average SPEI in Serbians regiones in 2015:", mean_spei_NUTSserbia_2015, "\n")
cat("Average SPEI in Serbians regiones in 2020:", mean_spei_NUTSserbia_2020, "\n")

# The calculated Standardized Precipitation-Evapotranspiration Index (SPEI) represents 
# average drought/humidity levels for Serbia. Positive SPEI values indicate humid conditions, 
# meaning precipitation exceeds evapotranspiration (higher moisture compared to the average). 
# Although still low, the SPEI value is slightly more positive than in 2015, suggesting a minor 
# increase in average humidity in 2020 compared to 2015, though not significant enough to 
# indicate a true humid climate anomaly.



##################################  Task 6  ####################################

# Load the provided Aqueduct 4.0 shapefile. Attribute “bws_raw” is the raw value 
# of the Aqueduct water stress index. For our purposes, we will assume “bws_raw” 
# is the water stress index of each watershed in 2015.
sf_Acqueduct <- read_sf("data/Aqueduct/Aqueduct_baseline.shp")



##################################  Task 7  ####################################

# Generate a water stress time series afte seting the seed
set.seed(565) 

# assume “bws_raw” is the water stress index of each watershed in 2015 and add 
# the vector to the sf_Aqueduct 
length(sf_Acqueduct$bws_raw)
water_stress_2015 <- sf_Acqueduct$bws_raw 
sf_Acqueduct$water_stress_2015 <- water_stress_2015

# Generate a matrix of random multipliers for 5 years (2016 to 2020) for 382 elements
multipliers <- matrix(rnorm(5 * 382, mean = 1, sd = 1), nrow = 382, ncol = 5)

# Calculate the annual values by multiplying the 2015 values by the random multipliers
water_stress_ts <- water_stress_2015 * multipliers

# Add those attributes to the Aqueduct shapefile
sf_Acqueduct$water_stress_2016 <- water_stress_ts[, 1] # Values for 2016
sf_Acqueduct$water_stress_2017 <- water_stress_ts[, 2] # Values for 2017
sf_Acqueduct$water_stress_2018 <- water_stress_ts[, 3] # Values for 2018
sf_Acqueduct$water_stress_2019 <- water_stress_ts[, 4] # Values for 2019
sf_Acqueduct$water_stress_2020 <- water_stress_ts[, 5] # Values for 2020



##################################  Task 8  ####################################

# Rasterize the values of “water_stress_20XX” attributes using terra, and then 
# extract them to the shapefiles of Serbia and Serbian NUTS 3. What is the 
# average water stress level in Serbia in 2020? Is this value equal to the simple
# average of water stress in Serbian NUTS 3 districts? Discuss this shortly.

# Rasterize water stress values for each year (2015-2020)
#Use the extent of Serbia to create empty rasters for each year

# Define the extent and parameters
bbox_extent <- ext(vect(box_sf))
years <- 2015:2020  # Years for which to create the rasters

# Initialize a list to store the rasters
rasters <- list()

# Loop to create and rasterize for each year
for (year in years) {
  raster <- rast(bbox_extent, ncol = 1000, nrow = 1000)
  field_name <- paste0("water_stress_", year)
  raster <- rasterize(vect(sf_Acqueduct), raster, field = field_name)
  rasters[[as.character(year)]] <- raster
}

# Plot each raster with titles for each year
par(mfrow = c(2, 3))
for (year in 2015:2020) {
  plot(rasters[[as.character(year)]], main = paste("Water Stress in Serbia -", year))
}
par(mfrow = c(1, 1))

#Extract raster values to Serbia shapefile
water_stress_2020_serbia <- exact_extract(rasters[[6]], shp, 'mean')

#Extract raster values to Serbian NUTS3 shapefile
water_stress_2020_nuts3 <- exact_extract(rasters[[6]], shp3, 'mean')

#Calculate average water stress in Serbia for 2020
avg_water_stress_2020_serbia <- mean(water_stress_2020_serbia, na.rm = TRUE)
avg_water_stress_2020_serbia

#Calculate average water stress in Serbian NUTS3 regions for 2020
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

# Load the provided population density data for 2015, crop it around the boundary 
# box already defined. What is the average population density level in Serbia? 
# Now, resample it on the resolution of SPEI data using the “bilinear” method. 
# Is the Serbian average population density changed? Discuss this shortly.

# Load the data on Serbian population density
population_density <- rast("data/gpw-v4-population-density-rev11_2015_15_min_asc/gpw_v4_population_density_rev11_2015_15_min.asc")

# Crop them on the box
cropped_population_density <- crop(population_density, box)

# Population density plot
plot(cropped_population_density) # high density in Belgrado
plot(shp3$geometry,add = T)

summary(cropped_population_density)

avg_pop_den <- exact_extract(
  x = cropped_population_density,
  y = shp,
  fun = 'mean')
avg_pop_den # 94.07967 persone per km^2

# Control the CRS of the two objects before proceding
spei_data
cropped_population_density
if (crs(cropped_population_density) != crs(reduced_spei)) {
  cropped_population_density <- project(cropped_population_density,
                                        crs(reduced_spei))
}

# Risampling
resampled_population_density <- resample(cropped_population_density,
                                         reduced_spei, method = "bilinear")

# Resampled population density plot
summary(resampled_population_density)
plot(resampled_population_density)
plot(shp3$geometry,add = T)

avg_pop_den_2 <- exact_extract(
  x = resampled_population_density,
  y = shp,
  fun = 'mean')
avg_pop_den_2 # 95.35654 the less the resolution the widther the high populated density area


# plottare vicino per capire la differenza



# -------------------------------- Section B: ---------------------------------
# ******************************** Not Mandatory Tasks *********************** #

##################################  Task 10  ###################################

# create a population-weighted version of SPEI and water stress data using the 
# gridded data of population density. # spei rispetti a popolazione e spei rispetto a water stress

# Step 1: Calculate the average population density in Serbia
pop_density_serbia <- exact_extract(resampled_population_density, shp, 'mean')
avg_pop_density_serbia <- mean(pop_density_serbia, na.rm = TRUE)

# Step 2: Create the population density ratio for each grid point
pop_density_ratio <- resampled_population_density / avg_pop_density_serbia

# Step 3: Initialize a list to store population-weighted SPEI values and plot them all together
years <- 2015:2020
pop_weighted_spei <- list()

# Loop through each year to calculate and plot the population-weighted SPEI
par(mfrow = c(2, 3))
for (i in seq_along(years)) {
  pop_weighted_spei[[i]] <- reduced_spei[[i]] * pop_density_ratio
  plot(pop_weighted_spei[[i]], main = paste("Population-weighted SPEI for", years[i]))
}
par(mfrow = c(1, 1))


# =============================== CARLO ================================

## Step 1: calculate the average pop.density in Serbia.
pop_density_serbia <- exact_extract(resampled_population_density, shp, 'mean')
pop_density_serbia
# Calculate the average population density for Serbia
avg_pop_density_serbia <- mean(resampled_population_density, na.rm = TRUE)
avg_pop_density_serbia
## Step 2: Create the ratio for each grid point
## pop.density in point i / average pop.density in Serbia

# Create the ratio by dividing the resampled population density raster by the average population density
pop_density_ratio <- resampled_population_density / avg_pop_density_serbia
pop_density_ratio
# Check the result by printing or plotting the ratio
print(pop_density_ratio)
plot(pop_density_ratio, main = "Population Density Ratio (Grid Point / Avg Pop Density in Serbia)")

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
plot(pop_weighted_spei_2020, main = "Population-weighted SPEI for 2020")


# ==============================================================================

##################################  Task 10  ###################################

# In your opinion is it relevant to use population-weighted climate variables when analyzing GDP data? Are they relevant when
# investigating Agricultural GVA or Agricultural yields? And what about Industrial GVA? Discuss this
# shortly (a few lines). 

# Using population-weighted climate variables can be relevant in analyzing GDP data, particularly for understanding how climate impacts economic activity across different sectors.

#When climate data are adjusted based on the distribution of the population in different geographical
#areas, they allow us to measure the impact of climate on more densely populated populations. 
#In the agricultural sector, since agriculture is often concentrated in specific areas, the 
# population weight can provide insights into how climatic variations influence productivity and 
# economic outcomes in densely populated regions. In the context of industrial GVA, the population 
# weight may be less relevant, as industrial activities are often less correlated with population density
# compared to agriculture. Industries can be located in areas with different climatic conditions 
# that do not necessarily align with population distributions.



##################################  Task 11  ###################################

# While higher resolution in population density offers more detail, it risks overfitting 
# when misaligned with climate data. Reducing SPEI resolution to match population density 
# is preferred, as it averages values over relevant spatial units, yielding a more accurate
# population-weighted SPEI index that reflects actual climate impacts on populated areas



##################################  Task 12  ###################################

# Load data with the GVA data of Sector A (NACE classification) andkeep the data from 2015 to 2020 
# for Serbian NUTS 3 regions and before merging the data, drop the Serbian NUTS called “RSZZZ”. 

SRB_secA <- read_xlsx("data/Serbian_gva_sector_a.xlsx", sheet = 2)
SRB_secA <- SRB_secA %>%
  filter(TERRITORY_ID != "RSZZZ")%>%
  select( c(TERRITORY_ID,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`))%>%
  rename(
    agr_gva_2015 = `2015`,
    agr_gva_2016 = `2016`,
    agr_gva_2017 = `2017`,
    agr_gva_2018 = `2018`,
    agr_gva_2019 = `2019`,
    agr_gva_2020 = `2020` 
  )

# Merge them with NUTS3
shp3 <- shp3 %>%
  left_join(SRB_secA, by = join_by(NUTS_ID == TERRITORY_ID))



##################################  Task 13  ###################################

# Generate a new simple feature object called “final_sf” whose features are the 
# Serbian NUTS 3 polygons and whose fields are the average values of SPEI,
# water stress, and Agricultural GVA in each considered year. 

final_sf <- shp3 %>%
  select(NUTS_ID, agr_gva_2015,agr_gva_2016,agr_gva_2017,agr_gva_2018,
         agr_gva_2019, agr_gva_2020,geometry) %>%
  cbind(extracted_spei_shp3, water_stress_matrix) %>%
  select(!ID)



##################################  Task 14  ###################################

# Plot the time series of SPEI and water stress in Serbia from 2015 to 2020.

# Reshape in long form the dataset
final_sf_long <- final_sf %>%
  pivot_longer(cols = starts_with("SPEI_"),  # Seleziona tutte le colonne che iniziano con "SPEI_"
               names_to = "year",             # Nome della nuova colonna per gli anni
               values_to = "spei")            # Nome della nuova colonna per i valori SPEI

# Rimuovi "SPEI_" dal nome dell'anno per ottenere solo il numero
final_sf_long <- final_sf_long %>%
  mutate(year = as.numeric(gsub("SPEI_", "", year)))

# Crea il grafico della serie temporale di SPEI dal 2015 al 2020
ggplot(final_sf_long, aes(x = year, y = spei)) +
  geom_line(color = "blue") +          # Linea di serie temporale
  geom_point(color = "red") +           # Punti per ogni anno
  labs(title = "Time Series of SPEI (2015-2020)",
       x = "Year",
       y = "SPEI") +
  theme_minimal()


##################################  Task 15  ###################################

# Plot the time series of SPEI, water stress, and Agricultural GVA of each Serbian NUTS 3 region.

# Supponiamo che le colonne del dataset siano:
# SPEI_2015, SPEI_2016, ..., SPEI_2020 per SPEI
# GVA_2015, GVA_2016, ..., GVA_2020 per Agricultural GVA
# E una colonna "region" per le regioni NUTS 3

# Riorganizza il dataset in formato lungo per SPEI e GVA
final_sf_long <- final_sf %>%
  pivot_longer(cols = starts_with("SPEI_"), 
               names_to = "year", 
               values_to = "spei") %>%
  mutate(year = as.numeric(gsub("SPEI_", "", year))) %>%
  left_join(
    final_sf %>%
      pivot_longer(cols = starts_with("agr_gva_"), 
                   names_to = "year_gva", 
                   values_to = "gva") %>%
      mutate(year_gva = as.numeric(gsub("agr_gva_", "", year_gva))),
    by = c("region", "year" = "year_gva")
  )

# Crea la serie temporale di SPEI e GVA per ciascuna regione
ggplot(final_sf_long, aes(x = year)) +
  geom_line(aes(y = spei, color = "SPEI"), size = 1) +         # Linea SPEI
  geom_line(aes(y = gva, color = "Agricultural GVA"), size = 1) + # Linea GVA
  facet_wrap(~ region, scales = "free_y") +                    # Facet per regione NUTS 3
  labs(title = "Time Series of SPEI and Agricultural GVA (2015-2020)",
       x = "Year",
       y = "Value",
       color = "Indicator") +
  theme_minimal() +
  scale_color_manual(values = c("SPEI" = "blue", "Agricultural GVA" = "green"))





