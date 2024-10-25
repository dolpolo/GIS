#Commands to set the directory:
getwd()
path <- "C:/Users/Davide/Desktop/Alma Mater/SECOND YEAR/GIS/Exam"
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
library(terra) 
library(ncdf4)
library(exactextractr)
# ------------------------------- Final Exam Tasks -----------------------------

# Section A: Research Proposal
# Section B: Task replication + non mandatory tasks

# ---------------------------------- Section B ---------------------------------

# ---- 1. ----

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

# ---- 2. ANCORA DA FARE ---- 
# Load SPEI data and crop it around the box using crop(). Some grid cells will (partly or completely)
# lay outside the Serbian border. Then, keep the last six layers. Four our purposes, those layers will be
# the SPEI values for the following years: 2015,2016,2017,2018,2019,2020.

spei_data <- rast("data/spei01.nc")
cropped_spei <- crop(spei_data, box)

# Check how many layers are in the SPEI data
n_layers <- nlyr(cropped_spei)

# Extract the last six layers (assuming they correspond to 2015-2020)
reduced_spei <- cropped_spei[[ (n_layers-5):n_layers ]]

# plot(r.spei.layer)
# plot(st_geometry(world),add=T)

# ---- 3. ----

set.seed(595)

random_numbers <- runif(6, min = 0, max = 1)

for (i in 1:6) {
  reduced_spei[[i]] <- reduced_spei[[i]] * random_numbers[i]
}

# The same was possible manually:
# reduced_spei[[1]] <- reduced_spei[[1]] * random_vector[1]
# reduced_spei[[2]] <- reduced_spei[[2]] * random_vector[2]
# ...

# ---- 4.
# Extract the grid points of the SPEI raster data. 
points <- as.points(reduced_spei)
points
points_sf <- st_as_sf(points)

ggplot()+
  geom_sf(data = points_sf)

# ---- 5. NON BENE QUI
# Extract the SPEI data for each polygon in “shp” and “shp3”. Keep all considered years. 
# What is the average SPEI level in Serbia in 2015? And in 2020?

names(reduced_spei)
# rename layers for every fittitious year 
names(reduced_spei) <- c("SPEI_2015", "SPEI_2016", "SPEI_2017", "SPEI_2018", "SPEI_2019", "SPEI_2020")

# Estrai i dati SPEI per ciascun poligono in "shp"
extracted_spei_shp <- extract(spei_data, shp, fun=mean, na.rm=TRUE)

# Estrai i dati SPEI per ciascun poligono in "shp3"
extracted_spei_shp3 <- extract(spei_data, shp3, fun=mean, na.rm=TRUE)

# Estrai i layer specifici per il 2015 e il 2020 (ipotesi: SPEI per anno in ciascun layer)
spei_2015 <- reduced_spei[["SPEI_2015"]]
spei_2020 <- reduced_spei[["SPEI_2020"]]

# Estrai i dati SPEI per ciascun poligono in "shp" per il 2015 e il 2020
extracted_spei_shp_2015 <- extract(spei_2015, shp, fun = mean, na.rm = TRUE)
extracted_spei_shp_2020 <- extract(spei_2020, shp, fun = mean, na.rm = TRUE)

# Estrai i dati SPEI per ciascun poligono in "shp3" per il 2015 e il 2020
extracted_spei_shp3_2015 <- extract(spei_2015, shp3, fun = mean, na.rm = TRUE)
extracted_spei_shp3_2020 <- extract(spei_2020, shp3, fun = mean, na.rm = TRUE)

# Calcola la media del livello SPEI per tutta la Serbia (shp e shp3) nel 2015 e nel 2020
mean_spei_serbia_2015 <- mean(c(extracted_spei_shp_2015$SPEI_2015, extracted_spei_shp3_2015$SPEI_2015), na.rm = TRUE)
mean_spei_serbia_2020 <- mean(c(extracted_spei_shp_2020$SPEI_2020, extracted_spei_shp3_2020$SPEI_2020), na.rm = TRUE)

# Visualizza i risultati
cat("Average SPEI in Serbia in 2015:", mean_spei_serbia_2015, "\n")
cat("Average SPEI in Serbia in 2020:", mean_spei_serbia_2020, "\n")

# Standardized Precipitation-Evapotranspiration Index) che hai calcolato rappresentano 
# valori medi dell’indice di siccità/umidità per la Serbia.
# Valori di SPEI positivi indicano condizioni di umidità, ovvero che il livello di 
# precipitazioni supera l'evapotraspirazione (maggiore umidità rispetto alla media).
# Anche se il valore è ancora basso, è leggermente più positivo rispetto al 2015. 
# Questo potrebbe indicare un lieve aumento dell'umidità media nel 2020 rispetto al 2015, 
# anche se l’incremento non è così significativo da indicare una vera e propria anomalia climatica umida.

# ---- 6. 
# Load the provided Aqueduct 4.0 shapefile. Attribute “bws_raw” is the raw value of the Aqueduct
# water stress index. Fou our purposes, we will assume “bws_raw” is the water stress index of each
# watershed in 2015.
sf_Acqueduct <- read_sf("data/Aqueduct/Aqueduct_baseline.shp")


# ---- 7. 
# Adapt the following lines to generate a water stress time series.

# Set seed for reproducibility
set.seed(565) # insert here the last 3 figures of student ID (matricola)
# Depending on your choices, the number of features may vary
# Assuming aqueduct_shapefile$bws_raw has got 99 elements
length(sf_Acqueduct$bws_raw) # 99
water_stress_2015 <- sf_Acqueduct$bws_raw # A vector of length 99
# Add this vector to the Aqueduct shapefile
sf_Acqueduct$water_stress_2015 <- water_stress_2015
# Generate a matrix of random multipliers for 5 years (2016 to 2020) for 99 elements
multipliers <- matrix(rnorm(5 * 382, mean = 1, sd = 1), nrow = 382, ncol = 5)
# Calculate the annual values by multiplying the 2015 values by the random multipliers
water_stress_ts <- water_stress_2015 * multipliers
# Add those attributes to the Aqueduct shapefile
sf_Acqueduct$water_stress_2016 <- water_stress_ts[, 1] # Values for 2016
sf_Acqueduct$water_stress_2017 <- water_stress_ts[, 2] # Values for 2017
sf_Acqueduct$water_stress_2018 <- water_stress_ts[, 3] # Values for 2018
sf_Acqueduct$water_stress_2019 <- water_stress_ts[, 4] # Values for 2019
sf_Acqueduct$water_stress_2020 <- water_stress_ts[, 5] # Values for 2020


# ---- 8. ANCORA NON VA BENE
# Rasterize the values of “water_stress_20XX” attributes using terra, and then extract them to the
# shapefiles of Serbia and Serbian NUTS 3. What is the average water stress level in Serbia in 2020? Is
# this value equal to the simple average of water stress in Serbian NUTS 3 districts? Discuss this shortly.

# 2. Rasterizza i dati di stress idrico per ogni anno
# Creiamo un raster per ogni anno da 2015 a 2020

ext <- ext(shp)

# 3. Rasterizza i dati di stress idrico per il 2015
# Creiamo un raster vuoto basato sull'estensione dello shapefile della Serbia
water_stress_raster_2015 <- rast(nrows=100, ncols=100, 
                                 xmin=ext[1], 
                                 xmax=ext[2],
                                 ymin=ext[3], 
                                 ymax=ext[4]) 

# Rasterizza i valori dallo shapefile per l'anno 2015
water_stress_raster_2015 <- rasterize(st_as_sf(sf_Acqueduct), 
                                      water_stress_raster_2015, 
                                      field = "water_stress_2015")

# 4. Estrai i valori rasterizzati per la Serbia e i NUTS 3
extracted_water_stress_serbia_2015 <- extract(water_stress_raster_2015, shp, fun = mean, na.rm = TRUE)
extracted_water_stress_nuts3_2015 <- extract(water_stress_raster_2015, shp3, fun = mean, na.rm = TRUE)

# 5. Visualizza i risultati
mean_water_stress_serbia_2015 <- mean(extracted_water_stress_serbia_2015, na.rm = TRUE)
mean_water_stress_nuts3_2015 <- mean(extracted_water_stress_nuts3_2015, na.rm = TRUE)

cat("Average Water Stress in Serbia in 2015:", mean_water_stress_serbia_2015, "\n")
cat("Average Water Stress in NUTS 3 in 2015:", mean_water_stress_nuts3_2015, "\n")


raster_template <- rast(ext(water_stress_data), res=0.01)  # Definisci la risoluzione, ad esempio 0.01 gradi

# Rasterizza l'attributo "water_stress_20XX"
water_stress_raster <- rasterize(water_stress_ts, raster_template, field="water_stress_20XX")

# Estrai i valori raster per i poligoni della Serbia
extracted_serbia <- extract(water_stress_raster, serbia_shp, fun=mean, na.rm=TRUE)

# Estrai i valori raster per i poligoni delle NUTS 3 serbe
extracted_nuts3 <- extract(water_stress_raster, nuts3_shp, fun=mean, na.rm=TRUE)

# ---- 9.
# Load the provided population density data for 2015, crop it around the boundary box already defined.
# What is the average population density level in Serbia? Now, resample it on the resolution of SPEI
# data using the “bilinear” method. Is the Serbian average population density changed? Discuss this
# shortly (a few lines).

population_density <- rast("data/gpw-v4-population-density-rev11_2015_15_min_asc/gpw_v4_population_density_rev11_2015_15_min.asc")

cropped_population_density <- crop(population_density, box)
plot(cropped_population_density) # high density in Belgrado
plot(shp3$geometry,add = T)

summary(cropped_population_density)

avg_pop_den <- exact_extract(
  x = cropped_population_density,
  y = shp,
  fun = 'mean')
avg_pop_den # 94.07967 persone per km^2

# Risampling si riferisce al processo di modificare la risoluzione di un raster esistente

spei_data
cropped_population_density

# Controlla il CRS
if (crs(cropped_population_density) != crs(reduced_spei)) {
  cropped_population_density <- project(cropped_population_density, crs(reduced_spei))
}

# Risampling
resampled_population_density <- resample(cropped_population_density, reduced_spei, method = "bilinear")

# Controllo finale del raster risampled
summary(resampled_population_density)
plot(resampled_population_density)
plot(shp3$geometry,add = T)

avg_pop_den_2 <- exact_extract(
  x = resampled_population_density,
  y = shp,
  fun = 'mean')
avg_pop_den_2 # 95.35654 essendosi abbassata la risoluzione abbiamo una zona dove la densità di popolazione è più ampia

# ------------------------- NON MANDATORY TASKS --------------------------------

# 10 ANCORA MALE
# create a population-weighted version of SPEI and water stress data using the 
# gridded data of population density.

# Hint:
# Step 1: calculate the average pop.density in Serbia.
avg_pop_den 

# Step 2: create the following ratio for each grid point
# pop.density in point i / average pop.density in Serbia
pop_density_ratio <- cropped_population_density / avg_pop_den

# Step 3: multiply the SPEI value in 2015 in point i
# by the ratio calculated at Step 2.
# Moltiplica i valori SPEI per il rapporto della densità di popolazione
spei_weighted_2015 <- spei_2015[[1]] %*% pop_density_ratio  # Supponendo che il 2015 sia il primo layer


# Step 4: multiply the SPEI value in 2016 in point i
# by the ratio calculated at Step 2.
# Step 5: the same for the remaining years.
# Step 6: the same for the water stress index.
# Step 7: from now on use the newly created pop.weighted climate variables.


# 12
SRB_sec <- read_xlsx("data/Serbian_gva_sector_a.xlsx")

