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

# Load & Plot Croatia's NUTS 3 Regions with Bounding Box

# Load Croatia's boundaries (world map)
shp <- ne_download(scale = 10, type = "countries", category = "cultural", returnclass = "sf") %>%
  filter(SOVEREIGNT == "Croatia")

# Load NUTS3 data & filter Croatia
shp3 <- st_read("data/Natural_Earth/NUTS_RG_01M_2021_4326_LEVL_3_repaired.shp") %>%
  filter(CNTR_CODE == "HR")

# Create a bounding box with a margin
box_sf <- st_as_sfc(st_bbox(shp) + c(-0.5, -0.5, 0.5, 0.5))

# Plot Croatia, NUTS3 regions, and bounding box
ggplot() +
  geom_sf(data = shp, fill = "lightblue", color = "grey40", size = 0.2) +
  geom_sf(data = shp3, fill = "palegreen", color = "grey30", size = 0.2) + 
  geom_sf(data = box_sf, fill = NA, color = "red", linetype = "dashed", size = 0.8) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "aliceblue"),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16), 
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic")
  ) +
  labs(title = "NUTS 3 Croatia", caption = "Reference: Natural Earth")

##################################  Task 2  ####################################

# Load & Crop SPEI Data (Climate Data)

# Load SPEI raster data
r.spei <- rast("data/Climate_data/spei01.nc")

# Crop the SPEI data using Croatia's bounding box
cropped_spei <- crop(r.spei, st_as_sf(box_sf))

# Keep the last 6 layers (corresponding to years of interest)
reduced_spei <- cropped_spei[[1435:1440]]


##################################  Task 3  ####################################

# Apply Random Weights to SPEI Layers

set.seed(360)  # Ensure reproducibility
random_vector <- runif(6)  # Generate 6 random values

# Multiply each layer by its corresponding random value
reduced_spei <- lapply(1:6, function(i) reduced_spei[[i]] * random_vector[i]) |> rast()

##################################  Task 4  ####################################

# Convert raster layers to points
points_sf <- as.points(reduced_spei) |> st_as_sf()

# Plot grid points over Croatian NUTS3 regions
ggplot() +
  geom_sf(data = shp3, fill = "palegreen", color = "black") +
  geom_sf(data = points_sf, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Grid Points of the SPEI Raster Data in Croatian NUTS 3")

# Plot grid points over Croatia boundary
ggplot() +
  geom_sf(data = shp, fill = "lightblue", color = "black") +
  geom_sf(data = points_sf, color = "red", size = 0.5) +
  theme_minimal() +
  labs(title = "Grid Points of the SPEI Raster Data in Croatia")


##################################  Task 5  ####################################

#Load PRE data: Climatic Research Unit Time series
pre_path <- "data/Climate_data/cru_ts4.08.2011.2020.pre.dat.nc"
r.pre <- rast(pre_path)
r.pre 

#Crop the SPEI data around the boundary box of Croatia
cropped_pre <- crop(r.pre, box_sf)

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
cropped_pre <- crop(r.pre_mean_annual, box_sf)
plot(cropped_pre)


##################################  Task 6  ####################################

# Load Aqueduct 4.0 shapefile
aqueduct_shapefile <- st_read("data/Aqueduct_data/Aqueduct_baseline.shp")


##################################  Task 7  ####################################

set.seed(360)

# Extract water stress values for 2015
aqueduct_shapefile$water_stress_2015 <- aqueduct_shapefile$bws_raw 

# Generate random multipliers (mean = 1, sd = 1) for years 2016-2020
multipliers <- matrix(rnorm(5 * nrow(aqueduct_shapefile), mean = 1, sd = 1), 
                      nrow = nrow(aqueduct_shapefile), ncol = 5)

# Compute water stress values for each year
water_stress_ts <- aqueduct_shapefile$water_stress_2015 * multipliers

# Add calculated values as new columns
years <- 2016:2020
colnames(water_stress_ts) <- paste0("water_stress_", years)
aqueduct_shapefile <- cbind(aqueduct_shapefile, water_stress_ts)

##################################  Task 8  ####################################

# Define bounding box and years list
bbox_extent <- ext(vect(box_sf))
years <- 2015:2020
rasters <- vector("list", length(years))
names(rasters) <- as.character(years)

# Loop to create and rasterize each year's data
for (year in years) {
  field_name <- paste0("water_stress_", year)
  raster <- rasterize(vect(aqueduct_shapefile), rast(bbox_extent, ncol = 250, nrow = 250), field = field_name)
  rasters[[as.character(year)]] <- mask(crop(raster, vect(shp)), vect(shp))  # Crop and mask for Croatia
}

# Plot raster maps for each year without printing extra lines in the console
par(mfrow = c(2, 3))
invisible(lapply(years, function(y) plot(rasters[[as.character(y)]], main = paste("Water Stress in Croatia -", y))))
par(mfrow = c(1, 1))

# Extract raster values for Serbia and Serbian NUTS3 regions in 2020
water_stress_2020_serbia <- exact_extract(rasters[["2020"]], shp, 'mean')
water_stress_2020_nuts3 <- exact_extract(rasters[["2020"]], shp3, 'mean')

# Compute average water stress levels
avg_water_stress_2020_serbia <- mean(water_stress_2020_serbia, na.rm = TRUE)
avg_water_stress_2020_nuts3 <- mean(water_stress_2020_nuts3, na.rm = TRUE)

# Extract raster values for Serbian NUTS3 regions (2015-2020)
water_stress_matrix <- do.call(cbind, lapply(years, function(y) exact_extract(rasters[[as.character(y)]], shp3, 'mean')))
colnames(water_stress_matrix) <- paste0("water_stress_", years)

# Initialize an empty list to store water stress values for each year
years <- 2015:2020
water_stress_list <- list()

# Loop through each year to calculate mean water stress and store in the list
for (i in seq_along(years)) {
  water_stress_list[[i]] <- exact_extract(rasters[[i]], shp3, 'mean')
}

##################################  Task 9  ####################################

# Load population density data for 2015
pop_path <- "data/Population_data/gpw_v4_population_density_rev11_2015_15_min.asc"
r.pop <- rast(pop_path)

# Crop to Croatia's bounding box
cropped_pop <- crop(r.pop, box_sf)

# Calculate average population density in Croatia before resampling
avg_pop_density_before <- mean(exact_extract(cropped_pop, shp, 'mean'), na.rm = TRUE)

# Resample to match SPEI resolution using bilinear interpolation
resampled_pop <- resample(cropped_pop, reduced_spei[[1]], method = "bilinear")

# Calculate average population density after resampling
avg_pop_density_after <- mean(exact_extract(resampled_pop, shp, 'mean'), na.rm = TRUE)

# Plot original and resampled population density data
par(mfrow = c(1, 2))
plot(cropped_pop, main = "Population Density in Croatia - 2015 (Original)")
plot(shp3$geometry, add = TRUE)
plot(resampled_pop, main = "Population Density in Croatia - 2015 (Resampled)")
plot(shp3$geometry, add = TRUE)
par(mfrow = c(1, 1))

# Summary of changes
cat("Before resampling, the average population density in Croatia was:", avg_pop_density_before, "\n")
cat("After resampling, the average population density in Croatia is:", avg_pop_density_after, "\n")
cat("Resampling slightly altered the average population density due to bilinear interpolation, which adjusts values based on neighboring cells.")

# -------------------------------- Section B: ----------------------------------

# ******************************** First Not Mandatory Tasks *******************

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

# Same for water stress index
water_stress_weighted_list <- list()
par(mfrow = c(2, 3))
for (i in seq_along(years)) {
  water_stress_weighted_list[[i]] <- water_stress_list[[i]] * pop_density_ratio
  plot(water_stress_weighted_list[[i]], main = paste("Population-weighted water-stress for", years[i]))
}
par(mfrow = c(1, 1))


# Same for precipitation data
precipitation_weigthed_list <- list()
par(mfrow = c(2, 3))
for (i in seq_along(years)) {
  precipitation_weigthed_list[[i]] <- reduced_pre[[i]] * pop_density_ratio
  plot(precipitation_weigthed_list[[i]], main = paste("Population-weighted precipitation for", years[i]))
}
par(mfrow = c(1, 1))
##################################  Task 10  ###################################

# Is it relevant to use population-weighted climate variables when analyzing GDP data? Are they relevant when investigating Construction GVA or Construction yields? And what about Industrial GVA? Discuss this shortly
# Using population-weighted climate variables can be relevant in analyzing GDP data, particularly for understanding climate's impact across different sectors. By adjusting climate data based on population distribution, we can better assess how climate affects economic activity, especially in densely populated regions.
# In the Construction sector, where production is often concentrated in specific areas, population-weighting can provide valuable insights into how climatic variations influence productivity and economic outcomes. For instance, understanding how temperature and precipitation affect crop yields in regions with higher population density can inform Construction policies and economic forecasts.
# In contrast, the relevance of population-weighted climate variables in industrial GVA may be less pronounced. Industrial activities often occur in diverse geographical areas that do not necessarily align with population densities, making climate impacts more complex. Industries can be located in regions with varying climatic conditions, thus diminishing the utility of population-weighted analysis.
# Overall, while population-weighted climate variables are crucial for understanding Construction impacts, their relevance for industrial analysis is limited due to the nature of industrial location and climate interaction.

##################################  Task 11  ###################################

# While higher resolution in population density offers more detail, it risks overfitting 
# when misaligned with climate data. Reducing SPEI resolution to match population density 
# is preferred, as it averages values over relevant spatial units, yielding a more accurate
# population-weighted SPEI index that reflects actual climate impacts on populated areas

##################################  Task 12  ###################################

# Load Croatian Hours Worked Data (Sector F)
Hours_worked_path <- "data/Urban_data/Hours_Worked.csv"
Croatian_Hours_worked <- read_csv(Hours_worked_path, show_col_types = FALSE) %>%
  filter(SECTOR == "F") %>%
  select(TERRITORY_ID, SECTOR, `2015`:`2020`) %>%
  rename(NUTS_ID = TERRITORY_ID) %>%
  rename_with(~ paste0("HW_", .), starts_with("20"))

# Load Croatian GVA Data (Sheet 2)
Croatiangva_path <- "data/Urban_data/ARDECO_SUVGZ.csv"
Croatian_GVA <- read_csv(Croatiangva_path, show_col_types = FALSE) %>%
  filter(SECTOR == "F") %>%
  filter(UNIT == "Million EUR" ) %>%
  select(TERRITORY_ID, `2015`:`2020`) %>%
  rename(NUTS_ID = TERRITORY_ID) %>%
  rename_with(~ paste0("cnst_gva_", .), starts_with("20"))


# Rename Iso to match the current nomenclature

Croatian_GVA <- Croatian_GVA %>% 
mutate(NUTS_ID = str_replace_all(NUTS_ID, c("HR047" = "HR021", 
                                              "HR048" = "HR022",
                                              "HR049" = "HR023", 
                                              "HR04A" = "HR024", 
                                              "HR04B" = "HR025",
                                              "HR04C" = "HR026", 
                                              "HR04D" = "HR027", 
                                              "HR04E" = "HR028", 
                                              "HR041" = "HR050", 
                                              "HR046" = "HR061", 
                                              "HR044" = "HR062", 
                                              "HR045" = "HR063", 
                                              "HR043" = "HR064", 
                                              "HR042" = "HR065")))

# Merge shp3 with Croatian Hours Worked & GVA Data
shp3_merged <- shp3 %>%
  left_join(Croatian_Hours_worked, by = "NUTS_ID") %>%
  left_join(Croatian_GVA, by = "NUTS_ID")

##################################  Task 13  ###################################

# Extract SPEI data for Serbia and regions
extracted_spei <- list(
  shp = extract(reduced_spei, shp, fun = mean, na.rm = TRUE),
  shp3 = extract(reduced_spei, shp3, fun = mean, na.rm = TRUE)
)

extracted_pre <- list(
  shp = extract(reduced_pre, shp, fun = mean, na.rm = TRUE),
  shp3 = extract(reduced_pre, shp3, fun = mean, na.rm = TRUE)
)

extracted_pre <- extracted_pre$shp3 %>% select(-ID)

# Merge all data into 'final_sf' and rename SPEI columns
final_sf <- shp3_merged %>%
  select(NUTS_ID, starts_with("cnst_gva"), geometry) %>%
  bind_cols(extracted_spei$shp3, water_stress_matrix, extracted_pre) %>%
  select(-ID) %>%
  rename_with(~paste0("SPEI_", 2015:2020), starts_with("spei_")) %>%
  rename_with(~paste0("pre_", 2015:2020), starts_with("maea_"))

# Compute averages for all numeric columns
final_sf_avg <- final_sf %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

##################################  Task 14  ###################################

# Reshape the data for SPEI and Water Stress
final_SPEI_sf_long_avg <- final_sf_avg %>%
  pivot_longer(cols = starts_with("SPEI_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("SPEI_", "", year)), variable = "SPEI")

final_water_sf_long_avg <- final_sf_avg %>%
  pivot_longer(cols = starts_with("water_stress"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("water_stress_", "", year)), variable = "Water Stress")

# Combine the two datasets into one
final_combined_long_avg <- bind_rows(final_SPEI_sf_long_avg, final_water_sf_long_avg)

# Generate the combined plot
ggplot(final_combined_long_avg, aes(x = year, y = value, color = variable)) +
  geom_line() +     
  geom_point() +      
  labs(title = "Time Series of SPEI and Water Stress (2015-2020)", x = "Year", y = "Value") +
  theme_minimal()

##################################  Task 15  ###################################

# Reshape the data for SPEI, water stress, and Construction GVA
final_SPEI_sf_long <- final_sf %>%
  select(starts_with("SPEI_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("SPEI_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("SPEI_", "", year)), variable = "SPEI")

final_water_sf_long <- final_sf %>%
  select(starts_with("water_stress_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("water_stress_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("water_stress_", "", year)), variable = "Water Stress")

final_pre_sf_long <- final_sf %>%
  select(starts_with("pre_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("pre_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("pre_", "", year)), variable = "Precipitation")

final_cnst_sf_long <- final_sf %>%
  select(starts_with("cnst_gva_"), NUTS_ID) %>%
  pivot_longer(cols = starts_with("cnst_gva_"), names_to = "year", values_to = "value") %>%
  mutate(year = as.numeric(gsub("cnst_gva_", "", year)), variable = "Construction GVA")

# Combine the datasets into one
final_combined_long <- bind_rows(final_SPEI_sf_long, final_water_sf_long, 
                                 final_cnst_sf_long, final_pre_sf_long) %>%
  arrange(NUTS_ID)

# Generate the combined plot
ggplot(final_combined_long, aes(x = year, y = value, color = variable)) +
  geom_line() + 
  geom_point() +
  facet_wrap(~ NUTS_ID, scales = "free_y") + 
  labs(title = "Time Series for each NUTS_ID", x = "Year", y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom")

##################################  Task 16  ###################################

# Calculate the growth rate of SPEI and Water Stress
final_sf <- final_sf %>%
  mutate(
    speigr = (SPEI_2020 - SPEI_2015) / SPEI_2015 * 100, 
    wsgr = (water_stress_2020 - water_stress_2015) / water_stress_2015 * 100,
    pregr = (pre_2020 - pre_2015) / pre_2015 * 100
  )

# Plot SPEI Growth Rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = speigr), color = "black") +  
  scale_fill_viridis_c(option = "magma", name = "SPEI Growth Rate (%)") + 
  labs(
    title = "Growth Rate of SPEI from 2015 to 2020 in Serbian NUTS 3 Regions",
    subtitle = "Percentage Growth Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Plot water stress Growth Rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = wsgr), color = "black") +  
  scale_fill_viridis_c(option = "viridis", name = "water stress Growth Rate (%)") +  
  labs(title = "Growth Rate of water stress from 2015 to 2020 in Serbian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")

##################################  Task 17  ###################################


# Plot SPEI Growth Rate
ggplot(data = final_sf) +
  geom_sf(aes(fill = pregr), color = "black") +  
  scale_fill_viridis_c(option = "rocket", name = "SPEI Growth Rate (%)") + 
  labs(
    title = "Growth Rate of Precipitation from 2015 to 2020 in Croatian NUTS 3 Regions",
    subtitle = "Percentage Growth Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


##################################  Task 18  ###################################

# Plot a map showing the growth rate of Construction GVA from 2015 to 2020 of each Serbian NUTS 3
# region, in CRS 3035. 

final_sf_3035 <- final_sf %>%
  st_transform(crs = 3035) %>%  # Imposta il CRS a 3035
  mutate(cnstgvcnst = ( cnst_gva_2020 - cnst_gva_2015) / cnst_gva_2015 * 100) 

ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = cnstgvcnst), color = "black") +  
  scale_fill_viridis_c(option = "viridis", name = "Construction GVA Growth Rate (%)") + 
  labs(title = "Growth Rate of Construction GVA from 2015 to 2020 in Serbian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")

#  plot the map again using blue for regions experiencing a negative growth, lightblue
#  for those having a growth rate between 0% and 5%, and white for regions with a growth rate >5%.

# divide growth rates in cathegories
final_sf_3035 <- final_sf_3035 %>%
  mutate(cnstvcnst_category = case_when(
    cnstgvcnst < 0 ~ "Negative Growth",
    cnstgvcnst >= 0 & cnstgvcnst <= 5 ~ "0-5% Growth",
    cnstgvcnst > 5 ~ ">5% Growth"
  ))

# generate the same grapth with the bin colours setted above
ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = cnstvcnst_category), color = "black") +  # Colora in base alla categoria
  scale_fill_manual(values = c("Negative Growth" = "blue", 
                               "0-5% Growth" = "lightblue", 
                               ">5% Growth" = "white"),
                    name = "Construction GVA Growth Rate") +
  labs(title = "Growth Rate of Construction GVA from 2015 to 2020 in Serbian NUTS 3 Regions",
       subtitle = "Percentage Growth Rate") +
  theme_minimal() +
  theme(legend.position = "right")  

# divide growth rates in cathegories
final_sf_3035 <- final_sf_3035 %>%
  mutate(cnstvcnst_category = case_when(
    cnstgvcnst < 25 ~ "0-25%",
    cnstgvcnst >= 25 & cnstgvcnst <= 50 ~ "25-50% Growth",
    cnstgvcnst > 50 ~ ">50% Growth"
  ))

# generate the same grapth with the bin colours setted above
ggplot(data = final_sf_3035) +
  geom_sf(aes(fill = cnstvcnst_category), color = "black") +  # Colora in base alla categoria
  scale_fill_manual(values = c("0-25%" = "blue", 
                               "25-50% Growth" = "lightblue", 
                               ">50% Growth" = "white"),
                    name = "Construction GVA Growth Rate") +
  labs(title = "Growth Rate of Construction GVA from 2015 to 2020 in Serbian NUTS 3 Regions",
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

# compute the Moran’s I Test for SPEI in 2015, and for Construction GVA in 2015

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
index_lag <- lag.listw(k_weights, final_sf$cnst_gva_2015)

# Is population spatially correlated across Europe?

# Moran's I Test
moran1_cnst2015 <- moran.test(final_sf$cnst_gva_2015,k_weights)
print(moran1_cnst2015) #statistically significant with a practical value of 0.74 of autocorrelation


# create a spatial autocorrelation scatterplot for the above-mentioned variables

# Precompute scaled values
final_sf <- final_sf %>%
  mutate(cnst_gva_2015_scaled = scale(cnst_gva_2015),
         index_lag_scaled = scale(index_lag))

# Compute mean values for horizontal and vertical lines
mean_cnst_gva_2015_scaled <- mean(final_sf$cnst_gva_2015_scaled, na.rm = TRUE)
mean_index_lag_scaled <- mean(final_sf$index_lag_scaled, na.rm = TRUE)

# Create the plot
ggplot <- ggplot(final_sf) +
  geom_point(aes(x = cnst_gva_2015_scaled, y = index_lag_scaled, fill = NUTS_ID), shape = 22, size = 3, alpha = 0.8) +
  geom_hline(yintercept = mean_index_lag_scaled, linetype = "dashed") +
  geom_vline(xintercept = mean_cnst_gva_2015_scaled, linetype = "dashed") +
  geom_smooth(aes(x = cnst_gva_2015_scaled, y = index_lag_scaled), method = "lm", color = "black") +
  labs(x = "cnst_gva_2015 (scaled)", y = "Lagged Index (scaled)", fill = "Country") +
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

