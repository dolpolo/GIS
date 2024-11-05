# GIS Project

This repository contains foundational R scripts for spatial data processing, developed for a university GIS course. Each RMarkdown (Rmd) and R file is organized by assignment or the exam names specified in the course.

## Folder Structure

The project is organized into two main folders:

### **Assignment**

This folder includes R and Rmd scripts for two assignments, focusing on spatial *vector* data processing with an economic perspective. Key projects include:

- **`assignment_1.Rmd`**: This script replicates papers that analyze spatial data with an economic focus, using data on attributes such as roads, population density, and electrical connections for countries like Brazil, Vietnam, Ethiopia, and South Africa.  
  **Note**: These datasets are large, and downloading them may cause issues. I plan to make them publicly accessible in a separate folder soon.

- **`netCDF_NASA.Rmd`**: This assignment analyzes the minimum distances from African markets to essential infrastructure, such as:
    - Nearest road
    - Nearest port
    - Nearest airport

    The data, stored in NASA’s netCDF format, is processed to evaluate accessibility for markets across Africa. 

### **Exam**

This folder contains specific tasks focused on processing raster data. These include:
- Raster manipulation and analysis techniques.
- Implementation of spatial functions for GIS-based exam tasks.

- **`FinalExamNetCDF_NASA.Rmd`**: This script focuses on analyzing the **Standardized Precipitation Evapotranspiration Index (SPEI)** data in relation to agricultural productivity and water stress impact. The SPEI index captures water availability by considering both precipitation and evapotranspiration, making it a useful measure for assessing drought impacts. In this exam:
    - **SPEI Data Processing**: import and process SPEI data for specific regions in SERBIA, studied at a national and NUTS3 level, using temporal and spatial filters to focus on areas with significant agricultural activity.
    - **Agricultural Impact Analysis**: link SPEI data to agricultural yield metrics, evaluating how water stress affects crop yields. This involves calculating correlations between SPEI values and agricultural productivity metrics, which may include crop yield or gross agricultural value.
    - **Visualization**: Maps and time series plots are generated to visualize the impact of water availability on agricultural performance over time.
--- 

Please reach out with any questions or for further clarification on the data and methods used in these assignments.
