# Shared setup: load packages and define project-wide paths using {here}
# Source this script at the top of all analyses:
#   source(here::here("analysis", "scripts", "00_Setup.R"))

library(here)
# --- Package loading ----------------------------------------------------------
# Spatial
library(sf)
library(terra)
library(raster) # retained for spatstat compatibility (Figure 6)
library(sp)

# Spatial statistics
library(spatstat)
library(spatstat.geom)

# Visualisation
library(viridis)
library(magick)
library(rnaturalearth)
library(rnaturalearthdata)

# Data manipulation
library(dplyr)
library(purrr)
library(reshape2)
library(broom)
library(magrittr)
library(data.table)

# Iteration / progress
library(foreach)
library(progress)

# Reproducibility
library(grateful)

# --- Project paths ------------------------------------------------------------
# All paths are relative to the repository root, anchored by {here}.
path_shp <- here("data", "raw", "shp") # vector data
path_grid <- here("data", "raw", "grid") # raster covariates
path_derived <- here("data", "derived") # CSV outputs
path_figures <- here("figures") # PDF figures
path_images <- here("images") # field photographs
path_funcs <- here("analysis", "functions") # custom R functions

# --- Source custom functions --------------------------------------------------
source(file.path(path_funcs, "rbias_sitesVSbackground.R"))
source(file.path(path_funcs, "rbias_simulationresults.R"))

# --- Coordinate reference system ----------------------------------------------
crs_utm <- "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
epsg_utm <- 25829 # ETRS89 / UTM zone 29N

message("Setup complete. Project root: ", here())
