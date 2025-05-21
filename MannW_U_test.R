## Descriptive approaches
# Load the R packages
spatpack<-c("raster","spatstat","rgdal","maptools",
            "sf", "plotly", "foreach", "dplyr",
            "progress", "purrr", "reshape2", "broom")

lapply(spatpack, require, character.only=TRUE)

# Set working directory
grid <- file.path("grid")
shp <- file.path("shp")

# Define crs
crs = "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Read sites and study area
sites <- sf::st_read(file.path(shp,
                               "sites.shp"),
                     layer = "sites")
sites <- sf::st_transform(sites, 25829)

st_write(sites, "sites.gpkg", driver="GPKG")  # Create a geopackage file
sites <- sf::st_read("shp/sites.gpkg")

study_area <- sf::st_read(file.path(shp,
                                    "study_area.shp"),
                          layer = "study_area")
study_area <- sf::st_transform(study_area, 25829)
study_area_sp <- as(study_area, "Spatial")

# Create the window of analysis
study_area_w <- as.owin(study_area_sp)

# Convertir el shapefile de los castros en un patrón de puntos
sppp_sites <- ppp(x = sites$X, 
                  y = sites$Y,
                  window = study_area_w)

# Read the covariables
dem <- raster::raster("grid/dem.tif")
slope <- raster::raster("grid/slope.tif")
cum_horizon <- raster::raster("grid/cum_horizon.tif")
cum_viewshed <- raster::raster("grid/cum_viewshed.tif")
dist_to_ridges <- raster::raster("grid/dist_to_ridges.tif")
dist_to_rocks <- raster::raster("grid/dist_to_rocks.tif")
dist_to_watedges <- raster::raster("grid/dist_to_wat_edges.tif")
tpi <- raster::raster("grid/tpi.asc")

# Part 1. Calculate and graph Monte Carlo sub-sampled 
# kernel density estimates for random locations vs site locations (based on Bocinsky 2017)

# Compare the sites with the covariables
source('r_functions/rbias_sitesVSbackground.R')
source('r_functions/rbias_simulationresults.R')

# To iterate the process, we create a function
perform_simulation <- function(covariate, covariate_name) {
  simulation_result <- sites_vs_background(covariate = covariate,
                                           sites = sites,
                                           nsim = 999,
                                           covariate_name = covariate_name)
  return(simulation_result)
}

# Define a list of covariates and their corresponding names
covariates <- list(dem = "Altitude (mASL)", 
                   slope = "Slope (degrees)",
                   cum_horizon = "Cumulative horizon",
                   cum_viewshed = "Cumulative viewshed",
                   dist_to_ridges = "Distance to ridges (m)",
                   dist_to_rocks = "Distance to rocky outcrops (m)",
                   dist_to_watedges = "Distance to watershed edges (m)",
                   tpi = "Topographic Prominence Index")

# Iterate over the covariates and perform the simulation for each
simulation_results <- list()
for (covariate_name in names(covariates)) {
  covariate <- get(covariate_name)
  simulation_result <- perform_simulation(covariate, covariates[[covariate_name]])
  simulation_results[[covariate_name]] <- simulation_result
}

# Set up a new PDF device with a nice size
pdf("figures/Figure_TAl.pdf", width = 12, height = 8)

# Adjust for layout
par(mfrow = c(2, 4))

# Iterate over the covariates and their simulation results to create the plots
for (i in seq_along(simulation_results)) {
  covariate_name <- names(simulation_results)[i]
  simulation_result <- simulation_results[[i]]
  
  # Create the plot using simulation_results_plot()
  simulation_results_plot(simulation_result)
}

# Close the PDF device
dev.off()

# Part 2. Compute two-sample Wilcoxon tests (Mann-Whitney U tests)

# Generate random background samples for each covariate
random_samples <- lapply(simulation_results, function(simulation_result) {
  ras_samples <- simulation_result$ras_samples
  lapply(ras_samples, function(ras_sample) {
    sample(ras_sample, length(simulation_result$sites_densities$Covariate), replace = TRUE)
  })
})

# Create a progress bar
pb <- progress_bar$new(total = length(simulation_results), format = "[:bar] :percent :eta")

# Perform Wilcoxon test for each covariate
wilcoxon_results <- lapply(seq_along(simulation_results), function(i) {
  covariate_name <- names(simulation_results)[i]
  simulation_result <- simulation_results[[i]]
  background_samples <- random_samples[[i]]
  
  # Update progress bar
  pb$tick()
  
  # Perform Wilcoxon test for each background sample
  wilcoxon_test_results <- lapply(background_samples, function(background_sample) {
    result <- wilcox.test(simulation_result$sites_densities$Covariate, background_sample, alternative = "greater", exact = FALSE)
    tidy(result) %>% select(statistic, p.value)
  })
  
  # Combine the results
  wilcoxon_test_results <- bind_rows(wilcoxon_test_results)
  
  # Get the median test statistic and 95% confidence interval
  wilcoxon_summary <- foreach::foreach(prob = c(0.025, 0.5, 0.975), .combine = rbind) %do% {
    wilcoxon_test_results %>%
      dplyr::summarise_all(quantile, probs = prob)
  } %>%
    t() %>%
    magrittr::set_colnames(c("Lower CI", "Median", "Upper CI"))
  
  # Create a data frame for the covariate and add covariate_name
  covariate_summary <- data.frame(covariate = covariate_name)
  wilcoxon_summary <- cbind(covariate_summary, wilcoxon_summary)
  
  return(wilcoxon_summary)
})

# Combine the results for all covariates
wilcoxon_results <- do.call(rbind, wilcoxon_results)

# Write output table as a CSV
write.csv(wilcoxon_results, "csv/Wilcoxon_results.csv")
