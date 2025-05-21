# Load required packages
spatpack <- c("sf", "terra", 
              "spatstat", "plotly", 
              "foreach", "dplyr", 
              "progress", "purrr", 
              "reshape2", "broom")
lapply(spatpack, require, character.only = TRUE)

# Define working directories
grid <- file.path("grid")
shp <- file.path("shp")

# Define the coordinate reference system (CRS)
crs <- "+proj=utm +zone=29 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# Read sites and study area using sf
sites <- sf::st_read("shp/sites_lab.shp")

# Read covariates using terra
dem <- terra::rast("grid/dtm.tif")
slope <- terra::rast("grid/slope.tif")
cum_horizon <- terra::rast("grid/cum_horizon.tif")
cum_viewshed <- terra::rast("grid/cum_viewshed.tif")
dist_to_ridges <- terra::rast("grid/dist_to_ridges.tif")
dist_to_rocks <- terra::rast("grid/dist_to_rocks.tif")
dist_to_watedges <- terra::rast("grid/dist_to_wat_edges.tif")
tpi <- terra::rast("grid/tpi.tif")

# Part 1: Calculate and plot kernel density estimates with Monte Carlo subsampling
# Compare the sites with the covariates
source('r_functions/rbias_sitesVSbackground.R')
source('r_functions/rbias_simulationresults.R')

# Create a function to iterate the simulation process
perform_simulation <- function(covariate, covariate_name) {
  simulation_result <- sites_vs_background(covariate = covariate,
                                           sites = sites,
                                           nsim = 999,
                                           covariate_name = covariate_name)
  return(simulation_result)
}

# Define a list of covariates and their corresponding names
covariates <- list(
  dem = "Altitude mASL", 
  slope = "Slope %",
  cum_viewshed = "Cumulative Viewshed",
  dist_to_ridges = "Distance to Ridges",
  dist_to_watedges = "Distance to Water Edges",
  tpi = "Topographic Position Index (TPI)"
)

# Apply the simulation function to each covariate
simulation_results <- lapply(names(covariates), 
                             function(covariate) {
  perform_simulation(get(covariate), 
                     covariates[[covariate]])
})

# Set up a new PDF device with a nice size
pdf("figures/Figure 4.pdf", width = 12, height = 7)
# Adjust for layout
par(mfrow = c(2, 3), mar = c(5, 5, 2, 3))
for (i in seq_along(simulation_results)) {
  simulation_result <- simulation_results[[i]]
  if (!is.null(simulation_result)) {
    # Create the plot using simulation_results_plot()
    simulation_results_plot(simulation_result, show_title = FALSE)
  }
}
# Close the PDF device
dev.off()

# Part 2 Compute two-sample Wilcoxon tests (Mann-Whitney U tests)
# Generate random background samples for each covariate
random_samples <- lapply(simulation_results, function(simulation_result) {
  if (is.null(simulation_result)) return(NULL)
  ras_samples <- simulation_result$ras_samples
  lapply(ras_samples, function(ras_sample) {
    sample(ras_sample, 
           length(simulation_result$sites_densities$Covariate), 
           replace = TRUE)
  })
})

# Create a progress bar
pb <- progress_bar$new(total = length(simulation_results), 
                       format = "[:bar] :percent :eta")

# Perform Wilcoxon test for each covariate
wilcoxon_results <- lapply(seq_along(simulation_results), function(i) {
  simulation_result <- simulation_results[[i]]
  background_samples <- random_samples[[i]]
  
  if (is.null(simulation_result) || is.null(background_samples)) {
    return(NULL)
  }
  
  # Update progress bar
  pb$tick()
  
  # Perform Wilcoxon test for each background sample
  wilcoxon_test_results <- lapply(background_samples, function(background_sample) {
    if (length(background_sample) < 2 || 
        length(simulation_result$sites_densities$Covariate) < 2) {
      return(data.frame(statistic = NA, p.value = NA))
    }
    result <- wilcox.test(simulation_result$sites_densities$Covariate, 
                          background_sample, 
                          alternative = "greater", 
                          exact = FALSE)
    broom::tidy(result) %>% select(statistic, p.value)
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
  covariate_summary <- data.frame(covariate = names(covariates)[i])
  wilcoxon_summary <- cbind(covariate_summary, wilcoxon_summary)
  
  return(wilcoxon_summary)
})

# Combine the results for all covariates
wilcoxon_results <- do.call(rbind, wilcoxon_results)

# Write output table as a CSV
write.csv(wilcoxon_results, "csv/Wilcoxon_results.csv")