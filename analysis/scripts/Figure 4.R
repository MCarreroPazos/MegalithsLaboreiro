# Figure 4. Monte Carlo KDE simulations and Mann-Whitney U tests for landscape covariates
source(here::here("analysis", "scripts", "00_Setup.R"))
set.seed(20250101) # for reproducibility of Monte Carlo simulations

# --- Load data ----------------------------------------------------------------
sites <- sf::st_read(file.path(path_shp, "sites_lab.shp"), quiet = TRUE) |>
  sf::st_transform(epsg_utm)

study_area <- sf::st_read(file.path(path_shp, "mask.shp"), quiet = TRUE) |>
  sf::st_transform(epsg_utm)

# Write sites to GeoPackage (open format) in shp folder
sf::st_write(sites, file.path(path_shp, "sites.gpkg"),
  driver = "GPKG", delete_dsn = TRUE, quiet = TRUE
)

# Create spatstat analysis window
study_area_w <- spatstat.geom::as.owin(sf::st_geometry(study_area))

# Create point pattern object
sppp_sites <- spatstat.geom::ppp(
  x = sites$X,
  y = sites$Y,
  window = study_area_w
)

# Convert sites to terra SpatVector for raster extraction
sites_tv <- terra::vect(sites)

# --- Load raster covariates ---------------------------------------------------
dtm <- terra::rast(file.path(path_grid, "dtm.tif"))
slope <- terra::rast(file.path(path_grid, "slope.tif"))
cum_horizon <- terra::rast(file.path(path_grid, "cum_horizon.tif"))
cum_viewshed <- terra::rast(file.path(path_grid, "cum_viewshed.tif"))
dist_to_ridges <- terra::rast(file.path(path_grid, "dist_to_ridges.tif"))
dist_to_rocks <- terra::rast(file.path(path_grid, "dist_to_rocks.tif"))
dist_to_watedges <- terra::rast(file.path(path_grid, "dist_to_wat_edges.tif"))
tpi <- terra::rast(file.path(path_grid, "tpi.tif"))

# Part 1. Monte Carlo sub-sampled kernel density estimates
# (based on Bocinsky 2017; Kempf & Günther 2022)

# Define covariate list with labels
covariates <- list(
  dtm              = "Altitude (mASL)",
  slope            = "Slope (degrees)",
  cum_horizon      = "Cumulative horizon",
  cum_viewshed     = "Cumulative viewshed",
  dist_to_ridges   = "Distance to ridges (m)",
  dist_to_rocks    = "Distance to rocky outcrops (m)",
  dist_to_watedges = "Distance to watershed edges (m)",
  tpi              = "Topographic Prominence Index"
)

# Wrapper function for the simulation
perform_simulation <- function(covariate_obj, covariate_label) {
  sites_vs_background(
    covariate      = covariate_obj,
    sites          = sites_tv,
    nsim           = 999,
    covariate_name = covariate_label
  )
}

# Run simulations for all covariates
simulation_results <- list()
for (cov_name in names(covariates)) {
  message("Running simulation for: ", cov_name)
  simulation_results[[cov_name]] <- perform_simulation(
    covariate_obj   = get(cov_name),
    covariate_label = covariates[[cov_name]]
  )
}

# Save combined KDE plot (Figure 4)
pdf(file.path(path_figures, "Figure 4.pdf"), width = 12, height = 8)
par(mfrow = c(2, 4))
for (i in seq_along(simulation_results)) {
  simulation_results_plot(simulation_results[[i]])
}
dev.off()

# Part 2. Mann-Whitney U tests (999 Monte Carlo iterations per covariate)
# Ensure sites vector matches raster CRS
sites_tv <- terra::project(sites_tv, terra::crs(dtm))

# Helper function to run and save MWU test for one covariate
run_mwu_test <- function(sim_result, site_values, output_filename, nsim = 999,
                         alternative = "greater") {
  pb <- progress::progress_bar$new(total = nsim, format = "[:bar] :percent :eta")

  MWU <- foreach(
    n = 1:nsim,
    .combine = rbind,
    .packages = c("broom", "dplyr")
  ) %do% {
    pb$tick()
    bg_sample <- sample(sim_result$ras_samples$Covariate,
      length(site_values),
      replace = FALSE
    )
    result <- wilcox.test(site_values, bg_sample,
      alternative = alternative, exact = FALSE
    )
    broom::tidy(result) %>% select(statistic, p.value)
  }

  MWU <- dplyr::bind_rows(MWU)

  summary_MWU <- foreach::foreach(
    prob = c(0.025, 0.5, 0.975),
    .combine = rbind
  ) %do%
    {
      MWU %>% dplyr::summarise_all(quantile, probs = prob)
    } %>%
    t() %>%
    magrittr::set_colnames(c("Lower CI", "Median", "Upper CI")) %>%
    magrittr::set_rownames(c("U statistic", "p-value"))

  write.csv(
    summary_MWU,
    file.path(path_derived, output_filename)
  )
  return(summary_MWU)
}

# Extract site values for each covariate
sites$altitude <- as.numeric(terra::extract(dtm, sites_tv, ID = FALSE)[[1]])
sites$slope_val <- as.numeric(terra::extract(slope, sites_tv, ID = FALSE)[[1]])
sites$cum_horiz_val <- as.numeric(terra::extract(cum_horizon, sites_tv, ID = FALSE)[[1]])
sites$cum_view_val <- as.numeric(terra::extract(cum_viewshed, sites_tv, ID = FALSE)[[1]])
sites$ridges_val <- as.numeric(terra::extract(dist_to_ridges, sites_tv, ID = FALSE)[[1]])
sites$rocks_val <- as.numeric(terra::extract(dist_to_rocks, sites_tv, ID = FALSE)[[1]])
sites$water_val <- as.numeric(terra::extract(dist_to_watedges, sites_tv, ID = FALSE)[[1]])

# Run MWU for each covariate
message("Running Mann-Whitney tests...")
run_mwu_test(
  simulation_results$dtm,
  sites$altitude,
  "Mann_Whitney_results_DEM.csv"
)

run_mwu_test(
  simulation_results$slope,
  sites$slope_val,
  "Mann_Whitney_results_Slope.csv"
)

run_mwu_test(
  simulation_results$cum_horizon,
  sites$cum_horiz_val,
  "Mann_Whitney_results_cum_horizon.csv"
)

run_mwu_test(
  simulation_results$cum_viewshed,
  sites$cum_view_val,
  "Mann_Whitney_results_cum_viewshed.csv"
)

run_mwu_test(
  simulation_results$dist_to_ridges,
  sites$ridges_val,
  "Mann_Whitney_results_dist_to_ridges.csv",
  alternative = "less" # sites are expected closer to ridges than random
)

run_mwu_test(
  simulation_results$dist_to_rocks,
  sites$rocks_val,
  "Mann_Whitney_results_dist_to_rocks.csv",
  alternative = "less" # sites are expected closer to rocky outcrops than random
)

run_mwu_test(
  simulation_results$dist_to_watedges,
  sites$water_val,
  "Mann_Whitney_results_dist_to_water_edges.csv",
  alternative = "less" # sites are expected closer to watershed edges than random
)

# Part 3. Landscape comparison table
rast_list <- list(
  dtm              = dtm,
  slope            = slope,
  cum_horizon      = cum_horizon,
  cum_viewshed     = cum_viewshed,
  dist_to_ridges   = dist_to_ridges,
  dist_to_rocks    = dist_to_rocks,
  dist_to_watedges = dist_to_watedges,
  tpi              = tpi
)

final_comparison <- data.frame(
  Variable = names(rast_list),
  Label = c(
    "Altitude (mASL)", "Slope (degrees)", "Cumulative horizon",
    "Cumulative viewshed", "Distance to ridges (m)",
    "Distance to rocky outcrops (m)", "Distance to watershed edges (m)",
    "Topographic Prominence Index"
  ),
  Observed_Median = NA_real_,
  Landscape_Median = NA_real_,
  P_Value = NA_real_
)

for (i in seq_len(nrow(final_comparison))) {
  curr_rast <- rast_list[[i]]
  if (!inherits(curr_rast, "SpatRaster")) curr_rast <- terra::rast(curr_rast)

  s_vals <- terra::extract(curr_rast, sites_tv, ID = FALSE, method = "simple")
  s_vals <- as.numeric(na.omit(unlist(s_vals)))

  l_vals <- terra::spatSample(curr_rast, 5000,
    na.rm = TRUE, method = "random", values = TRUE
  )
  l_vals <- as.numeric(na.omit(unlist(l_vals)))

  if (length(s_vals) > 0 && length(l_vals) > 0) {
    final_comparison$Observed_Median[i] <- median(s_vals)
    final_comparison$Landscape_Median[i] <- median(l_vals)
    final_comparison$P_Value[i] <- wilcox.test(s_vals, l_vals,
      exact = FALSE
    )$p.value
  }
}

final_comparison$Significant <- ifelse(final_comparison$P_Value < 0.05,
  "YES", "no"
)

display_comparison <- final_comparison
display_comparison$P_Value <- format.pval(display_comparison$P_Value,
  digits = 3, eps = 0.001
)
print(display_comparison)

write.csv(display_comparison,
  file.path(path_derived, "Final_Landscape_Comparison.csv"),
  row.names = FALSE
)
