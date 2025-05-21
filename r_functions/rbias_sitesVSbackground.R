sites_vs_background <- function(covariate, sites, nsim = 999, covariate_name = names(covariate)) {
  # Extract values at site locations and ensure it's numeric
  cov_char <- as.numeric(na.omit(terra::extract(covariate, sites)[,2])) # Ensure numeric conversion
  covariate_values <- as.numeric(stats::na.omit(terra::values(covariate))) # Ensure numeric vector
  
  # Define range for density estimation
  min_ras <- min(covariate_values)
  max_ras <- max(covariate_values)
  
  # Calculate density for site-specific covariate values
  sites_densities <- stats::density(cov_char, from = min_ras, to = max_ras, n = 1101)
  sites_densities <- data.table::data.table(
    Covariate = sites_densities$x,
    Frequency = sites_densities$y * length(sites_densities$y)
  )
  
  # Simulations for background samples
  ras_samples <- lapply(X = 1:nsim, FUN = function(sim) {
    ras_sample <- sample(x = covariate_values, size = nrow(sites), replace = FALSE)
    ras_dens <- stats::density(ras_sample, from = min_ras, to = max_ras, n = 1101)
    data.table::data.table(
      Covariate = ras_dens$x,
      Frequency = ras_dens$y * length(ras_dens$y)
    )
  })
  
  # Combine simulations into a data.table and calculate confidence intervals
  ras_samples <- data.table::rbindlist(ras_samples)
  ras_samples <- ras_samples[, .(
    lower_ci = stats::quantile(Frequency, probs = 0.025),
    median = stats::quantile(Frequency, probs = 0.5),
    upper_ci = stats::quantile(Frequency, probs = 0.975)
  ), by = Covariate]
  
  return(list(nsim = nsim, ras_samples = ras_samples, sites_densities = sites_densities, covariate_name = covariate_name))
}
