# Figure 6: Spatial statistical models — logistic regression map and
# inhomogeneous pair-correlation function envelopes (null, first-order,
# second-order models)

source(here::here("analysis", "scripts", "00_Setup.R"))
set.seed(20250101) # for reproducibility of Monte Carlo envelopes

# --- Load raster covariates (raster package for spatstat compatibility) -------
elevation <- raster::raster(file.path(path_grid, "dtm.tif"))
cumulative_horizon <- raster::raster(file.path(path_grid, "cum_horizon.tif"))
geomorph <- raster::raster(file.path(path_grid, "sum_structure.tif"))
slope_r <- raster::raster(file.path(path_grid, "slope.tif"))

# --- Load sites and study area ------------------------------------------------
sites <- sf::st_read(file.path(path_shp, "sites_lab.shp"), quiet = TRUE) |>
  sf::st_transform(epsg_utm)
study_area <- sf::st_read(file.path(path_shp, "mask.shp"), quiet = TRUE) |>
  sf::st_transform(epsg_utm)

area <- spatstat.geom::as.owin(study_area)
sppp <- spatstat.geom::ppp(
  x      = sf::st_coordinates(sites)[, 1],
  y      = sf::st_coordinates(sites)[, 2],
  window = area
)

# --- Convert rasters to spatstat im objects -----------------------------------
rast_to_im <- function(r) {
  m <- as.matrix(r)
  x_coord <- seq(raster::xmin(r), raster::xmax(r), length.out = ncol(m))
  y_coord <- seq(raster::ymin(r), raster::ymax(r), length.out = nrow(m))
  spatstat.geom::as.im(X = m, xcol = x_coord, yrow = y_coord)
}

elev_im <- rast_to_im(elevation)
horizon_im <- rast_to_im(cumulative_horizon)
geomorph_im <- rast_to_im(geomorph)
slope_im <- rast_to_im(slope_r)

# --- Fit point process models -------------------------------------------------
covlist <- list(
  elev_im = elev_im, horizon_im = horizon_im,
  geomorph_im = geomorph_im, slope_im = slope_im
)
fotrend <- ~ elev_im + horizon_im + geomorph_im + slope_im

# Logistic (first-order) model with stepwise selection
mod1 <- step(
  spatstat.model::ppm(sppp,
    trend = fotrend, interaction = NULL,
    covariates = covlist, method = "logi"
  ),
  verbose = FALSE
)

# Predicted intensity surface from the stepwise model (uses all retained covariates)
logodds <- predict(mod1)

numSims <- 999

# A. Null model (complete spatial randomness)
mod0 <- spatstat.model::ppm(sppp, ~1)
Pcfinhom_mod0 <- spatstat.explore::envelope(mod0,
  fun = spatstat.explore::pcfinhom,
  correction = "best", nsim = numSims,
  verbose = FALSE
)

# B. First-order model
Pcfinhom_mod1 <- spatstat.explore::envelope(mod1,
  fun = spatstat.explore::pcfinhom,
  correction = "best", nsim = numSims,
  verbose = FALSE
)

# C. Second-order model (area interaction)
mod2 <- step(
  spatstat.model::ppm(sppp,
    trend = fotrend,
    interaction = spatstat.model::AreaInter(150),
    covariates = covlist, method = "logi"
  ),
  trace = 0
)
Pcfinhom_mod2 <- spatstat.explore::envelope(mod2,
  fun = spatstat.explore::pcfinhom,
  correction = "best", nsim = numSims,
  verbose = FALSE
)

# --- Helper function to draw Figure 6 -----------------------------------------
draw_fig6 <- function() {
  par(mfrow = c(2, 2))
  plot(logodds, main = "Logistic Regression Map")
  plot(sites$geometry, add = TRUE, pch = 21, cex = 1)
  
  legend_args <- list(
    legend = c("Megalithic sites", "CSR", "999 random simulations"),
    col    = c("black", "red", "grey"),
    lty    = c(1, 2, 1),
    lwd    = c(1, 1, 3),
    cex    = 0.8,
    bty    = "n"
  )
  
  plot(Pcfinhom_mod0,
    xlim = c(0, 2000), ylim = c(0, 20),
    legend = FALSE, main = "a. Random model", xlab = "Distance in metres"
  )
  do.call(legend, c(list("topright"), legend_args))
  
  plot(Pcfinhom_mod1,
    xlim = c(0, 2000), ylim = c(0, 20),
    legend = FALSE, main = "b. First-order model", xlab = "Distance in metres"
  )
  do.call(legend, c(list("topright"), legend_args))
  
  plot(Pcfinhom_mod2,
    xlim = c(0, 2000), ylim = c(0, 20),
    legend = FALSE, main = "c. Second-order model\n(AreaInter = 150)",
    xlab = "Distance in metres"
  )
  do.call(legend, c(list("topright"), legend_args))
  par(mfrow = c(1, 1))
}

# --- Save figure (PDF + PNG) ---------------------------------------------------
pdf(file.path(path_figures, "Figure 6.pdf"), width = 12, height = 10)
draw_fig6()
dev.off()

png(file.path(path_images, "fig6.png"), width = 3000, height = 2400, res = 300)
draw_fig6()
dev.off()

message("Figure 6 saved to: ", path_figures, " and ", path_images)
