# Figure 3: Selected covariates
source(here::here("analysis", "scripts", "00_Setup.R"))

# --- Load rasters -------------------------------------------------------------
rast_A <- terra::rast(file.path(path_grid, "cum_viewshed.tif"))
rast_B <- terra::rast(file.path(path_grid, "dist_to_ridges.tif"))
rast_C <- terra::rast(file.path(path_grid, "cum_horizon.tif"))
rast_D <- terra::rast(file.path(path_grid, "dist_to_wat_edges.tif"))

# --- Custom transparent palette (for cumulative horizon) ---------------------
create_transparent_palette <- function(colors, alpha_values) {
  rgb_values <- grDevices::col2rgb(colors) / 255
  apply(rgb_values, 2, function(col) {
    grDevices::rgb(col[1], col[2], col[3], alpha = alpha_values)
  })
}

n_colors <- 100
terrain_colors <- grDevices::terrain.colors(n_colors)
alpha_values <- seq(0.1, 1, length.out = n_colors)
transparent_pal <- create_transparent_palette(terrain_colors, alpha_values)

# --- Save figure (PDF + PNG) --------------------------------------------------
pdf(file = file.path(path_figures, "Figure 3.pdf"), width = 10, height = 8)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(rast_A, main = "A - Cumulative Viewshed", col = viridis::magma(100))
plot(rast_B, main = "B - Distance to Ridges", col = grDevices::terrain.colors(100))
plot(rast_C, main = "C - Cumulative Horizon", col = transparent_pal)
plot(rast_D, main = "D - Distance to Water", col = blues9)
par(mfrow = c(1, 1))
dev.off()

png(file.path(path_images, "fig3.png"), width = 2400, height = 1800, res = 300)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(rast_A, main = "A - Cumulative Viewshed", col = viridis::magma(100))
plot(rast_B, main = "B - Distance to Ridges", col = grDevices::terrain.colors(100))
plot(rast_C, main = "C - Cumulative Horizon", col = transparent_pal)
plot(rast_D, main = "D - Distance to Water", col = blues9)
par(mfrow = c(1, 1))
dev.off()
message("Figure 3 saved to: ", path_figures, " and ", path_images)
