# Load required libraries
library(terra)
library(viridis)

# Define the directory path (modify according to your directory)
dir_path <- "/Users/miguelpazoscarrero/Desktop/Laboreiro/grid/"

# Load rasters
rast_A <- rast(paste0(dir_path, "cum_viewshed.tif"))
rast_B <- rast(paste0(dir_path, "dist_to_ridges.tif"))
rast_C <- rast(paste0(dir_path, "cum_horizon.tif"))
rast_D <- rast(paste0(dir_path, "dist_to_wat_edges.tif"))

# Create a custom palette for plot C (Cumulative horizon)
create_transparent_palette <- function(colors, alpha_values) {
  rgb_values <- col2rgb(colors) / 255
  apply(rgb_values, 2, function(col) {
    rgb(col[1], col[2], col[3], alpha = alpha_values)
  })
}

# Generate a terrain-like color palette with transparency
n_colors <- 100
terrain_colors <- terrain.colors(n_colors)
alpha_values <- seq(0.1, 1, length.out = n_colors)
transparent_palette <- create_transparent_palette(terrain_colors, 
                                                  alpha_values)
# 2x2 plotting layout
par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(rast_A, main = "A - Cumulative Viewshed", 
     col = magma(100))
plot(rast_B, main = "B - Distance to Ridges", 
     col = terrain.colors(100))
plot(rast_C, main = "C - Cumulative Horizon", 
     col = transparent_palette)
plot(rast_D, main = "D - Distance to Water", 
     col = blues9)

# Reset plot layout
par(mfrow = c(1, 1))
# Save the figure as a PDF file (modify the file path to yours)
dev.print(pdf, file = "~/Desktop/Laboreiro/Figures/Figure 3.pdf")