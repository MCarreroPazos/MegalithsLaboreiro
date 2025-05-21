# Load libraries
library(terra)
library(viridis)
library(sf)

# Load rasters
rast <- rast(paste0("/Users/miguelpazoscarrero/Desktop/Laboreiro/grid/", 
                    "cum_horizon.tif"))

# Load shapefiles
sites_lab <- st_read("/Users/miguelpazoscarrero/Desktop/Laboreiro/shp/sites_lab.shp")
sites_all <- st_read("/Users/miguelpazoscarrero/Desktop/Laboreiro/shp/sites_all.shp")
intervisibility <- st_read("/Users/miguelpazoscarrero/Desktop/Laboreiro/shp/intervisibility_sites.shp")

# Generate a terrain-like color palette with transparency
create_transparent_palette <- function(colors, alpha_values) {
  rgb_values <- col2rgb(colors) / 255
  apply(rgb_values, 2, function(col) {
    rgb(col[1], col[2], col[3], alpha = alpha_values)
  })
}
n_colors <- 100
terrain_colors <- terrain.colors(n_colors)
alpha_values <- seq(0.1, 1, length.out = n_colors)
transparent_palette <- create_transparent_palette(terrain_colors, 
                                                  alpha_values)
               
# Plot figure 5                                                  
par(mfrow = c(1, 2))
plot(rast, main = "A", 
     col = transparent_palette)
plot(intervisibility$geometry, add = TRUE, lwd = 0.3)
plot(sites_lab$geometry, add = TRUE, pch = 21, cex = 0.8)
plot(rast, main = "B", 
     col = transparent_palette)
plot(sites_all$geometry, add = TRUE, pch = 21, cex = 0.8)

# Save the figure as a PDF file (modify the file path to yours)
dev.print(pdf, file = "~/Desktop/Laboreiro/Figures/Figure 5.pdf")
