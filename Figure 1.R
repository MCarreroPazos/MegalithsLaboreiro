# Libraries
library(sf)
library(rnaturalearth)
library(terra)
library(prettymapr)

# Set working directory and paths
setwd("~/Desktop/Laboreiro/")
shp_path <- file.path("shp")
grid_path <- file.path("grid")

# Load spatial data
sites <- sf::st_read(file.path(shp_path, "sites_lab.shp")) %>% st_transform(4326)
study_area <- sf::st_read(file.path(shp_path, "mask.shp")) %>% st_transform(4326)

# Load DEM
dem <- terra::rast(file.path(grid_path, "dtm.tif"))
dem <- terra::project(dem, "EPSG:4326")
# Load Iberia basemap
iberia <- ne_countries(country = c("spain", "portugal"), returnclass = "sf", scale = 10)
iberia <- st_cast(st_geometry(iberia), "POLYGON")
iberia <- iberia[st_coordinates(st_centroid(iberia))[,"X"] > -10, ]
iberia <- st_union(iberia[st_area(iberia) > units::set_units(50000000, "m^2")])
iberia <- st_transform(iberia, 4326)
franceandorra <- st_cast(st_geometry(ne_countries(country = "france", returnclass = "sf", scale = 10)), "POLYGON")
franceandorra <- franceandorra[st_area(franceandorra) == units::set_units(max(st_area(franceandorra)), "m^2")]
franceandorra <- st_union(franceandorra, st_geometry(ne_countries(country = "andorra", returnclass = "sf", scale = 10)))
franceandorra <- st_transform(franceandorra, 4326)
northafrica <- ne_countries(country = c("morocco", "algeria"), returnclass = "sf", scale = 10)
northafrica <- st_cast(st_geometry(northafrica), "POLYGON")
northafrica <- st_union(northafrica[st_area(northafrica) > units::set_units(50000000, "m^2")])
northafrica <- st_transform(northafrica, 4326)
andorra <- st_cast(st_geometry(ne_countries(country = "andorra", returnclass = "sf", scale = 10)), "POLYGON")
andorra <- st_transform(andorra, 4326)
iberiaAndorra <- st_union(iberia, andorra)

# Panel 1 and Panel 2 in the first row, and Panel 3 in the second row spanning both columns
layout_matrix <- rbind(c(1, 2),
                       c(3, 3))
# Set the layout
layout(layout_matrix)
# Panel 1: Plot study area
plot(iberiaAndorra, border = NA, col = "grey75",
     main = "a. Study area in the Iberian Peninsula") 
plot(franceandorra, col = "grey90", border = NA, add = TRUE) 
plot(northafrica, col = "grey90", border = NA, add = TRUE) 
# Draw a solid rectangle to represent the zoom window
rect(xleft = -8.4, xright = -7.85, 
     ybottom = 41.85, ytop = 42.25,
     col = "black",
     border = "black", lty = 1)
addnortharrow(scale = 0.5)
addscalebar(pos = "bottomright")
box()

# Panel 2. Study area in the Iberian Peninsula
zoom_window <- st_sfc(st_point(c(-8.9484, 41.7)), st_point(c(-7.7481, 42.2)), crs = 4326)
window_coord_sf <- st_coordinates(zoom_window)
iberia_nw <- ne_countries(country = c("spain", "portugal"), returnclass = "sf", scale = 10)
iberia_nw <- st_cast(st_geometry(iberia_nw), "POLYGON")
iberia_nw <- iberia_nw[st_coordinates(st_centroid(iberia_nw))[,"X"] > -10, ]
iberia_nw <- st_transform(iberia_nw, 4326)

## Set up the plot layout and margins
par(mar = c(4, 4, 4, 4))
## Plot Iberia map with bounding box (window coordinates)
plot(st_geometry(iberia_nw), col = "lightgray", border = "black", 
     main = "b. Serra do Laboreiro region", xlim = c(window_coord_sf[1, "X"], window_coord_sf[2, "X"]),
     ylim = c(window_coord_sf[1, "Y"], window_coord_sf[2, "Y"]), 
     xlab = "Longitude", ylab = "Latitude", axes = FALSE)
## Add custom axis labels for longitude and latitude
axis(1, at = seq(-8.95, -7.75, by = 0.1), labels = TRUE)  # Longitude (x-axis)
axis(2, at = seq(41.7, 42.2, by = 0.1), labels = TRUE)  # Latitude (y-axis)
## Add labels for Spain and Portugal
text(x = c(-8.6, -8.59), y = c(42.13, 41.91), 
     labels = c("Spain", "Portugal"), 
     col = "black", 
     cex = 0.8, 
     font = 2)
## Draw the dashed rectangle to represent the zoom window
rect(xleft = -8.3015, xright = -7.9843, ybottom = 41.9483, ytop = 42.1125, 
     border = "black", lty = 2,
     cex=2)
## Add the vertical text for "Atlantic Ocean"
text(x = -8.99, y = 42.0, 
     labels = "Atlantic Ocean", 
     col = "black", 
     srt = 90, 
     cex = 0.8, 
     font = 2)
addnortharrow(scale = 0.5)
addscalebar(pos = "bottomright")
box() # add bounding box

# Panel 3: DEM with site points
# Plot DEM
plot(dem, main = "c. Altitude (masl)")
# Add site points
plot(sites$geometry, add = TRUE, 
     pch = 21, 
     col = "white", 
     bg = "black")
# Adjusted North Arrow Position (moved left)
# North Arrow (top-left)
arrows(x0 = -8.15, y0 = 42.085, 
       x1 = -8.15, y1 = 42.09, 
       length = 0.1, col = "black", lwd = 2)
# Add the "N" label for North just above the arrow
text(x = -8.15, y = 42.093, labels = "N", 
     cex = 0.8, font = 2)
# Small Scale Bar (just below the arrow)
# Define the start and end points for the scale bar
scale_start_x <- -8.154
scale_end_x <- -8.145
scale_y <- 42.082
# Draw the scale bar with tick marks
segments(x0 = scale_start_x, 
         y0 = scale_y, 
         x1 = scale_end_x, 
         y1 = scale_y, 
         col = "black", 
         lwd = 3)
# Add scale text below the scale bar (e.g., "1 km")
text(x = -8.1497, 
     y = 42.0789, 
     labels = "1km", 
     cex = 0.8, 
     font = 2, 
     col = "black")
# Add a legend for the site points (centered on the plot)
legend(x = -8, y = 42.03,  # Adjust x and y coordinates for the legend position
       legend = "Sites", 
       pch = 21, 
       pt.bg = "black", 
       pt.cex = 1.2,  # Adjust point size if needed
       col = "white", 
       bty = "n",     # No border around the legend
       cex = 1)     # Adjust font size of the legend

# Save figure 1 (pdf)
dev.print(pdf, file = "~/Desktop/Laboreiro/Figures/Figure 1.pdf")

