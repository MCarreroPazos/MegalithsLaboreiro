# Load R Packages
spatpack<-c("raster","spatstat", "sp")
lapply(spatpack, require, character.only=TRUE)

# Load raster covariates
elevation <- raster("grid/dtm.tif")
cumulative_horizon <- raster("grid/cum_horizon.tif")
geomorph <- raster("grid/sum_structure.tif")
slope <- raster("grid/slope.tif")
cumulative_viewshed <- raster("grid/cum_viewshed.tif")

# Load sites
sites <- sf::st_read("shp/sites_lab.shp")

# Create spatial point pattern process
study_area <- sf::st_read("shp/mask.shp")
area <- as.owin(study_area)
sppp <- ppp(x=sites$X, y=sites$Y, window=area)

# Convert raster to image
# Elevation
## Get the matrix of values from the raster
elev_matrix <- as.matrix(elevation)
## Get the coordinates for the raster, ensuring they match the matrix dimensions
x_coords_elev <- seq(xmin(elevation), xmax(elevation), length.out = ncol(elev_matrix))
y_coords_elev <- seq(ymin(elevation), ymax(elevation), length.out = nrow(elev_matrix))
## Create the im object using the matrix and coordinates
elev_im <- spatstat.geom::as.im(X = elev_matrix, xcol = x_coords_elev, yrow = y_coords_elev)
# Cumulative Horizon
## Get the matrix of values from the raster
horizon_matrix <- as.matrix(cumulative_horizon)
## Get the coordinates for the raster, ensuring they match the matrix dimensions
x_coords_hrz <- seq(xmin(cumulative_horizon), xmax(cumulative_horizon), length.out = ncol(horizon_matrix))
y_coords_hrz <- seq(ymin(cumulative_horizon), ymax(cumulative_horizon), length.out = nrow(horizon_matrix))
## Create the im object using the matrix and coordinates
horizon_im <- spatstat.geom::as.im(X = horizon_matrix, xcol = x_coords_hrz, yrow = y_coords_hrz)
# Geomorphons (sum)
## Get the matrix of values from the raster
geomorph_matrix <- as.matrix(geomorph)
## Get the coordinates for the raster, ensuring they match the matrix dimensions
x_coords_gmph <- seq(xmin(geomorph), xmax(geomorph), length.out = ncol(geomorph_matrix))
y_coords_gmph <- seq(ymin(geomorph), ymax(geomorph), length.out = nrow(geomorph_matrix))
## Create the im object using the matrix and coordinates
geomorph_im <- spatstat.geom::as.im(X = geomorph_matrix, xcol = x_coords_gmph, yrow = y_coords_gmph)
# Slope
## Get the matrix of values from the raster
slope_matrix <- as.matrix(slope)
## Get the coordinates for the raster, ensuring they match the matrix dimensions
x_coords_slp <- seq(xmin(slope), xmax(slope), length.out = ncol(slope_matrix))
y_coords_slp <- seq(ymin(slope), ymax(slope), length.out = nrow(slope_matrix))
## Create the im object using the matrix and coordinates
slope_im <- spatstat.geom::as.im(X = slope_matrix, xcol = x_coords_slp, yrow = y_coords_slp)
# Cumulative viewshed
## Get the matrix of values from the raster
cum_view_matrix <- as.matrix(cumulative_viewshed)
## Get the coordinates for the raster, ensuring they match the matrix dimensions
x_coords_cview <- seq(xmin(cumulative_viewshed), xmax(cumulative_viewshed), length.out = ncol(cum_view_matrix))
y_coords_cview <- seq(ymin(cumulative_viewshed), ymax(cumulative_viewshed), length.out = nrow(cum_view_matrix))
## Create the im object using the matrix and coordinates
cumview_im <- spatstat.geom::as.im(X = cum_view_matrix, xcol = x_coords_cview, yrow = y_coords_cview)

# Fit Point Process Model to Data
covlist <- list(elev_im, horizon_im, geomorph_im, slope_im)
names(covlist) <- c("elev_im", "horizon_im", "geomorph_im", "slope_im")
fotrend <- ~ elev_im + horizon_im + geomorph_im + slope_im
mod1 <- step(ppm(sppp, trend=fotrend, interaction=NULL, covariates=covlist, method="logi"))
summary(mod1)

# Create a logistic regression surface
logodds <- -22.3439702866+(elev_im*6.897505e-03)
plot(logodds, main="Logistic Regression Map")

# Point process models
numSims <- 999 # Define the number of Monte Carlo Simulations to run

# A. Null model
mod0 <- ppm(sppp, ~1)
Pcfinhom_mod0 <- envelope(mod0, 
                          fun=pcfinhom, 
                          correction="best", 
                          nsim=numSims)
# B. First order interaction (using the covariates)
Pcfinhom_mod1 <- envelope(mod1, 
                          fun=pcfinhom, 
                          correction="best", 
                          nsim=numSims)
# C. Second order interaction (using the covariates and creating an area of interaction between points)
mod2 <- step(ppm(sppp, trend=fotrend, 
                 interaction=AreaInter(150), 
                 covariates=covlist, 
                 method="logi"))
Pcfinhom_mod2 <- envelope(mod2, 
                          fun=pcfinhom, 
                          correction="best", 
                          nsim=numSims)

# Plot figure
# Set up a 2x2 plotting layout
par(mfrow = c(2, 2))
# Plot 1: Logistic Regression Map
plot(logodds, 
     main = "Logistic Regression Map")
plot(sites$geometry, add=T, pch=21, cex= 1)
# Plot 2: Random model
plot(Pcfinhom_mod0, 
     xlim = c(0, 2000), 
     ylim = c(0, 20), 
     legend = FALSE, 
     main = "a. Random model", 
     xlab="Distance in metres")
legend("topright", 
       legend = c("Megalithic sites", "CSR", "999 random simulations"),
       col = c("black", "red", "grey"),
       lty = c(1, 2, 1), 
       lwd = c(1, 1, 3), 
       cex = 0.8, 
       bty = "n")
# Plot 3: First-order model
plot(Pcfinhom_mod1, 
     xlim = c(0, 2000), 
     ylim = c(0, 20), 
     legend = FALSE, 
     main = "b. First-order model", 
     xlab="Distance in metres")
legend("topright", 
       legend = c("Megalithic sites", "CSR", "999 random simulations"),
       col = c("black", "red", "grey"),
       lty = c(1, 2, 1), 
       lwd = c(1, 1, 3), 
       cex = 0.8, 
       bty = "n")
# Plot 4: Second-order model
plot(Pcfinhom_mod2, 
     xlim = c(0, 2000), 
     ylim = c(0, 20), 
     legend = FALSE, 
     main = "c. Second-order model\n(AreaInter = 150)", 
     xlab="Distance in metres")
legend("topright", 
       legend = c("Megalithic sites", "CSR", "999 random simulations"),
       col = c("black", "red", "grey"),
       lty = c(1, 2, 1), 
       lwd = c(1, 1, 3), 
       cex = 0.8, 
       bty = "n")
# Reset plotting parameters to default
par(mfrow = c(1, 1))

# Save figure 6 (pdf)
dev.print(pdf, file = "figures/Figure 6.pdf")
