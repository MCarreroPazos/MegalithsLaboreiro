simulation_results_plot <- function(simulation_results, show_title = FALSE) {
  nsim <- as.numeric(simulation_results$nsim)
  ras_samples <- data.frame(simulation_results$ras_samples)
  sites_densities <- data.frame(simulation_results$sites_densities)
  covariate_name <- simulation_results$covariate_name
  
  plot(x = ras_samples$Covariate, y = ras_samples$median, xlab = covariate_name, 
       ylab = "Relative Frequency [%]", ylim = c(min(sites_densities$Frequency, 
                                                     min(ras_samples$lower_ci)), max(sites_densities$Frequency, 
                                                                                     max(ras_samples$upper_ci))), type = "l", lty = 2, cex.lab = 1.7)
  
  graphics::polygon(c(ras_samples$Covariate, rev(ras_samples$Covariate)), 
                    c(ras_samples$lower_ci, rev(ras_samples$upper_ci)), col = "grey75", 
                    border = FALSE)
  graphics::lines(x = ras_samples$Covariate, y = ras_samples$median, 
                  type = "l", lty = 2, lwd = 2)
  graphics::lines(ras_samples$Covariate, ras_samples$upper_ci, 
                  col = "grey", lty = 2)
  graphics::lines(ras_samples$Covariate, ras_samples$lower_ci, 
                  col = "grey", lty = 2)
  graphics::lines(x = sites_densities$Covariate, y = sites_densities$Frequency, 
                  col = "black", lty = 1, lwd = 2)
  
  # Add title only if show_title is TRUE
  if (show_title) {
    graphics::mtext(side = 3, line = 2.5, at = -10, adj = 0, 
                    font = 2, cex = 1, covariate_name)
  }
}
