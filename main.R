source("compute.R")

library(scatterplot3d)

compute_data_std <- function() {

    data_all <- data.frame()

    # For O_M = 0 to 1 separated by 0.01 increments
    for(omega in seq(0.0, 1 + 1e-10, by=0.05)) {

        # Compute high density for low z
        data1 <- compute_data(0, 15, 15*1000, omega)

        # Only compute 1000 for the remaining z up to 1100
        data2 <- compute_data(15, 1100, 1000+1, omega)

        # Remove the first entry which is the same as the last in data1
        data2 <- data2[-1, ]

        # Combine the datas and write them to file
        combined_data <- rbind(data1, data2)

        # Add the omega column to the combined_data
        combined_data$omegaM <- omega

        data_all <- rbind(data_all, combined_data)
    }

    write.csv(data_all, "output.csv", row.names=FALSE)

    color_gradient <- colorRampPalette(c("blue", "red"))
    num_colors <- 100
    scaled_colors <- cut(data_all$y, breaks = num_colors, labels = FALSE)

    png(filename="static_plot.png", width=800, height=600)
    scatterplot3d(data_all$omegaM, data_all$z, data_all$y, 
                xlab="omegaM", ylab="z", zlab="y", 
                pch=19, color=color_gradient(num_colors)[scaled_colors])

}

# Compute data between zmin and zmax with samples number of data points using OmegaM = om
compute_data_rnd <- function(n = 25000) {
  
  # Half of the samples for each stratified region
  n_dens = 10000
  n_lowdens = 5000
  
  # Sampling for z = [0, 15] and omegaM = [0, 1]
  omegaM_samples_important1 <- runif(n_dens/2, 0, 1)
  z_samples_important1 <- runif(n_dens/2, 0, 15)
  
  # Sampling for z = [0, 1100] and omegaM = [0, 0.1]
  omegaM_samples_important2 <- runif(n_dens/2, 0, 0.1)
  z_samples_important2 <- runif(n_dens/2, 0, 1100)
  
  # Sampling for z in [0, 15] across all omegaM
  z_samples_other <- runif(n_lowdens, 15, 1100)
  omegaM_samples_other <- runif(n_lowdens, 0.1, 1)
  
  # Combine the important samples
  omegaM_samples <- c(omegaM_samples_important1, omegaM_samples_important2, omegaM_samples_other)
  z_samples <- c(z_samples_important1, z_samples_important2, z_samples_other)
  
  # Create an empty data frame to store results
  data_all <- data.frame(omegaM = numeric(), z = numeric(), y = numeric())
  
  total_samples <- n_dens + n_lowdens

  # Loop through each z_sample and omegaM_sample
  for(i in 1:total_samples){ # Adjusted loop length
    res <- cosdist(
      z = z_samples[i],
      H0 = 70, 
      OmegaM = omegaM_samples[i], 
      OmegaL = 1 - omegaM_samples[i], 
      OmegaR = 0, 
      w0 = -1,
      wprime = 0
    )
    
    res = res[, c("z","CoDistTran")] # Only take z and CoDistTran cols
    res$y <- res$CoDistTran * 70 / 299792.458 # Build Y
    
    # Combine the results
    current_data <- data.frame(y = res$y, z = z_samples[i], omegaM = omegaM_samples[i])
    data_all <- rbind(data_all, current_data)
    
  }
  
  # Write to CSV
  write.csv(data_all, "output.csv", row.names=FALSE)
  
  # Plotting
  color_gradient <- colorRampPalette(c("blue", "red"))
  num_colors <- 100
  scaled_colors <- cut(data_all$y, breaks = num_colors, labels = FALSE)

  png(filename="static_plot.png", width=800, height=600)
  scatterplot3d(data_all$omegaM, data_all$z, data_all$y, 
                xlab="omegaM", ylab="z", zlab="y", 
                pch=19, color=color_gradient(num_colors)[scaled_colors], 
                zlim=c(min(data_all$y), 50)) # Setting maximum y value to 50
  dev.off()
    
  return(data_all)
}

compute_data_std()
