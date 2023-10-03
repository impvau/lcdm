
source("compute.R")

# Test funcs if you want to run
#test1()
#test2()

data_all <- data.frame()

# For O_M = 0.2 to 0.4 separated by 0.025 increments
for(omega in seq(0.2, 0.4 + 1e-10, by=0.025)) {

    # Compute high density for low z
    data1 <- compute_data(0, 15, 15*1000, omega)

    # Only compute 1000 for the remaining z up to 1100
    data2 <- compute_data(15, 1100, 1000+1, omega)

    # Remove the first entry which is the same as the last in data1
    data2 <- data2[-1, ]

    # Combine the datas and write them to file
    combined_data <- rbind(data1, data2)
    
    # Write the combined_data to a file
    filename <- paste0("output.", omega, ".csv")
    write.csv(combined_data, filename, row.names=FALSE)

    # Add the omega column to the combined_data
    combined_data$omega <- omega

    # Create output with all the data within
    data_all <- rbind(data_all, combined_data)
  
}

# Write concatenated output to combined file
write.csv(data_all, "output.all.csv", row.names=FALSE)
