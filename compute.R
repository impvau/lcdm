
library(celestial)
# See docs on library at https://cran.r-project.org/web/packages/celestial/celestial.pdf
# Not great, but they provide some underlying references

# Compute data between zmin and zmax with samples number of data points using OmegaM = om
compute_data <- function(zmin = 0, zmax = 15, samples, om = 0.3) {

    # Generate z values to evaluate
    z <- seq(zmin, zmax, length.out = samples)

    # Note that we can't specify the reference set if we want OmegaM to work 
    # else it is overwritten with 0.3
    res <- cosdist(
        z=z,
        H0=70, 
        OmegaM=om, 
        OmegaL=1-om, 
        OmegaR=0, 
        w0 = -1,
        wprime = 0
    )

    # Articulate the fields in the datafile
    res = res[, c("z","CoDistTran")]                # Only take z and CoDistTran cols
    res$y <- res$CoDistTran*70/299792.458           # Build Y
    res$znow <- 2*(1-1/sqrt(res$z+1))               # P's expr
    res$zrel <- ((res$z+1)^2-1)/((res$z+1)^2+1)     # P's expr 2
    res$mine <- res$z*exp(-res$z/4)                 # P's model

    # Reorder the columns to have 'y' as the first column
    res <- res[, c("y", setdiff(names(res), "y"))]

    return(res)
}

# The samples from the LCDM website have different values for the same number of points we generate
# We want to ensure we compute the exact same values as the website to ensure we are using the function
# correctly, given how painful the docs are.
test <- function() {

    print('Testing using the variables output from https://cosmocalc.icrar.org/')

    # Test the function on output from https://cosmocalc.icrar.org/
    # We use the exact z values from the low z-values of the web output
    ztest <- c(
        0.000694081784944,
        0.001388645319412,
        0.002083690937777,
        0.002779218974647,
        0.003475229764857,
        0.004171723643479,
        0.004868700945815,
        0.005566162007402,
        0.006264107164007,
        0.006962536751632,
        0.007661451106513,
        0.008360850565116,
        0.009060735464143,
        0.009761106140531,
        0.010461962931448,
        0.011163306174297,
        0.011865136206716,
        0.012567453366577,
        0.013270257991985,
        0.013973550421283,
        0.014677330993045,
        0.015381600046083
    )

    # We evaluate the cosdist and print the values. 
    # From this we can compare the CoDistTrans value for exact values in the web-computed csv
    res <- cosdist(
        z=ztest,
        H0=70, 
        OmegaM=0.3, 
        OmegaL=1-OmegaM-OmegaR, 
        OmegaR=0, 
        w0 = -1,
        wprime = 0
    )
    print(res, digits=15)
    print('------------------')
    print('end')
}

# In test2 we compute the values for O_M = 0.2 and O_M = 0.4 to ensure the data is different
# This was neccessary in the uncovering that when Ref = '737' is provided (or any other) then 
# the computation will replace this with 0.3 and ignore our O_M
test2 <- function() {

    ztest <- c(
        0,
        0.007026054,
        0.014101474,
        0.021226606,
        0.0284018,
        0.035627407,
        0.042903781,
        0.05023128,
        0.057610262,
        0.065041089,
        0.072524126,
        0.080059739,
        0.087648297,
        0.095290173,
        0.102985742,
        0.11073538,
        0.118539467,
        0.126398386,
        0.134312522,
        0.142282264,
        0.150308001,
        0.158390128
    )

    res1 <- cosdist_new(
        z=ztest,
        H0=70, 
        OmegaM=0.2, 
        OmegaL=0.8, 
        OmegaR=0, 
        w0 = -1,
        wprime = 0
    )
    res1 = res1[, c("z","CoDistTran")]                # Only take z and CoDistTran cols

    res2 <- cosdist_new(
        z=ztest,
        H0=70, 
        OmegaM=0.4, 
        OmegaL=0.6, 
        OmegaR=0, 
        w0 = -1,
        wprime = 0
    )
    res2 = res2[, c("z","CoDistTran")]                # Only take z and CoDistTran cols

    # Here we see side-by-side that different CoDistTran values are computed for the different O_M values
    result <- cbind(res1, res2)
    print(result)

}