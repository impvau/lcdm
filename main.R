
library(celestial)

# Generate z values across a range, 1000 samples per z
zmax = 11
z <- seq(0, zmax, length.out = zmax*1000)

res <- cosdist(
    z=z,
    H0=70, 
    OmegaM=0.3, 
    OmegaL=1-OmegaM-OmegaR, 
    OmegaR=0, 
    w0 = -1,
    wprime = 0, 
    ref = '737'
)

res = res[, c("z","CoDistTran")]                # Only take z and CoDistTran cols
res$y <- res$CoDistTran*70/299792.458           # Build Y
res$znow <- 2*(1-1/sqrt(res$z+1))               # P's expr
res$zrel <- ((res$z+1)^2-1)/((res$z+1)^2+1)     # P's expr 2
res$mine <- res$z*exp(-res$z/4)                 # P's model

# Output 
write.csv(res, "output.csv", row.names=FALSE)

# Test the function on output from https://cosmocalc.icrar.org/
# To ensure that we are computing it the same
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
res <- cosdist(
    z=ztest,
    H0=70, 
    OmegaM=0.3, 
    OmegaL=1-OmegaM-OmegaR, 
    OmegaR=0, 
    w0 = -1,
    wprime = 0, 
    ref = '737'
)
print(res, digits=15)

print('end')
