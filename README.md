
# Overview

Code for generating data from https://cosmocalc.icrar.org/ but via the R package.

The exact same data is produced, however the values of z do not perfectly align with the sequence generated behind the scenes on the web page.

Using VS Code, rebuild the dev container and then run

```
Rscript main.R
```

This generates output.0.2.csv, output.0.225.csv, ..., output.0.4.csv i.e. the model for different OmegaM
Between z= 0 and 15, 1000 samples are generated per +1 z.
Between z = 15 and z = 1100, 1000 samples are generated in total

All data is concatenated in output.all.csv with an additional Omega field for each row item
