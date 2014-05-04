data-analysis-template
======================
Download needed R packages, run:

Rscript requirements.R


To reproduce the data:

We cleaned the raw data in stata to get clean1.rda, cannot be reproduced. Too large to open in R.


For the rest of the data run:

Rscript clean_data.R #Will produce clean2.rda
Rscript data2mat.R #will produce data.mat for matlab analysis


Analysis:

Rscript regression.R #produces linear regression graphs in /graphs, and corresponding data in /data/simulated
Rscript regression_bystate.R #produces us map graphs of coefficients of linear regression in /graphs, and corresponding data in /data/simulated
matlab -r matlab_plots.m #will produce graphs of percent growth in /graphs, and corresponding data in /data/simulated. This is assuming you have matlab.
