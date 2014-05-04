install.packages('R.matlab',  repos='http://cran.us.r-project.org')
require(R.matlab)

dataclean = readRDS(paste(getwd(),'/../','data/cleaned/clean2.rda',sep=''))

writeMat('../data/cleaned/data.mat', x=as.matrix(dataclean))
