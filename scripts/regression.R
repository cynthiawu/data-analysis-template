dataclean = readRDS(paste(getwd(),'/../','data/cleaned/clean2.rda',sep=''))

FinalData <- aggregate(cbind(dataclean[,4]) ~ dataclean[,1] + dataclean[,2] + dataclean[,3], FUN = sum)

colnames(FinalData) <- c("year", "place", "industry", "jobs")

Hightech1980 <- subset(FinalData, year == 1980 & industry == "Hightech")
Manufacturing1980 <- subset(FinalData, year == 1980 & industry == "Manufacturing")
Retail1980 <- subset(FinalData, year == 1980 & industry == "Retail")

Hightech1990 <- subset(FinalData, year == 1990 & industry == "Hightech")
Manufacturing1990 <- subset(FinalData, year == 1990 & industry == "Manufacturing")
Retail1990 <- subset(FinalData, year == 1990 & industry == "Retail")

Hightech2000 <- subset(FinalData, year == 2000 & industry == "Hightech")
Manufacturing2000 <- subset(FinalData, year == 2000 & industry == "Manufacturing")
Retail2000 <- subset(FinalData, year == 2000 & industry == "Retail")

saveRDS(Retail2000,file=paste(getwd(),'/../','data/simulated/Retail2000.rda',sep=''))
saveRDS(Retail1990,file=paste(getwd(),'/../','data/simulated/Retail1990.rda',sep=''))
saveRDS(Retail1980,file=paste(getwd(),'/../','data/simulated/Retail1980.rda',sep=''))

saveRDS(Manufacturing2000,file=paste(getwd(),'/../','data/simulated/Manufacturing2000.rda',sep=''))
saveRDS(Manufacturing1990,file=paste(getwd(),'/../','data/simulated/Manufacturing1990.rda',sep=''))
saveRDS(Manufacturing1980,file=paste(getwd(),'/../','data/simulated/Manufacturing1980.rda',sep=''))

saveRDS(Hightech2000,file=paste(getwd(),'/../','data/simulated/Hightech2000.rda',sep=''))
saveRDS(Hightech1990,file=paste(getwd(),'/../','data/simulated/Hightech1990.rda',sep=''))
saveRDS(Hightech1980,file=paste(getwd(),'/../','data/simulated/Hightech1980.rda',sep=''))

RegM1980 <- lm(Hightech1980[,4] ~ Manufacturing1980[,4])
RegR1980 <- lm(Hightech1980[,4] ~ Retail1980[,4])
RegM1990 <- lm(Hightech1990[,4] ~ Manufacturing1990[,4])
RegR1990 <- lm(Hightech1990[,4] ~ Retail1990[,4])
RegM2000 <- lm(Hightech2000[,4] ~ Manufacturing2000[,4])
RegR2000 <- lm(Hightech2000[,4] ~ Retail2000[,4])
