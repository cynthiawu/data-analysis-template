dataclean = readRDS(paste(getwd(),'/../','data/cleaned/clean2.rda',sep=''))

FinalData <- aggregate(cbind(dataclean[,4]) ~ dataclean[,1] + dataclean[,2] + dataclean[,3], FUN = sum)

colnames(FinalData) <- c("year", "place", "industry", "jobs")

Hightech1980 <- subset(FinalData, year == 1980 & industry == "Hightech")
Hightech1980["log_jobs"] <- NA
Hightech1980[,5] <- log(Hightech1980[,4])
saveRDS(Hightech1980, paste(getwd(),'/../','data/simulated/Hightech1980.rda',sep=''))


Manufacturing1980 <- subset(FinalData, year == 1980 & industry == "Manufacturing")
Manufacturing1980["log_jobs"] <- NA
Manufacturing1980[,5] <- log(Manufacturing1980[,4])
saveRDS(Manufacturing1980, paste(getwd(),'/../','data/simulated/Manufacturing1980.rda',sep=''))

Retail1980 <- subset(FinalData, year == 1980 & industry == "Retail")
Retail1980["log_jobs"] <- NA
Retail1980[,5] <- log(Retail1980[,4])
saveRDS(Retail1980, paste(getwd(),'/../','data/simulated/Retail1980.rda',sep=''))

Hightech1990 <- subset(FinalData, year == 1990 & industry == "Hightech")
Hightech1990["log_jobs"] <- NA
Hightech1990[,5] <- log(Hightech1990[,4])
saveRDS(Hightech1990, paste(getwd(),'/../','data/simulated/Hightech1990.rda',sep=''))

Manufacturing1990 <- subset(FinalData, year == 1990 & industry == "Manufacturing")
Manufacturing1990["log_jobs"] <- NA
Manufacturing1990[,5] <- log(Manufacturing1990[,4])
saveRDS(Manufacturing1990, paste(getwd(),'/../','data/simulated/Manufacturing1990.rda',sep=''))

Retail1990 <- subset(FinalData, year == 1990 & industry == "Retail")
Retail1990["log_jobs"] <- NA
Retail1990[,5] <- log(Retail1990[,4])
saveRDS(Retail1990, paste(getwd(),'/../','data/simulated/Retail1990.rda',sep=''))

Hightech2000 <- subset(FinalData, year == 2000 & industry == "Hightech")
Hightech2000["log_jobs"] <- NA
Hightech2000[,5] <- log(Hightech2000[,4])
saveRDS(Hightech2000, paste(getwd(),'/../','data/simulated/Hightech2000.rda',sep=''))

Manufacturing2000 <- subset(FinalData, year == 2000 & industry == "Manufacturing")
Manufacturing2000["log_jobs"] <- NA
Manufacturing2000[,5] <- log(Manufacturing2000[,4])
saveRDS(Manufacturing2000, paste(getwd(),'/../','data/simulated/Manufacturing2000.rda',sep=''))

Retail2000 <- subset(FinalData, year == 2000 & industry == "Retail")
Retail2000["log_jobs"] <- NA
Retail2000[,5] <- log(Retail2000[,4])
saveRDS(Retail2000, paste(getwd(),'/../','data/simulated/Retail2000.rda',sep=''))

RegM1980 <- lm(Hightech1980[,4] ~ Manufacturing1980[,4])
RegR1980 <- lm(Hightech1980[,4] ~ Retail1980[,4])
RegM1990 <- lm(Hightech1990[,4] ~ Manufacturing1990[,4])
RegR1990 <- lm(Hightech1990[,4] ~ Retail1990[,4])
RegM2000 <- lm(Hightech2000[,4] ~ Manufacturing2000[,4])
RegR2000 <- lm(Hightech2000[,4] ~ Retail2000[,4])


LogRegM1980 <- lm(Hightech1980[,5] ~ Manufacturing1980[,5])
LogRegR1980 <- lm(Hightech1980[,5] ~ Retail1980[,5])

LogRegM1990 <- lm(Hightech1990[,5] ~ Manufacturing1990[,5])
LogRegR1990 <- lm(Hightech1990[,5] ~ Retail1990[,5])

LogRegM2000 <- lm(Hightech2000[,5] ~ Manufacturing2000[,5])
LogRegR2000 <- lm(Hightech2000[,5] ~ Retail2000[,5])

jpeg('../graphs/h8m8.jpg')
plot(Hightech1980[,5], Manufacturing1980[,5], xlab = "log value of Hightech Employment", ylab = "log value of Manufacturing Employment", main = "Regression of Manufacturing on Hightech Employment in 1980")
abline(LogRegM1980)
dev.off()

jpeg('../graphs/h9m9.jpg')
plot(Hightech1990[,5], Manufacturing1990[,5], xlab = "log value of Hightech Employment", ylab = "log value of Manufacturing Employment", main = "Regression of Manufacturing on Hightech Employment in 1990")
abline(LogRegM1990)
dev.off()

jpeg('../graphs/h0m0.jpg')
plot(Hightech2000[,5], Manufacturing2000[,5], xlab = "log value of Hightech Employment", ylab = "log value of Manufacturing Employment", main = "Regression of Manufacturing on Hightech Employment in 2000")
abline(LogRegM2000)
dev.off()

jpeg('../graphs/h8r8.jpg')
plot(Hightech1980[,5], Retail1980[,5], xlab = "log value of Hightech Employment", ylab = "log value of Retail Employment", main = "Regression of Retail on Hightech Employment in 1980")
abline(LogRegR1980)
dev.off()

jpeg('../graphs/h9r9.jpg')
plot(Hightech1990[,5], Retail1990[,5], xlab = "log value of Hightech Employment", ylab = "log value of Retail Employment", main = "Regression of Retail on Hightech Employment in 1990")
abline(LogRegR1990)
dev.off()

jpeg('../graphs/h0r0.jpg')
plot(Hightech2000[,5], Retail2000[,5], xlab = "log value of Hightech Employment", ylab = "log value of Retail Employment", main = "Regression of Retail on Hightech Employment in 2000")
abline(LogRegR2000)
dev.off()

