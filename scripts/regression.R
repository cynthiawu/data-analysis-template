dataclean = readRDS(paste(getwd(),'/../','data/cleaned/clean2.rda',sep=''))

FinalData <- aggregate(cbind(dataclean[,4]) ~ dataclean[,1] + dataclean[,2] + dataclean[,3], FUN = sum)

colnames(FinalData) <- c("year", "place", "industry", "jobs")

Retail2000 = readRDS(paste(getwd(),'/../','data/simulated/Retail2000.rda',sep=''))
Retail2000["log_jobs"] <- NA
Retail2000[,5] <- log(Retail2000[,4])

Retail1990 = readRDS(paste(getwd(),'/../','data/simulated/Retail1990.rda',sep=''))
Retail1990["log_jobs"] <- NA
Retail1990[,5] <- log(Retail1990[,4])

Retail1980 = readRDS(paste(getwd(),'/../','data/simulated/Retail1980.rda',sep=''))
Retail1980["log_jobs"] <- NA
Retail1980[,5] <- log(Retail1980[,4])

Manufacturing2000= readRDS(paste(getwd(),'/../','data/simulated/Manufacturing2000.rda',sep=''))
Manufacturing2000["log_jobs"] <- NA
Manufacturing2000[,5] <- log(Manufacturing2000[,4])

Manufacturing1990 = readRDS(paste(getwd(),'/../','data/simulated/Manufacturing1990.rda',sep=''))
Manufacturing1990["log_jobs"] <- NA
Manufacturing1990[,5] <- log(Manufacturing1990[,4])

Manufacturing1980 = readRDS(paste(getwd(),'/../','data/simulated/Manufacturing1980.rda',sep=''))
Manufacturing1980["log_jobs"] <- NA
Manufacturing1980[,5] <- log(Manufacturing1980[,4])

Hightech2000 = readRDS(paste(getwd(),'/../','data/simulated/Hightech2000.rda',sep=''))
Hightech2000["log_jobs"] <- NA
Hightech2000[,5] <- log(Hightech2000[,4])

Hightech1990 = readRDS(paste(getwd(),'/../','data/simulated/Hightech1990.rda',sep=''))
Hightech1990["log_jobs"] <- NA
Hightech1990[,5] <- log(Hightech1990[,4])

Hightech1980 = readRDS(paste(getwd(),'/../','data/simulated/Hightech1980.rda',sep=''))
Hightech1980["log_jobs"] <- NA
Hightech1980[,5] <- log(Hightech1980[,4])

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


plot(Hightech1980[,5], Manufacturing1980[,5], xlab = "log value of Hightech Employment", ylab = "log value of Manufacturing Employment", main = "Regression of Manufacturing on Hightech Employment in 1980")
abline(LogRegM1980)

plot(Hightech1990[,5], Manufacturing1990[,5], xlab = "log value of Hightech Employment", ylab = "log value of Manufacturing Employment", main = "Regression of Manufacturing on Hightech Employment in 1990")
abline(LogRegM1990)

plot(Hightech2000[,5], Manufacturing2000[,5], xlab = "log value of Hightech Employment", ylab = "log value of Manufacturing Employment", main = "Regression of Manufacturing on Hightech Employment in 2000")
abline(LogRegM2000)

plot(Hightech1980[,5], Retail1980[,5], xlab = "log value of Hightech Employment", ylab = "log value of Retail Employment", main = "Regression of Retail on Hightech Employment in 1980")
abline(LogRegR1980)

plot(Hightech1990[,5], Retail1990[,5], xlab = "log value of Hightech Employment", ylab = "log value of Retail Employment", main = "Regression of Retail on Hightech Employment in 1990")
abline(LogRegR1990)

plot(Hightech2000[,5], Retail2000[,5], xlab = "log value of Hightech Employment", ylab = "log value of Retail Employment", main = "Regression of Retail on Hightech Employment in 2000")
abline(LogRegR2000)

require(maps)
require(ggplot2)
require(data.table)

us.state.map <- map_data('state')
head(us.state.map)
states <- levels(as.factor(us.state.map$region))
df <- data.frame(region = states, value = runif(length(states), min=0, max=100), stringsAsFactors = FALSE)

map.data <- merge(us.state.map, df, by='region', all=T)
map.data <- map.data[order(map.data$order),]
head(map.data)

map.county <- data.table(map_data('county'))
map.county[,value:=sample(5, 1), by=list(region, subregion)]

ggplot(map.data, aes(x = long, y = lat, group=group, fill=value)) + geom_polygon(colour = "white")

ggplot(map.county, aes(x = long, y = lat, group=group, fill=as.factor(value))) + geom_polygon(colour = "white")
