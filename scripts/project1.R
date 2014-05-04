library(foreign)

IndData <- read.dta("/Users/biyingli/src/stat133/data-analysis-template/industrydata.dta")
IndData <- subset(IndData, select=c("year", "msa", "ind1990", "jobs"))
NoEduData <- aggregate(cbind(IndData[,4]) ~ IndData[,1] + IndData[,2] + IndData[,3], FUN = sum)
colnames(NoEduData) <- c("year", "place", "ind1990", "jobs")

manu = c("Meat products","Food industries, n.s.","Apparel and accessories, except knit","Pulp, paper, and paperboard mills","Soaps and cosmetics","Miscellaneous plastics products","Footwear, except rubber and plastic","Furniture and fixtures","Iron and steel foundries")
retail = c("Department stores","Food stores, n.e.c.","Apparel and accessory stores, except shoe","Shoe stores","Furniture and home furnishings stores","Eating and drinking places","Book and stationery stores","Jewelry stores")
hightech = c("Computers and related equipment","Machinery, except electrical, n.e.c.","Radio, TV, and communication equipment","Electrical machinery, equipment, and supplies, n.e.c.","Aircraft and parts","Computer and data processing services","Computer and data processing services","Engineering, architectural, and surveying services","Machinery, n.s.","Motor vehicles and motor vehicle equipment")

lev = levels(NoEduData$ind1990)

for (i in 1:length(lev)) {
  if (lev[i] %in% manu) {
    lev[i] <- "Manufacturing"
  }
  else if (lev[i] %in% retail) {
    lev[i] <- "Retail"
  }
  else if (lev[i] %in% hightech) {
    lev[i] <- "Hightech"
  }
  else {
    lev[i] <- "bad data"
  }
}
levels(NoEduData$ind1990) <- lev

dataclean = NoEduData[as.character(NoEduData$ind1990) != "bad data",]

FinalData <- aggregate(cbind(dataclean[,4]) ~ dataclean[,1] + dataclean[,2] + dataclean[,3], FUN = sum)





colnames(FinalData) <- c("year", "place", "industry", "jobs")

Hightech1980 <- subset(FinalData, year == 1980 & industry == "Hightech")
Hightech1980["log_jobs"] <- NA
Hightech1980[,5] <- log(Hightech1980[,4])

Manufacturing1980 <- subset(FinalData, year == 1980 & industry == "Manufacturing")
Manufacturing1980["log_jobs"] <- NA
Manufacturing1980[,5] <- log(Manufacturing1980[,4])

Retail1980 <- subset(FinalData, year == 1980 & industry == "Retail")
Retail1980["log_jobs"] <- NA
Retail1980[,5] <- log(Retail1980[,4])

Hightech1990 <- subset(FinalData, year == 1990 & industry == "Hightech")
Hightech1990["log_jobs"] <- NA
Hightech1990[,5] <- log(Hightech1990[,4])

Manufacturing1990 <- subset(FinalData, year == 1990 & industry == "Manufacturing")
Manufacturing1990["log_jobs"] <- NA
Manufacturing1990[,5] <- log(Manufacturing1990[,4])

Retail1990 <- subset(FinalData, year == 1990 & industry == "Retail")
Retail1990["log_jobs"] <- NA
Retail1990[,5] <- log(Retail1990[,4])

Hightech2000 <- subset(FinalData, year == 2000 & industry == "Hightech")
Hightech2000["log_jobs"] <- NA
Hightech2000[,5] <- log(Hightech2000[,4])

Manufacturing2000 <- subset(FinalData, year == 2000 & industry == "Manufacturing")
Manufacturing2000["log_jobs"] <- NA
Manufacturing2000[,5] <- log(Manufacturing2000[,4])

Retail2000 <- subset(FinalData, year == 2000 & industry == "Retail")
Retail2000["log_jobs"] <- NA
Retail2000[,5] <- log(Retail2000[,4])


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



install.packages("maps")
install.packages("ggplot2")
install.packages("data.table")

require(maps)
require(ggplot2)
require(data.table)

us.state.map <- map_data('state')
head(us.state.map)
statesnames = c("alabama", "alaska", "arizona",	"arkansas", "california", "colorado", "connecticut", "delaware", "district of columbia", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")
abbrev_string = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

lev1 = levels(databystates$place)

for (i in 1:length(lev1)) {
	tmp = as.character(lev1[i])
	lev1[i] <- substr(tmp, nchar(tmp)-1, nchar(tmp))
}
levels(databystates$place) <- lev1

lev3 = levels(H1980State[,2])

for (i in 1:length(abbrev_string)) {
  index = match(abbrev_string[i], as.character(lev3))
  lev3[index] <- statesnames[index]
}

levels(NoEduData$ind1990) <- lev


states <- levels(as.factor(us.state.map$region))
df <- data.frame(region = states, value = runif(length(states), min=0, max=100), stringsAsFactors = FALSE)

map.data <- merge(us.state.map, df, by='region', all=T)
map.data <- map.data[order(map.data$order),]
head(map.data)

ggplot(map.data, aes(x = long, y = lat, group=group, fill=value)) + geom_polygon(colour = "white")




map.county <- data.table(map_data('county'))
map.county[,value:=sample(5, 1), by=list(region, subregion)]

ggplot(map.county, aes(x = long, y = lat, group=group, fill=as.factor(value))) + geom_polygon(colour = "white")
