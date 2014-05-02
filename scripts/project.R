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
Manufacturing1980 <- subset(FinalData, year == 1980 & industry == "Manufacturing")
Retail1980 <- subset(FinalData, year == 1980 & industry == "Retail")

Hightech1990 <- subset(FinalData, year == 1990 & industry == "Hightech")
Manufacturing1990 <- subset(FinalData, year == 1990 & industry == "Manufacturing")
Retail1990 <- subset(FinalData, year == 1990 & industry == "Retail")

Hightech2000 <- subset(FinalData, year == 2000 & industry == "Hightech")
Manufacturing2000 <- subset(FinalData, year == 2000 & industry == "Manufacturing")
Retail2000 <- subset(FinalData, year == 2000 & industry == "Retail")

RegM1980 <- lm(Hightech1980[,4] ~ Manufacturing1980[,4])
RegR1980 <- lm(Hightech1980[,4] ~ Retail1980[,4])
RegM1990 <- lm(Hightech1990[,4] ~ Manufacturing1990[,4])
RegR1990 <- lm(Hightech1990[,4] ~ Retail1990[,4])
RegM2000 <- lm(Hightech2000[,4] ~ Manufacturing2000[,4])
RegR2000 <- lm(Hightech2000[,4] ~ Retail2000[,4])