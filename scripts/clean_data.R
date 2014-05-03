IndData = readRDS(paste(getwd(),'/../','data/cleaned/clean1.rda',sep=''))

manu = c("Meat products","Food industries, n.s.","Apparel and accessories, except knit","Pulp, paper, and paperboard mills","Soaps and cosmetics","Miscellaneous plastics products","Footwear, except rubber and plastic","Furniture and fixtures","Iron and steel foundries")
retail = c("Department stores","Food stores, n.e.c.","Apparel and accessory stores, except shoe","Shoe stores","Furniture and home furnishings stores","Eating and drinking places","Book and stationery stores","Jewelry stores")
hightech = c("Computers and related equipment","Machinery, except electrical, n.e.c.","Radio, TV, and communication equipment","Electrical machinery, equipment, and supplies, n.e.c.","Aircraft and parts","Computer and data processing services","Computer and data processing services","Engineering, architectural, and surveying services","Machinery, n.s.","Motor vehicles and motor vehicle equipment")

edudata <- aggregate(cbind(IndData$jobs) ~ IndData$year + IndData$msa + IndData$ind1990, FUN = sum)
colnames(edudata) <- c("year", "msa", "ind1990", "jobs")

lev = levels(edudata$ind1990)

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
levels(edudata$ind1990) <- lev

dataclean = edudata[as.character(edudata$ind1990) != "bad data",]
data <- aggregate(cbind(dataclean$jobs) ~ dataclean$year + dataclean$msa + dataclean$ind1990, FUN = sum)
colnames(data) <- c("year", "msa", "ind1990", "jobs")

FinalData <- aggregate(cbind(data[,4]) ~ data[,1] + data[,2] + data[,3], FUN = sum)

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
saveRDS(data,file=paste(getwd(),'/../','data/cleaned/clean2.rda',sep=''))
