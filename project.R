library(foreign)
citydata <- read.dta("/Users/biyingli/src/stat133/data-analysis-template/city-data.dta")
subset(citydata, select=c("year", "msa", "ind1990", "jobs"))
NoEduData <- aggregate(cbind(citydata$jobs) ~ citydata$year + citydata$msa + citydata$ind1990, FUN = sum)

NoEduData3[,3] <- gsub("Meat products","Maunfacture",as.character(NoEduData3[,3]))



mgsub <- function(pattern, replacement, x, ...) {
  for (i in 1:length(pattern)) {
    result <- gsub(pattern[i], replacement[i], x, ...)
  }
  result
}


NoEduData3[,3] <- gsub("Meat products","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Food industries, n.s.","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Apparel and accessories, except knit","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Pulp, paper, and paperboard mills","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Soaps and cosmetics","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Miscellaneous plastics products","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Footwear, except rubber and plastic","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Furniture and fixtures","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Iron and steel foundries","Maunfacturing",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Department stores","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Food stores, n.e.c.","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Apparel and accessory stores, except shoe","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Shoe stores","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Furniture and home furnishings stores","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Eating and drinking places","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Book and stationery stores","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Jewelry stores","Retail",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Computers and related equipment","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Machinery, except electrical, n.e.c.","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Radio, TV, and communication equipment","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Electrical machinery, equipment, and supplies, n.e.c.","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Electrical machinery, equipment, and supplies, n.s.","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Aircraft and parts","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Computer and data processing services","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Engineering, architectural, and surveying services","Hightech",as.character(NoEduData3[,3]))
NoEduData3[,3] <- gsub("Machinery, n.s.","Hightech",as.character(NoEduData[,3]))
NoEduData3[,3] <- gsub("Motor vehicles and motor vehicle equipment","Hightech",as.character(NoEduData[,3]))

FinalData <- aggregate(cbind(NoEduData3[,4]) ~ NoEduData3[,1] + NoEduData3[,2] + NoEduData3[,3], FUN = sum)