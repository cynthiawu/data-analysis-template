load("~/proj2/data-analysis-template/citydata.RData")

manu = c("Meat products","Food industries, n.s.","Apparel and accessories, except knit","Pulp, paper, and paperboard mills","Soaps and cosmetics","Miscellaneous plastics products","Footwear, except rubber and plastic","Furniture and fixtures","Iron and steel foundries")
retail = c("Department stores","Food stores, n.e.c.","Apparel and accessory stores, except shoe","Shoe stores","Furniture and home furnishings stores","Eating and drinking places","Book and stationery stores","Jewelry stores")
hightech = c("Computers and related equipment","Machinery, except electrical, n.e.c.","Radio, TV, and communication equipment","Electrical machinery, equipment, and supplies, n.e.c.","Aircraft and parts","Computer and data processing services","Computer and data processing services","Engineering, architectural, and surveying services","Machinery, n.s.","Motor vehicles and motor vehicle equipment")

lev = levels(citydata$ind1990)

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
levels(citydata$ind1990) <- lev

dataclean = citydata[as.character(citydata$ind1990) != "bad data",]


