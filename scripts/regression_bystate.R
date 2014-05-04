dataclean = readRDS(paste(getwd(),'/../','data/cleaned/clean2.rda',sep=''))

data <- aggregate(cbind(dataclean[,4]) ~ dataclean[,1] + dataclean[,2] + dataclean[,3], FUN = sum)

colnames(data) <- c("year", "place", "industry", "jobs")

states = c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "missouri", "mississippi", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming")
abrev = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

lev = levels(data$place)

for (i in 1:length(lev)) {
  tmp = as.character(lev[i])
  lev[i] <- substr(tmp, nchar(tmp)-1, nchar(tmp))
}

for (i in 1:length(lev)) {
  index = match(as.character(lev[i]), abrev)
  lev[i] <- states[index]
}

levels(data$place) <- lev
coeff <- vector()

for (i in 1:length(states)) {
  tmpd = data[as.character(data$place) == states[i],]
  tmpdh = tmpd[as.character(tmpd$industry) == "Hightech",]
  tmpdm = tmpd[as.character(tmpd$industry) == "Manufacturing",]
  result = tryCatch({
    summ =  summary(lm(tmpdh[,4] ~ tmpdm[,4]))
    coef = summ$coefficients[2,1]
    if (is.na(coef)) {
      coef = 0
     }
    coeff[i] = coef
}, error = function(e){
     coeff[i] = 0
   })
}
coeff[is.na(coeff)] <- 0


