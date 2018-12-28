
#takes as input the output of the clean.jh.detailed.sales.R script

#STILL NEED TO JOIN WITH SEQUENCE WITH ALL DATES TO FILL IN MISSING DATA.  THERE AREN'T SALES FOR EVERY DATE

exploratory.time.series <- function(clean.df){
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step
  start.year <- year(clean.df$Day)[1]
  start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
  sales.agg <- aggregate(Total.Sales ~ Day, clean.df, sum)
  #all.dates <- seq(date())...
  
  sales.ts <- ts(sales.agg$Total.Sales, start = c(start.year, start.day), frequency = 365)
  
  sales.ts
}


