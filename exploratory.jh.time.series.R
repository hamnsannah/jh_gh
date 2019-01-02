
#takes as input the output of the clean.jh.detailed.sales.R script

#STILL NEED TO JOIN WITH SEQUENCE WITH ALL DATES TO FILL IN MISSING DATA.  THERE AREN'T SALES FOR EVERY DATE

exploratory.jh.time.series <- function(clean.df, freq = 365){
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step
  start.year <- year(clean.df$Day)[1]
  if(freq == 365){
    start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
    sales.agg <- aggregate(Total.Sales ~ Day, clean.df, sum)
    all.dates <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)),by = 1)
    sales.agg.all <- left_join(data.frame(Day = all.dates), sales.agg)
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.day), frequency = 365)
  }
  if(freq == 12){
    start.month <- month(head(clean.df$Month,1))
    sales.agg <- aggregate(Total.Sales ~ Month + Year, clean.df, sum)
    print(head(sales.agg, 25))
    sales.agg.all <- left_join(sales.agg, data.frame(Month = 1:12))
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.month), frequency = 12)
  }
  sales.ts[is.na(sales.ts)] <- 0
  
  sales.ts
}


