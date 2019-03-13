#this is an alternate version of exploratory.jh.time.series.R that aggregates units (Qty.Sold) instead of Total.Sales
#tried to use aggregate() to group based on a column designated in a function but couldn't get it working


exploratory.jh.time.series.units <- function(clean.df, freq = 365){
  # includes day, week, and year frequency options
  if(freq == 52){print("Error: use freq = 53 for weekly to account for partial week at EOY")}
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step but doing it here too in case not
  start.year <- year(clean.df$Day)[1]
  
  if(freq == 365){
    start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
    sales.agg <- aggregate(Qty.Sold ~ Day, clean.df, sum)
    all.dates <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), 
                          to = as.Date(max(sales.agg$Day, na.rm = TRUE)),by = 1)
    sales.agg.all <- left_join(data.frame(Day = all.dates), sales.agg)
    sales.ts <- ts(sales.agg.all$Qty.Sold, start = c(start.year, start.day), frequency = 365)
  }
  if(freq == 12){
    start.month <- month(head(clean.df$Month,1))
    sales.agg <- aggregate(Qty.Sold ~ Month + Year, clean.df, sum)
    sales.agg.all <- left_join(sales.agg, data.frame(Month = 1:12))
    sales.ts <- ts(sales.agg.all$Qty.Sold, start = c(start.year, start.month), frequency = 12)
  }
  if(freq == 53){
    clean.df$Week <- week(clean.df$Date.Sold)
    start.week <- head(clean.df$Week,1)
    sales.agg <- aggregate(Qty.Sold ~ Week + Year, clean.df, sum)
    sales.agg.all <- left_join(sales.agg, data.frame(Week = 1:53))
    sales.ts <- ts(sales.agg.all$Qty.Sold, start = c(start.year, start.week), frequency = 53)
    
  }
  sales.ts[is.na(sales.ts)] <- 0
  
  sales.ts
}