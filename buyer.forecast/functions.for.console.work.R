# functions to import data and prepare to work in console
library(dplyr)
wt.data <- read.csv("data/sales.data.cache2019-03-02.csv", stringsAsFactors = FALSE)

wt.filter <- filter(wt.data, Supplier == "CLARA BEAU JEWELRY")

exploratory.jh.time.series <- function(clean.df, freq = 365){
  # includes day, week, and year frequency options
  if(freq == 52){print("Error: use freq = 53 for weekly to account for partial week at EOY")}
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step but doing it here too in case not
  start.year <- year(clean.df$Day)[1]
  
  if(freq == 365){
    start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
    sales.agg <- aggregate(Total.Sales ~ Day, clean.df, sum)
    all.dates <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), 
                          to = as.Date(max(sales.agg$Day, na.rm = TRUE)),by = 1)
    sales.agg.all <- left_join(data.frame(Day = all.dates), sales.agg)
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.day), frequency = 365)
  }
  if(freq == 12){
    start.month <- month(head(clean.df$Month,1))
    sales.agg <- aggregate(Total.Sales ~ Month + Year, clean.df, sum)
    sales.agg.all <- left_join(sales.agg, data.frame(Month = 1:12))
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.month), frequency = 12)
  }
  if(freq == 53){
    clean.df$Week <- week(clean.df$Date.Sold)
    start.week <- head(clean.df$Week,1)
    sales.agg <- aggregate(Total.Sales ~ Week + Year, clean.df, sum)
    sales.agg.all <- left_join(sales.agg, data.frame(Week = 1:53))
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.week), frequency = 53)
    
  }
  sales.ts[is.na(sales.ts)] <- 0
  
  sales.ts
}

wt.ts <- exploratory.jh.time.series(wt.filter, freq = 53)

fit.bfc.jh <- function(ts.obj, model.to.use = "arima", surplus.interval = 80){
  model.options = c("arima", "ets", "stlf", "snaive", "rwf", "mean", "drift","snaive+6mo") # in advanced menu
  require(fpp2)
  if(model.to.use == "arima"){
    ts.fit <- auto.arima(ts.obj)
    fc <- forecast(ts.fit, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "ets"){
    ts.fit <- ets(ts.obj)
    fc <- forecast(ts.fit, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "stlf"){
    fc <- stlf(ts.obj, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "snaive"){
    fc <- snaive(ts.obj, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "rwf"){
    fc <- rwf(ts.obj, h = frequency(ts.obj), level = surplus.interval)
  } else if(model.to.use == "mean"){
    # use mean of rolling 12 months
    ts.freq <- frequency(ts.obj)
    fc <- meanf(tail(ts.obj, ts.freq), h = frequency(ts.obj), level = surplus.interval)
  } else if(model.to.use == "drift"){
    fc <- rwf(ts.obj, drift = TRUE, h = frequency(ts.obj), level = surplus.interval)
  } else if(model.to.use == "snaive+6mo"){
    #snaive+6mo calculates the recent growth rate comparing the last 6 mo. of sales with the same period one year earlier.
    #then it multiplies the seasonal naive by that rate.
    ts.freq <- frequency(ts.obj)
    cy.6mo <- tail(ts.obj, ts.freq/2)
    py.6mo <- head(tail(ts.obj, ts.freq*1.5), ts.freq/2) #subset to last 18 mo. then to first 6 mo. of that subset
    growth.rate <- (sum(cy.6mo) - sum(py.6mo))/sum(py.6mo)
    ts.grow <- ts.obj*(1+growth.rate)
    fc <- snaive(ts.grow, h = frequency(ts.obj), level = surplus.interval)
  }
  #fc <- forecast(ts.fit, h = frequency(ts.obj), level = c(80))
  #autoplot(ts.obj) + autolayer(fc)
  fc
}

wt.fc <- fit.bfc.jh(wt.ts)


####
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