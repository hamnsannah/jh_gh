table.bfc.jh <- function(forecast.df){
  forecast.start.date <- min(forecast.df$date)
  forecast.end.date <- max(forecast.df$date)
  point.fc <- sum(forecast.df$Point.Forecast)
  safety.fc <- sum(forecast.df[,3]) # column name could change if using confidence interval other than 80.  80% means 90% is below it.
  table.with.data <- data.frame("Forecast to Stay In-Stock"= safety.fc, "Forecast 50-50" = point.fc, 
                                   "Forecast Start Date" = forecast.start.date, "Forecast End Date"= forecast.end.date)
  #table.with.data$forecast.start.date <- date(table.with.data$forecast.start.date)
  #table.with.data$forecast.end.date <- date(table.with.data$forecast.end.date)
  colnames(table.with.data) <- c("Forecast to Stay In-Stock", "Forecast 50-50", "Forecast Start Date", "Forecast End Date")
  table.with.data
}