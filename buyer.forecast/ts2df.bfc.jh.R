#this function takes the output of fit.bfc.jh.R and turns it back into a data.frame so can be filtered with calendar interface in Shiny
ts2df.bfc.jh <- function(fc.obj){
  require(lubridate)
  df <- data.frame(fc.obj)
  freq <- nrow(df)
  df$year <- floor(as.numeric(row.names(df)))
  df$freq.increments <- round((as.numeric(row.names(df))-df$year)*freq)+1 # +1 makes it human readable.  "First week of year" = 1
  if(freq==53){
    df$date <- date(paste0(df$year, "-01-01"))+weeks(df$freq.increments-1)
  } else if(freq==365){
    df$date <- date(paste0(df$year, "-01-01"))+(df$freq.increments-1)
  } else if(freq==12){
    df$date <- date(paste0(df$year, "-01-01"))+months(df$freq.increments-1)
  }
  #datefloors <- r
  df
}