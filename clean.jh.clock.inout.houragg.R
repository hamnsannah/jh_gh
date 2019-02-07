#####
# Can account for 12:17-1:00 and 1:00-2:00, but not 2:00-2:23.  Seems to round 2:23 to 3 or not include the hour at all
#####

clean.jh.clock.inout.houragg <- function(cleaned.clock.data){
  
  
  clock.df <- cleaned.clock.data
  require(lubridate)
  print(ceiling_date(Sys.time()))
  hourly.df <- data.frame()
  for(i in 1:8){ ##nrow(clock.df)){
    row.i <- clock.df[i,]
    print(paste("i=", i))
    row.k <- select(row.i, StoreID, ID, CashierID, BatchNumber, Day)
    for(k in 1:ceiling(row.i$HoursWorked)){
      #row.i$TimeBegin
      print(paste("k =",k))
    
    if(ceiling_date(row.i$TimeIn[1], "hour") < row.i$TimeOut[1]){ #if TimeEnd goes beyond end of hour
      print("top of hour")
      
      row.k$TimeBegin <- row.i$TimeIn[1]

      row.k$TimeEnd <- ceiling_date(row.k$TimeBegin, "hour")        
      if(row.k$TimeEnd == row.k$TimeBegin){ #fixes for error that ceiling_date for date on the hour is itself
        row.k$TimeEnd <- row.k$TimeBegin+hours(1)
        row.k$TimeDiff <- as.numeric(1)
      }else{
        row.k$TimeDiff <- as.numeric((row.k$TimeEnd-row.k$TimeBegin)/60)
      }

      print(row.k)
      row.i$TimeIn <- row.k$TimeEnd #advances time increment by one hour
      rbind(hourly.df, row.k)
      # need to finish these two loops which will turn labor data into an automated calculations of hourly totals
    }  else if(row.i$TimeIn[1] < row.i$TimeOut[1]){ # if incremented TimeIn is still earlier than TimeEnd
      print("partial hour")
      row.k <- select(row.i, StoreID, ID, CashierID, BatchNumber, Day)
      row.k$TimeBegin <- row.i$TimeIn[1]
      row.k$TimeEnd <- row.i$TimeOut[1]
      row.k$TimeDiff <- as.numeric((row.k$TimeEnd-row.k$TimeBegin)/60)
      print(row.k)
      rbind(hourly.df, row.k)
    }
    }
  }
  hourly.df
}