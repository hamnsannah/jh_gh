# this script takes data from the TimeClock SQL table that's been cleaned by clean.jh.clock.inout.R
# ...and it cuts the clocked in and clocked out times into hours to enable hourly aggregation...
# ...then it aggregates it.  This is a processing step to prepare for vizzes of hourly sales to inform staffing

# sample usage: labor.agg <- clean.jh.clock.inout.houragg(labor.data)

clean.jh.clock.inout.houragg <- function(cleaned.clock.data){
  
  
  clock.df <- cleaned.clock.data
  clock.df <- clock.df[!is.na(clock.df$TimeOut),]
  clock.df <- clock.df[!is.na(clock.df$TimeIn),]
  require(lubridate)
  #print(ceiling_date(Sys.time()))
  hourly.df <- data.frame()
  for(i in 1:nrow(clock.df)){
    row.i <- clock.df[i,] #row.i is one row containing an employee's entire shift
    print(paste("i=", i))
    print(nrow(clock.df))
    row.k <- select(row.i, StoreID, ID, CashierID, TimeBegin = TimeIn, TimeEnd = TimeOut, BatchNumber, Day)
    #print("initial row.k")
    #print(row.k)

    
    k.iterations <- as.numeric(ceiling_date(row.k$TimeEnd, "hour")- floor_date(row.k$TimeBegin, "hour"))
    
    if(ceiling_date(row.k$TimeBegin, "hour") < row.k$TimeEnd){
      row.k$TimeEnd <- ceiling_date(row.k$TimeBegin, "hour")     
    }
    #print(k.iterations)
    for(k in 1:k.iterations){   #(ceiling(row.i$HoursWorked)+1)){ #row.i$TimeBegin
      #print(paste("k =",k))
    
    if(ceiling_date(row.k$TimeBegin, "hour") < row.k$TimeEnd){ #if TimeOut goes beyond end of hour, and prevents scenario of 1:17-1:43 shift
      #print("end at top of hour")

      #row.k$TimeEnd <- ceiling_date(row.k$TimeBegin, "hour")        
      if(row.k$TimeEnd == row.k$TimeBegin){ #fixes for error that ceiling_date for date on the hour is itself
        row.k$TimeEnd <- row.k$TimeBegin+hours(1)
        row.k$TimeDiff <- as.numeric(1)
      }else{
        row.k$TimeDiff <- as.numeric((row.k$TimeEnd-row.k$TimeBegin)/60)
      }

      #print(row.k)
      hourly.df <- rbind(hourly.df, row.k) # add to df here
 
      # need to finish these two loops which will turn labor data into an automated calculations of hourly totals
    }  else if(row.i$TimeIn[1] < row.i$TimeOut[1]){ # if incremented TimeIn is still earlier than TimeEnd
      #print("end at partial hour")
      #row.k <- select(row.i, StoreID, ID, CashierID, BatchNumber, Day)
      #row.k$TimeBegin <- row.i$TimeIn[1]
      #row.k$TimeEnd <- row.i$TimeOut[1]
      row.k$TimeDiff <- as.numeric((row.k$TimeEnd-row.k$TimeBegin)/60)
      #print(row.k)
      hourly.df <- rbind(hourly.df, row.k)
    }
      #prepare for next iteration
      #print("tests of quantities")
      #print(row.k$TimeBegin)
      #print(row.k$TimeEnd)
      #print(row.i$TimeOut)
      #print(row.k$TimeEnd+hours(1))
      #print("end test of quantities")
      
      row.k$TimeBegin <- row.k$TimeEnd #advances time increment by one hour
      if(row.i$TimeOut < (row.k$TimeEnd+hours(1))){
        row.k$TimeEnd <- row.i$TimeOut
      }else{
        row.k$TimeEnd <- (row.k$TimeEnd+hours(1))
      }
      #print(row.k$TimeEnd)
    }
  }
  hourly.df$Day <- date(hourly.df$TimeBegin)
  hourly.df$TimeFloor <- floor_date(hourly.df$TimeBegin, "hour")
  hourly.agg <- aggregate(TimeDiff ~ TimeFloor + Day, sum, hourly.df)
  write.csv(hourly.df, "hourly.df.csv", row.names = FALSE)
  hourly.agg #hourly.df
}

#take row.i and supply those values to row.k initially.
#leave the row.k begin date, but cut end date to top of hour if it extends past.
# for next row make the begin date the last row.k's end date
# if row.i end date is less than last row.k end date, use that.  If not just add one.  