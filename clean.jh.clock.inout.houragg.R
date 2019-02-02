clean.jh.clock.inout.houragg <- function(cleaned.clock.data){
  clock.df <- cleaned.clock.data
  print(ceiling_date(Sys.time()))
  hourly.df <- data.frame()
  for(i in nrow(clock.data)){
    row.i <- clock.df[i,]
    for(k in round(row.i$HoursWorked)){
      
    
    if(ceiling_date(row.i$TimeIn[1], "hour") < row.i$TimeOut[1]){
      row.k$TimeBegin <- row.i$TimeIn[1]
      # need to finish these two loops which will turn labor data into an automated calculations of hourly totals
    }  
    }
  }
}