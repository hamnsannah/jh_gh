require(lubridate)
labor.data$Day <- date(labor.data$TimeIn)

labor.data$TimeIn <- ymd_hms(labor.data$TimeIn)
labor.data$TimeOut <- ymd_hms(labor.data$TimeOut)

labor.data$SecondsWorked <- labor.data$TimeOutDate - labor.data$TimeInDate
labor.data$HoursWorked <- as.numeric(labor.data$SecondsWorked)/3600