require(lubridate)
require(dplyr)
labor.data$Day <- date(labor.data$TimeIn)

labor.data$TimeIn <- ymd_hms(labor.data$TimeIn)
labor.data$TimeOut <- ymd_hms(labor.data$TimeOut)

labor.data$SecondsWorked <- labor.data$TimeOut - labor.data$TimeIn
labor.data$HoursWorked <- as.numeric(labor.data$SecondsWorked)/3600
labor.data <- arrange(labor.data, Day)