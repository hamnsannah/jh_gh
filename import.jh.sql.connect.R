#establishes connection to any database imported into SQL Server.

import.jh.sql.connect <- function(db.name.in.sql.server){
  library(odbc)
  library(DBI)
  
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver = "SQL Server",
                        Server = "LAPTOP-JHRVTF04\\SQLEXPRESS",
                        Database = db.name.in.sql.server,
                        Trusted_Connection = "True")
  con
}