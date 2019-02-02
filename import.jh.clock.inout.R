import.jh.clock.inout <- function(){
  joined.df <- DBI::dbGetQuery(con,'
                              SELECT [StoreID], [ID], [CashierID], [TimeIn], [TimeOut], [BatchNumber]

                              FROM TimeClock')
                               
  joined.df
}