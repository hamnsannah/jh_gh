# this script takes the raw data imported by import.jh.detailed.sales and formats it like the wt detailed sales report columns

clean.jh.detailed.sales <- function(sql.df){
  library(dplyr)
  library(lubridate)
    
    sql.df <- sql.df %>%
      rename(Sold.Price = Price, Cashier = CashierName, Date.Sold = TransactionTime,
             Qty.Sold = Quantity, Transaction = TransactionNumber) %>%
      mutate(Total.Sales = Qty.Sold*Sold.Price) %>%
      select(Department, Category, Supplier, ItemID, Description, Qty.Sold, Sold.Price, 
             Total.Sales, Transaction, Date.Sold, Register, Cashier)
    
    sql.df$Date.Sold <- ymd_hms(sql.df$Date.Sold)
    sql.df$Year <- year(sql.df$Date.Sold)
    sql.df$Month <- month(sql.df$Date.Sold)
    sql.df$Department <- trimws(sql.df$Department)
    sql.df$Category <- trimws(sql.df$Category)
    sql.df$Supplier <- trimws(sql.df$Supplier)
    sql.df$Item <- trimws(sql.df$Item)
    #print(head(sql.df))
    sql.df$Dept.By.Year <- paste(sql.df$Department, sql.df$Year)
    sql.df$Categ.By.Year <- paste(sql.df$Category, sql.df$Year)
    sql.df
  }