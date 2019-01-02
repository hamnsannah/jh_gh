# must be already connected to database before using this function, probably with import.jh.connect.R

#sample usage: detailed.sales.data <- import.jh.detailed.sales()

import.jh.detailed.sales <- function(){
  joined.df <- DBI::dbGetQuery(con,'
                                SELECT TransactionEntry.ItemID, TransactionEntry.Price, 
                                 TransactionEntry.TransactionTime, TransactionEntry.Quantity, 
                                  TransactionEntry.TransactionNumber, Item.Description, 
                                 Department.Name AS "Department", Category.Name AS "Category", 
                                 SupplierName AS "Supplier", Cashier.Name AS "CashierName", 
                                 Register.Number AS "Register", Register.Description AS "RegLocation"

                                 FROM TransactionEntry
                                 
                                 JOIN Item ON TransactionEntry.ItemID = Item.ID
                                 JOIN Department ON Item.DepartmentID = Department.ID
                                 JOIN Category ON Item.CategoryID = Category.ID
                                 JOIN Supplier ON Item.SupplierID = Supplier.ID
                                 JOIN [Transaction] ON TransactionEntry.TransactionNumber = [Transaction].TransactionNumber
                                 JOIN Batch ON [Transaction].BatchNumber = Batch.BatchNumber
                                 JOIN Register ON Batch.RegisterID = Register.ID
                                 JOIN Cashier ON [Transaction].CashierID = Cashier.ID')
  joined.df
  }