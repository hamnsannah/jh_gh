clean.jh.anonymize.sales <- function(clean.df){
  #meant for use with cached files read in that have Dept.By.Year and Categ.By.Year as columns
  
  #Departments
  dept.vec <- unique(clean.df$Department)
  dept.len <- length(dept.vec)
  dept.anon <- paste0("Department", seq(from = 1, to = dept.len, by = 1))
  
  for(i in 1:dept.len){
    clean.df$Department[clean.df$Department %in% dept.vec[i]] <- dept.anon[i]
  }
  print("Departments complete")
  
  #Categories
  categ.vec <- unique(clean.df$Category)
  categ.len <- length(categ.vec)
  categ.anon <- paste0("Category", seq(from = 1, to = categ.len, by = 1))
  
  for(i in 1:categ.len){
    clean.df$Category[clean.df$Category %in% categ.vec[i]] <- categ.anon[i]
  }
  print("Categories complete")
  
  #Product Descriptions
  prod.vec <- unique(clean.df$Description)
  prod.len <- length(prod.vec)
  prod.anon <- paste0("Description", seq(from = 1, to = prod.len, by = 1))
  
  for(i in 1:prod.len){
    clean.df$Description[clean.df$Description %in% prod.vec[i]] <- prod.anon[i]
  }
  print("Product descriptions complete")
  
  #Supplier
  supp.vec <- unique(clean.df$Supplier)
  supp.len <- length(supp.vec)
  supp.anon <- paste0("Supplier", seq(from = 1, to = supp.len, by = 1))
  
  for(i in 1:supp.len){
    clean.df$Supplier[clean.df$Supplier %in% supp.vec[i]] <- supp.anon[i]
    
  }
  print("Suppliers complete")
  
  #Cashier
  cash.vec <- unique(clean.df$Cashier)
  cash.len <- length(cash.vec)
  cash.anon <- paste0("Cashier", seq(from = 1, to = cash.len, by = 1))
  
  for(i in 1:cash.len){
    clean.df$Cashier[clean.df$Cashier %in% cash.vec[i]] <- cash.anon[i]
  }
  print("Cashiers complete")

  #Dept.By.Year
  deptyear.vec <- unique(clean.df$Dept.By.Year)
  deptyear.len <- length(deptyear.vec)
  deptyear.anon <- paste(clean.df$Department, clean.df$Year)
  
  for(i in 1:deptyear.len){
    clean.df$Dept.By.Year[clean.df$Dept.By.Year %in% deptyear.vec[i]] <- deptyear.anon[i]
  }
  print("Depts by year complete")
  
  #Categ.By.Year
  categyear.vec <- unique(clean.df$Categ.By.Year)
  categyear.len <- length(categyear.vec)
  categyear.anon <- paste(clean.df$Category, clean.df$Year)
  
  for(i in 1:categyear.len){
    clean.df$Categ.By.Year[clean.df$Categ.By.Year %in% categyear.vec[i]] <- categyear.anon[i]
  }
  print("Category by year complete")
    
  clean.df
}
  
  # need to anonymize Departments, Categories, Products, and Suppliers.  