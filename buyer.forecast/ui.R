#ui.R for supplier dash shiny project
library(shiny)
require(dplyr)
require(lubridate)

setwd("C://Users/The Pritchard family/Documents/R/jh_gh/")
dept.cat.supp <- read.csv("data/dept.cat.supp.csv", stringsAsFactors = FALSE)


department.vector <- c("ALL DEPTS.", sort(unique(dept.cat.supp$Department)))
category.vector <- c("ALL CATEGORIES", sort(unique(dept.cat.supp$Category)))
supplier.vector <- c("ALL SUPPLIERS", sort(unique(dept.cat.supp$Supplier)))
#product.vector <- c("Product 1", "Product 2")
default.end <- today()+months(6)
shinyUI(pageWithSidebar(
  headerPanel(" "),
  sidebarPanel(
    selectInput('id1','Department', choices = department.vector),
    selectInput('id2', 'Category', choices = category.vector),
    selectInput('id3', 'Supplier', choices = supplier.vector), #, selected = "ACOMO JEWELRY"),
    #selectInput('id4', 'Product', choices = product.vector), # needs both description and item number
    
    uiOutput("product.list"),
    
    dateInput('id5', 'Forecast Start Date'),
    dateInput('id6', 'Forecast End Date', value = today()+weeks(26)),
    selectInput('id8', 'Select Dollars or Units', choices = c("Dollars", "Units"), selected = "Dollars"),
    actionButton("go", "Calculate Forecast")
    #h5('This dashboard provides multiple views of the sales of the selected supplier.  
    #   It shows whether a supplier is outperforming or underperforming prior years and which categories and products are driving that trend.')
  ),
  mainPanel(
    dataTableOutput("output.text"),
    plotOutput("fc.plot")
  )
  )
)