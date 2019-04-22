#ui.R for supplier dash shiny project
library(shiny)

supplier.vector <- c("ACOMO JEWELRY", "Supplier 2", "Supplier 3")
department.vector <- c("Department 1", "Department 2")
category.vector <- c("Category 1", "Category 2")
product.vector <- c("Product 1", "Product 2")

shinyUI(pageWithSidebar(
  headerPanel(" "),
  sidebarPanel(
    #h3('Enter Market and Test Date Here'),
    selectInput('id1', 'Supplier', choices = supplier.vector, selected = "ACOMO JEWELRY"),
    selectInput('id2','Department', choices = department.vector),
    selectInput('id3', 'Category', choices = category.vector),
    selectInput('id4', 'Product', choices = product.vector), # needs both description and item number
    dateInput('id5', 'Forecast Start Date'),
    dateInput('id6', 'Forecast End Date'),
    selectInput('id8', 'Select Dollars or Units', choices = c("Dollars", "Units", selected = "Dollars"))
    
    #h5('This dashboard provides multiple views of the sales of the selected supplier.  
    #   It shows whether a supplier is outperforming or underperforming prior years and which categories and products are driving that trend.')
  ),
  mainPanel(
    textOutput("output.text")
  )
  )
)