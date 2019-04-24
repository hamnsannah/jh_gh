library(shiny)

#need to switch to feather or heather for data loading per Joe Cheng
setwd("C://Users/The Pritchard family/Documents/R/jh_gh/")
wt.data <- read.csv("data/sales.data.cache2019-03-02.csv", stringsAsFactors = FALSE)
# fix this later by mutating data being imported
wt.data$Product <- paste(wt.data$Description, wt.data$Item)
product.menu.df <- read.csv("data/product.menu.df.csv", stringsAsFactors = FALSE)


shinyServer(
  function(input, output){
    output$output.text <- renderText(input$id1)
    
    output$product.list <- renderUI({
      supplier.name <- input$id3
      
      products.from.supplier <- c("ALL PRODUCTS", filter(product.menu.df, Supplier %in% supplier.name))
        #unique(narrow.df.for.menu[narrow.df.for.menu$Market %in% market2, 2])
      #menu.facility.list <- menu.facility.list[order(menu.facility.list)]
      print(head(products.from.supplier, 20))
      selectInput('id4', 'Product', choices = products.from.supplier)
    })
    output$output.text <- renderText({
    #filtered.reactive <- reactive({
      data.for.model <- wt.data
      dept.filter <- input$id1
      cat.filter <- input$id2
      supplier.filter <- input$id3
      product.filter <- input$id4
      
      if(dept.filter != "ALL DEPTS."){
        data.for.model <- filter(wt.data, Department == dept.filter)
      } else if(cat.filter != "ALL CATEGORIES"){
        data.for.model <- filter(wt.data, Category == cat.filter)
      } else if(supplier.filter != "ALL SUPPLIERS"){
        data.for.model <- filter(wt.data, Supplier == supplier.filter)
      } else if(product.filter != "ALL PRODUCTS"){
        data.for.model <- filter(wt.data, Product == product.filter)
      }
      #data.for.model <- filter(wt.data, Department == , Supplier == supplier.name)
    #})
    dim(data.for.model)
    })
    #test.text <- filtered.reactive()
    #output$output.text <- renderText(test.text)
      
    #output$output.text <- renderText(dim(filtered.reactive()))
  })