library(shiny)
library(data.table)


#need to switch to feather or heather for data loading per Joe Cheng
setwd("C://Users/The Pritchard family/Documents/R/jh_gh/")
wt.data <- read.csv("data/sales.data.cache2019-03-02.csv", stringsAsFactors = FALSE)
# fix this later by mutating data being imported
wt.data$Product <- paste(wt.data$Description, wt.data$Item)
product.menu.df <- read.csv("data/product.menu.df.csv", stringsAsFactors = FALSE)

setDT(wt.data)

#read in functions
exploratory.jh.time.series <- function(clean.df, freq = 365){
  # includes day, week, and year frequency options
  if(freq == 52){print("Error: use freq = 53 for weekly to account for partial week at EOY")}
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step but doing it here too in case not
  start.year <- year(clean.df$Day)[1]
  
  if(freq == 365){
    start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
    sales.agg <- aggregate(Total.Sales ~ Day, clean.df, sum)
    all.dates <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)),by = 1)
    sales.agg.all <- left_join(data.frame(Day = all.dates), sales.agg)
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.day), frequency = 365)
  }
  if(freq == 12){
    start.month <- month(head(clean.df$Month,1))
    sales.agg <- aggregate(Total.Sales ~ Month + Year + Day, clean.df, sum)
    print(head(sales.agg, 25))
    first.of.month.seq <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)), by = "month")
    first.of.month.df <- data.frame("Day" = first.of.month.seq, "Month" = month(first.of.month.seq), "Year" = year(first.of.month.seq))
    sales.agg.all <- left_join(first.of.month.df, sales.agg)
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.month), frequency = 12)
  }
  if(freq == 53){
    clean.df$Week <- week(clean.df$Date.Sold)
    start.week <- head(clean.df$Week,1)
    sales.agg <- aggregate(Total.Sales ~ Week + Year + Day, clean.df, sum)
    print(head(sales.agg, 25))
    first.of.week.seq <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)), by = "week")
    first.of.week.df <- data.frame("Day" = first.of.week.seq, "Week" = week(first.of.week.seq), "Year" = year(first.of.week.seq))
    sales.agg.all <- left_join(first.of.week.df, sales.agg)
    #sales.agg.all <- left_join(sales.agg, data.frame(Week = 1:53))
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.week), frequency = 53)
    
    
  }
  sales.ts[is.na(sales.ts)] <- 0
  
  sales.ts
}

exploratory.jh.time.series.units <- function(clean.df, freq = 365){
  # includes day, week, and year frequency options
  if(freq == 52){print("Error: use freq = 53 for weekly to account for partial week at EOY")}
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step but doing it here too in case not
  start.year <- year(clean.df$Day)[1]
  
  if(freq == 365){
    start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
    sales.agg <- aggregate(Qty.Sold ~ Day, clean.df, sum)
    all.dates <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), 
                          to = as.Date(max(sales.agg$Day, na.rm = TRUE)),by = 1)
    sales.agg.all <- left_join(data.frame(Day = all.dates), sales.agg)
    sales.ts <- ts(sales.agg.all$Qty.Sold, start = c(start.year, start.day), frequency = 365)
  }
  if(freq == 12){
    start.month <- month(head(clean.df$Month,1))
    sales.agg <- aggregate(Qty.Sold ~ Month + Year, clean.df, sum)
    sales.agg.all <- left_join(sales.agg, data.frame(Month = 1:12))
    sales.ts <- ts(sales.agg.all$Qty.Sold, start = c(start.year, start.month), frequency = 12)
  }
  if(freq == 53){
    clean.df$Week <- week(clean.df$Date.Sold)
    start.week <- head(clean.df$Week,1)
    sales.agg <- aggregate(Qty.Sold ~ Week + Year, clean.df, sum)
    sales.agg.all <- left_join(sales.agg, data.frame(Week = 1:53))
    sales.ts <- ts(sales.agg.all$Qty.Sold, start = c(start.year, start.week), frequency = 53)
    
  }
  sales.ts[is.na(sales.ts)] <- 0
  
  sales.ts
}

fit.bfc.jh <- function(ts.obj, model.to.use = "arima", surplus.interval = 80){
  model.options = c("arima", "ets", "stlf", "snaive", "rwf", "mean", "drift","snaive+6mo") # in advanced menu
  require(fpp2)
  if(model.to.use == "arima"){
    ts.fit <- auto.arima(ts.obj)
    fc <- forecast(ts.fit, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "ets"){
    ts.fit <- ets(ts.obj)
    fc <- forecast(ts.fit, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "stlf"){
    fc <- stlf(ts.obj, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "snaive"){
    fc <- snaive(ts.obj, h = frequency(ts.obj), level = surplus.interval)
  }else if(model.to.use == "rwf"){
    fc <- rwf(ts.obj, h = frequency(ts.obj), level = surplus.interval)
  } else if(model.to.use == "mean"){
    # use mean of rolling 12 months
    ts.freq <- frequency(ts.obj)
    fc <- meanf(tail(ts.obj, ts.freq), h = frequency(ts.obj), level = surplus.interval)
  } else if(model.to.use == "drift"){
    fc <- rwf(ts.obj, drift = TRUE, h = frequency(ts.obj), level = surplus.interval)
  } else if(model.to.use == "snaive+6mo"){
    #snaive+6mo calculates the recent growth rate comparing the last 6 mo. of sales with the same period one year earlier.
    #then it multiplies the seasonal naive by that rate.
    ts.freq <- frequency(ts.obj)
    cy.6mo <- tail(ts.obj, ts.freq/2)
    py.6mo <- head(tail(ts.obj, ts.freq*1.5), ts.freq/2) #subset to last 18 mo. then to first 6 mo. of that subset
    growth.rate <- (sum(cy.6mo) - sum(py.6mo))/sum(py.6mo)
    ts.grow <- ts.obj*(1+growth.rate)
    fc <- snaive(ts.grow, h = frequency(ts.obj), level = surplus.interval)
  }
  #fc <- forecast(ts.fit, h = frequency(ts.obj), level = c(80))
  #autoplot(ts.obj) + autolayer(fc)
  fc
}

ts2df.bfc.jh <- function(fc.obj){
  require(lubridate)
  df <- data.frame(fc.obj)
  freq <- nrow(df)
  df$year <- floor(as.numeric(row.names(df)))
  df$freq.increments <- round((as.numeric(row.names(df))-df$year)*freq)+1 # +1 makes it human readable.  "First week of year" = 1
  if(freq==53){
    df$date <- date(paste0(df$year, "-01-01"))+weeks(df$freq.increments-1)
  } else if(freq==365){
    df$date <- date(paste0(df$year, "-01-01"))+(df$freq.increments-1)
  } else if(freq==12){
    df$date <- date(paste0(df$year, "-01-01"))+months(df$freq.increments-1)
  }
  #datefloors <- r
  df
}

plot.bfc.jh <- function(forecast.df){
  require(ggplot2)
  g <- ggplot(data = forecast.df, aes(x = date))+
    geom_line(aes(y = Hi.80), color = "blue", size = 2)+
    geom_line(aes(y = Point.Forecast), color = "green", size = 2)+
    labs(y = "Sales of Selection", x = "Date")+
    scale_y_continuous(labels = scales::dollar)+
    theme_dark()
  g
}

shinyServer(
  function(input, output){
    #output$output.text <- renderText(input$id1)
    
    output$product.list <- renderUI({
      supplier.name <- input$id3
      
      products.from.supplier <- c("ALL PRODUCTS", filter(product.menu.df, Supplier %in% supplier.name))
        #unique(narrow.df.for.menu[narrow.df.for.menu$Market %in% market2, 2])
      #menu.facility.list <- menu.facility.list[order(menu.facility.list)]
      print(head(products.from.supplier, 20))
      selectInput('id4', 'Product', choices = products.from.supplier)
    })
    all.rows <- 1:nrow(wt.data)

      filter1_rows <- reactive({
        if(input$id1 != "ALL DEPTS."){
          wt.data[Department %in% input$id1, which = TRUE]}
        else{all.rows}
      })
      filter2_rows <- reactive({
        if(input$id2 != "ALL CATEGORIES"){
          wt.data[Category %in% input$id2, which = TRUE]}
        else{all.rows}
      })
      filter3_rows <- reactive({
        if(input$id3 != "ALL SUPPLIERS"){
          wt.data[Supplier %in% input$id3, which = TRUE]}
        else{all.rows}
      })
      filter4_rows <- reactive({
        validate(
          need(length(input$id4) > 0, message = FALSE)
        )
        if(length(input$id4) > 0 & input$id4 != "ALL PRODUCTS"){
          wt.data[Product %in% input$id4, which = TRUE]}
        else{all.rows}
      })
      
      #output$mydata <- renderDataTable({
      #  final_rows <- intersect(filter1_rows(), filter2_rows())
      #  final_rows <- intersect(final_rows,     filter3_rows())
      #  iris[final_rows]
      filtered.data <- eventReactive(input$go,{
        final_rows <- intersect(filter1_rows(), filter2_rows())
        final_rows <- intersect(final_rows, filter3_rows())
        
        final_rows <- intersect(final_rows, filter4_rows())
        (wt.data[final_rows])
      }
        
      )
      output$output.text <- renderDataTable({
        #final_rows <- intersect()
        #final_rows <- intersect(filter1_rows(), filter2_rows())
        #final_rows <- intersect(final_rows, filter3_rows())
        #if(length(filter4_rows) > 0){final_rows <- intersect(final_rows, filter4_rows())}
        f.data <- filtered.data()
        head(f.data)
      })
      ts.data <- reactive({
        f.data <- filtered.data()
        if(input$id8 == "Dollars"){
          exploratory.jh.time.series(f.data, freq = 53)}
        else {
          exploratory.jh.time.series.units(f.data, freq = 53)
        }
        })
      forecast.df <- reactive({
        ts.data.to.use <- ts.data()
        fc <- fit.bfc.jh(ts.data.to.use)
        fc.df <- ts2df.bfc.jh(fc)
      })
      
      output$fc.plot <- renderPlot({
        forecast.df2 <- forecast.df()
        plot.bfc.jh(forecast.df2)
      })
        
      #})

    
    #test.text <- filtered.reactive()
    #output$output.text <- renderText(test.text)
      
    #output$output.text <- renderText(dim(filtered.reactive()))
  })