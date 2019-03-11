#this formula generates a forecast using the selected model
#next need to switch back to data frame to enable date filtering again (not possible in ts as far as I know)
# then generate plots
# then generate kable

# alternate way to structure this product would be to generate an R Markdown containing every product from a particular supplier
# the report would generate numbers based on last year for every product

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