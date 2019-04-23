#next steps
#- copy dynamic menus I used for k apps like pioneer generator
#- install go button and make it interact properly


# components of this web app
# 1. visualization to show the forecast for the specified period
# 2. Table to show requested values for each week or month (or day)
# 3. Input for Department, Category, Supplier, Product to use as filter.  Also, beginning of forecasted period to end date.
# 4. Also way to make informed choice on which inventory level to use, 
# showing the implications for how often out of stock and surplus left at end of period

# Structure?
# First a plot showing forecast including some prior period, maybe all of it?
# Second a table with the same data but in tabular form
# Third an inventory visualization perhaps with a slider where the implications for surplus and out of stock can be computed

#Logic
#1 import data as streamlined and aggregated as possible (maybe just aggregate identical products sold on same day)
#2 filter data to Supplier, Product, Category, Department using inputs
#3 generate model based on all available data or using window predetermined for that store (i.e. starting 3 years ago)
#4 generate a forecast using that model and using the confidence intervals requested
#5 plot the forecast with the safety rate and the mean.  This will include from h=1 to end of period requested
#6 On a kable, provide mean & safety rate forecasts for requested dates.
#6b Also list cost of surplus inventory and % of out of stock time

#ui.R
#need menus that interact
#1 Supplier
#2 Department (populates only the departments that supplier has products in, maybe?  I think this is necessary and helpful
#3 Category (populates only the categories that Supplier & Department have)
#4 Product (populates only what's included in what's selected above)

#5 start date (when is current inventory is expected to run out?)
#6 end date (how long does the inventory you're about to order need to last?)

#7 select dollars or units

#8 a Calculate Forecast button

#9 advanced force a model selection (at the bottom of the main panel)

#process for data updates
#need to generate updated product and supplier list for dynamic menu
    #import most recent data set
    #create a new column wt.data$Product <- paste(wt.data$Description, wt.data$Item)
    #product.menu.df <- select(wt.data, Supplier, Product)
    #product.menu.df <- unique(product.menu.df)
    #product.menu.df <- arrange(product.menu.df, Product)
    #write.csv(product.menu.df, "data/product.menu.df.csv", row.names = FALSE)