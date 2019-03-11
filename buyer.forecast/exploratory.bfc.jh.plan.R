
# components of this web app
# 1. visualization to show the forecast for the specified period
# 2. Table to show requested values for each week or month (or day)
# 3. Input for Department, Category, Supplier, Product to use as filter.  Also, beginning of forecasted period to end date.
# 4. Also way to make informed choice on which inventory level to use, 
# showing the implications for how often out of stock and surplus left at end of period

# Structure?
# First a plot showing forecast including some prior period, maybe all of it?
# Second a table wit the same data but in tabular form
# Third an inventory viualization perhaps with a slider where the implications for surplus and out of stock can be computed

#Logic
#1 import data as streamlined and aggregated as possible (maybe just aggregate identical products sold on same day)
#2 filter data to Supplier, Product, Category, Department using inputs
#3 generate model based on all available data or using window predetermined for that store (i.e. starting 3 years ago)
#4 generate a forecast using that model and using the confidence intervals requested
#5 plot the forecast with the safety rate and the mean.  This will include from h=1 to end of period requested
#6 On a kable, provide mean & safety rate forecasts for requested dates.
#6b Also list cost of surplus inventory and % of out of stock time