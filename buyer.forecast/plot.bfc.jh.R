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