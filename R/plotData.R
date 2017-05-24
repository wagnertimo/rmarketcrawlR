#'
#' The plotData script
#'
#' @author Timo Wagner, \email{wagnertimo@gmx.de}
#'
#' It contains main functions to provide several plots for analyzing operating reserve data.
#'





#' @title plotXminAVGMWvs4secMW
#'
#' @description This method plots the 1min average of the operating reserve needs, 15min negative and positive needs, as well as the 15min calls against the original 4sec operating reserve needs
#'
#' @param dataframe - data.frame with the operating reserve needs (in MW), the type ("RZBedarf"), a DateTime (POSIXct object) and the averages (1min, 15min neg. and pos.) operating reserve needs and the 15min calls.
#'
#' @return a line chart with the 4sec operating reserve needs (grey)...
#'
#' @examples
#' dataframe <- buildCorrectingCallsDF(df.1min, df.avg.15min.neg, df.avg.15min.pos, df.calls.preprocessed)
#' plotXminAVGMWvs4secMW(dataframe)
#'
#' @export
#'
plotAVGMWvs4secMW <- function(dataframe) {

  library(ggplot2)

  g2 <- ggplot(dataframe, aes(DateTime)) +
    geom_line(aes(y = MW, colour = "MW")) +
    geom_step(aes(y = avg_1min_MW, colour = "avg. 1min needs")) +
    geom_step(aes(y = avg_15min_MW_NEG, colour = "avg. neg. 15min needs")) +
    geom_step(aes(y = avg_15min_MW_POS, colour = "avg. pos. 15min needs")) +
    geom_step(aes(y = neg_MW, colour = "avg. neg. calls")) +
    geom_step(aes(y = pos_MW, colour = "avg. pos. calls")) +
    scale_colour_manual(values = c("#79c5dc", "#fb7474", "#de1b1b", "#77d49c", "#5cb26c", "#ababab")) +
    labs(x = "Date and Time", y = "Power (in MW)") +
    ggtitle('Operating Reserve Needs') +
    theme(plot.title = element_text(size = 20, face="bold", margin = margin(10, 0, 10, 0)),
          axis.title.x = element_text(color="forestgreen", vjust=-0.35),
          axis.title.y = element_text(color="cadetblue" , vjust=0.35)
    ) +
    scale_y_continuous(label = function(x){return(paste( x, " MW"))})

  g2

}






#' @title plotCorrectedNeeds
#'
#' @description This method plots the 1min average of the operating reserve needs, 15min negative and positive needs, as well as the 15min calls against the corrected 1min needs to approximate the 1min calls.
#'
#' @param dataframe - data.frame with the operating reserve needs (in MW), and the averages (1min, 15min neg. and pos.) operating reserve needs and the 15min calls and the corrected needs (variable Corrected).
#'
#' @return a line chart with the averages of operating reserve needs (grey)...
#'
#' @examples
#' dataframe <- approximateCalls(s.needs, s.calls)
#' plotCorrectedNeeds(dataframe)
#'
#' @export
#'
plotCorrectedNeeds <- function(dataframe){

  library(ggplot2)

  g2 <- ggplot(dataframe, aes(DateTime)) +
    # geom_line(aes(y = MW, colour = "MW")) +
    geom_step(aes(y = avg_1min_MW, colour = "avg. 1min needs")) +
    geom_step(aes(y = avg_15min_MW_NEG, colour = "avg. neg. 15min needs")) +
    geom_step(aes(y = avg_15min_MW_POS, colour = "avg. pos. 15min needs")) +
    geom_step(aes(y = neg_MW, colour = "avg. neg. calls")) +
    geom_step(aes(y = pos_MW, colour = "avg. pos. calls")) +
    geom_step(aes(y = Corrected, colour = "Corrected")) +
    scale_colour_manual(values = c("#79c5dc", "#fb7474", "#de1b1b", "#77d49c", "#5cb26c", "#ababab")) +
    labs(x = "Date and Time", y = "Power (in MW)") +
    ggtitle('Operating Reserve Needs') +
    theme(plot.title = element_text(size = 20, face="bold", margin = margin(10, 0, 10, 0)),
          axis.title.x = element_text(color="forestgreen", vjust=-0.35),
          axis.title.y = element_text(color="cadetblue" , vjust=0.35)
    ) +
    scale_y_continuous(label = function(x){return(paste( x, " MW"))})

  g2
}





#' @title plotApproximatedData
#'
#' @description This method plots the 1min average of the operating reserve needs, 15min negative and positive needs, as well as the 15min
#'
#' @param dataframe - data.frame with the averages (15min of neg. and pos.) of the operating reserve needs and the 15min calls and the approximated 1min calls (avg_1min_MW).
#'
#' @return a line chart
#'
#' @examples
#' dataframe <- approximateCallsInRecursion(s.needs, s.calls)
#' plotApproximatedData(dataframe)
#'
#' @export
#'
plotApproximatedData <- function(dataframe){

  library(ggplot2)

  g2 <- ggplot(dataframe, aes(DateTime)) +
    # geom_line(aes(y = MW, colour = "MW")) +
    geom_step(aes(y = avg_1min_MW, colour = "avg. 1min needs")) +
    geom_step(aes(y = avg_15min_MW_NEG, colour = "avg. neg. 15min needs")) +
    geom_step(aes(y = avg_15min_MW_POS, colour = "avg. pos. 15min needs")) +
    geom_step(aes(y = neg_MW, colour = "avg. neg. calls")) +
    geom_step(aes(y = pos_MW, colour = "avg. pos. calls")) +
    scale_colour_manual(values = c("#79c5dc", "#fb7474", "#de1b1b", "#77d49c", "#ababab")) +
    labs(x = "Date and Time", y = "Power (in MW)") +
    ggtitle('Operating Reserve Needs') +
    theme(plot.title = element_text(size = 20, face="bold", margin = margin(10, 0, 10, 0)),
          axis.title.x = element_text(color="forestgreen", vjust=-0.35),
          axis.title.y = element_text(color="cadetblue" , vjust=0.35)
    ) +
    scale_y_continuous(label = function(x){return(paste( x, " MW"))})

  g2
}



#' ------------------------------------------------------------
#' Plotting techniques of marginal work prices, call probabilites etc.
#'







#' @title plotMWPTimeSeries
#'
#' @description This method plots the call probabilites over time (specified by the time range of the input data set) for a given price.
#' The time steps can be defined in minutes. E.g. call probabilities for ever 60 minutes (hourly time series data).
#' The sign of the reserve energy has also to be specified as an input parameter. This is neccessary to distinguish between the product types.
#'
#' @param data - data.frame of the marginal work prices @seealso getMarginalWorkPrices()
#' @param price - define a price
#' @param direction - filters the plot by the direction of the reserve energy. String as "POS" or "NEG"
#' @param granularity - defines the time steps in minutes on the x axis.
#'
#' @return a line chart with time on x axis (depending on the granularity) and call probability on y axis
#'
#' @examples
#' marginalworkprices <- getMarginalWorkPrices(needs, calls, auctions)
#' # Plot the call probability time series for price 55 in a 60min interval over the whole time period (range) in the input data set
#' plotMWPTimeSeries(marginalworkprices, 55, "POS", 60)
#'
#' @export
#'
plotMWPTimeSeries <- function(data, price, direction, granularity){
  library(ggplot2)
  library(scales)

  df <- getTimeSeriesProbForPrice(data, price, c("Tarif", "Direction"), granularity)
  df <- filter(df, Direction == direction)
  df$cuttedTime <- as.POSIXct(df$cuttedTime, tz = "Europe/Berlin")

  plot <- ggplot(df, aes(cuttedTime, Prob, group = 1)) +
    geom_line(colour = "blue") +
    labs(x = "Hours", y = "Call Probability",
         title = paste("Call Probability over time for price", price)) +
    scale_x_datetime(labels = date_format("%H:%M"))

  plot
}

#' Helper function fot the @seealso plotMWPTimeSeries function.
#'
#' It produces the right data set which can than be plotted.
#' condition is only based on Tarif and Direction
#' granularity of time in minutes. E.g. 60 --> 60min leads to hourly data
#'
getTimeSeriesProbForPrice <- function(data, price, conditionOnTarifAndDirection, granularity){

  library(dplyr)

  # split data into the granularity
  # data have to be the 1min approx calls
  data$cuttedTime <- cut(data$DateTime, breaks = paste(granularity, "min", sep = " "))
  #d <- split(data, data$cuttedTime)

  # Get the number of work pries which are less than the given price and based on the conditioned subset
  dots = c("cuttedTime", conditionOnTarifAndDirection)
  d <- data %>%
    group_by_(.dots = dots) %>%
    summarise(totalPriceCount = n(), n = sum(marginal_work_price >= price))

  # Get the total amount of observations based on the conditions (filter/subset) BUT for both NEG and POS directions
  d2 <- data %>%
    group_by_(.dots = dots[-which(dots %in% "Direction")]) %>%
    summarise(n = n())

  rs2 <- left_join(d, d2, by = dots[-which(dots %in% "Direction")], suffix = c(".price",".total"))
  # # add the price to which the call probability belongs to the result data.frame
  rs2$Price <- price
  # # calculate the call probability
  rs2$Prob <- ifelse(round(rs2$n.price/rs2$n.total, digits = 4) == Inf, 0, round(rs2$n.price/rs2$n.total, digits = 4))


  #special(d, price, condition)
  return(rs2)

}





#' @title plotContourCallProb
#'
#' @description This method creates a contour plot (3 dimensions) on the call probability (y axis) and work prices (color scheme) over time.
#'
#' @param data - data.frame
#' @param startPrice - specifies the start price for the price range
#' @param endPrice - specifies the end price for the price range
#' @param direction - filters the plot by the direction of the reserve energy. String as "POS" or "NEG"
#' @param granularityPrice - specifies the price steps within the defined price range (startPrice - endPrice). E.g. 0.1 leads to 0.1, 0.2, 0.3 etc.
#' @param granularityTime - specifies the time steps in minutes over the time period defined by its input data.frame. E.g. 60 leads to hourly time series data.
#' @param numCores - specifies the number of cores used for the parallel computing operation.
#'
#' @return a contour plot with the call probabilities (y axis) for different work prices (color scheme) specified by the price range over a time series.
#'
#' @examples
#' marginalworkprices <- getMarginalWorkPrices(needs, calls, auctions)
#' # Contour plot for call probabilities of work prices (positive reserve energy) between 0 - 80 in whole steps (0,1,2,...,79,80) in an hourly time series data.
#' plotContourCallProb(m, 0, 80, "POS", 1, 60, 2)
#'
#' @export
#'
plotContourCallProb <- function(data, startPrice, endPrice, direction, granularityPrice, granularityTime, numCores){

  library(plotly)

  df <- contourCallProbTimeSeries(data, startPrice, endPrice, granularityPrice, granularityTime, numCores)
  df <- filter(df, Direction == direction)
  df$cuttedTime <- as.POSIXct(df$cuttedTime, tz = "Europe/Berlin")

  p2 <- plot_ly(df, x = ~cuttedTime, y = ~Prob, z = ~Price, type = "contour") %>%
    layout(
      title = "Chart Summary",
      xaxis = list(title="Date", ticks = df$cuttedTime)
    )
  p2

}



#' Helper function for the @seealso plotContourCallProb function.
#' It creates the appropriate data set --> parallel computing
contourCallProbTimeSeries <- function(timeSeriesData, mini, maxi, granularity, timesteps, numCores) {
  library(dplyr)
  library(data.table)
  library(foreach)
  library(doParallel)

  cl <- makeCluster(numCores) #not to overload your computer
  registerDoParallel(cl)

  end <- (ceiling(maxi) - mini) * 1/granularity
  # Calculate for each price within the given price range the call probability specified on a time period and product type (Tarif and Direction)
  df <- foreach(i = 0:end,
                .combine = function(x,y) rbindlist(list(x,y), use.names = TRUE, fill = TRUE),
                .export = c("getTimeSeriesProbForPrice"),
                .packages = c("dplyr"),
                .verbose=FALSE) %dopar% {

                  temp <- getTimeSeriesProbForPrice(timeSeriesData, (mini + i*granularity), c("Tarif", "Direction"), timesteps)
                  temp
                }

  #stop cluster
  stopCluster(cl)

  # convert data.table object back to data.frame
  df <- setDF(df)
  # set the NA values generated by the data.table::rbindlist method --> rbindlist has to be used, otherwise missing combinations get lost
  df[is.na(df)] <- 0

  return(df)
}


#' @title getHighestPriceWithHighestCallProb
#'
#' @description This method creates a data.frame with same variables as the input data.frame but filter for the highest call probabilities with highest price at each time step.
#'
#'
#' @param data - data.frame
#' @param startPrice -
#' @param endPrice -
#' @param direction - filters the plot by the direction of the reserve energy. String as "POS" or "NEG"
#' @param granularityPrice -
#' @param granularityTime -
#' @param numCores -
#'
#' @return data.frame with highest probabilities in Prob column and the corresponding highest price in the Price column for each time step
#'
#' @examples
#' marginalworkprices <- getMarginalWorkPrices(needs, calls, auctions)
#' # Get the data.frame with highest call probability and its highest work price for each time step.
#' # price range from 0 to 80 with whole steps (0,1,2,...,79,80) and hourly time steps (60 minutes)
#' getHighestPriceWithHighestCallProb(m, 0, 80, "POS", 1, 60, 2)
#'
#' @export
#'
getHighestPriceWithHighestCallProb <- function(data, startPrice, endPrice, direction, granularityPrice, granularityTime, numCores) {

  df <- contourCallProbTimeSeries(data, startPrice, endPrice, granularityPrice, granularityTime, numCores)
  df <- filter(df, Direction == direction)
  df$cuttedTime <- as.POSIXct(df$cuttedTime, tz = "Europe/Berlin")

  # get the highest price with highest possible probability for every hour/timestep
  # group by POSIXct object does not work --> method is NOT variable --> get the price range
  range = max(df$Price) - min(df$Price) + 1
  result <- df %>% arrange(cuttedTime, desc(Prob), desc(Price)) %>% filter(row_number() %% range == 1)

  return(result)

}






plotContourHourOfDay <- function(data, price, granularity){

}






