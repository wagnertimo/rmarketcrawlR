#'
#' Testing script
#'


productType <- 'NEG_NT'
dateFrom <- '13.03.2017'
df <- df_auctions

getTheMeanPowerPrice <- function(df, productType, dateFrom) {

  # filter out the austrian offers and get the specific date as well as product type
  df_temp <- filter(df, product_name == productType & date_from == dateFrom & is.na(offers_AT))

  # Sum up the product of power price and called power of all orders in ratio to the overall called power
  return(sum(df_temp$power_price*df_temp$called_power_MW) / sum(df_temp$called_power_MW))

}


getTheMeanPowerPrice(df, 'NEG_NT', '20.03.2017')





