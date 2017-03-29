#'
#' Testing script
#'

# ------------------------------------------------------------------------------------------------------

getTheMeanPowerPrice <- function(df, productType, dateFrom) {

  # filter out the austrian offers and get the specific date as well as product type
  df_temp <- filter(df, product_name == productType & date_from == dateFrom & is.na(offers_AT))

  # Sum up the product of power price and called power of all orders in ratio to the overall called power
  return(sum(df_temp$power_price*df_temp$called_power_MW) / sum(df_temp$called_power_MW))

}

getTheMeanPowerPrice(df, 'NEG_NT', '20.03.2017')

# ------------------------------------------------------------------------------------------------------

#
# TODO: ADD THE SPECIAL CASE OF HOMOGENITY. LOOK IF CALLS ARE POSITIVE (OR NEGATIVE) BUT NEEDS ARE ALL NEGATIVE (OR POSITIVE)
#       --> add a new variable 1 or 0 special case (true or false). And if true, add the closes MW
#

# ----------------------------------------------------------------------------------------

# Get the operating reserve needs and calls as well as the weekly auctions
# Raw data from regelleistung.net and transnetbw.de (4sec needs)
df.main.needs <- getOperatingReserveNeeds("30.12.2015", "30.12.2015")
df.main.calls <- getOperatingReserveCalls('30.12.2015', '30.12.2015', '6', 'SRL')
df.main.auctions <- getOperatingReserveAuctions('23.12.2015', '30.12.2015', '2')

# Preprocess and format the calls and need data
df.main.calls.preprocessed <- preProcessOperatingReserveCalls(df.main.calls)
df.main.needs.preprocessed <- preprocessOperatingReserveNeeds(df.main.needs)

# get a sample of the call and needs data -> the first 30mins
s.needs <- df.main.needs.preprocessed[1:450,]
s.calls <- df.main.calls.preprocessed[1:2,]


# Build up a data.frame with all relevant averages and values to approximate the 1min calls
# r <- buildCorrectingCallsDF(s.needs, s.calls)
# plotAVGMWvs4secMW(r)

#
# Calculate the corrected needs or approximate the calls. Based on the standard case
#
df.corrected <- approxOperatingReserveCalls(s.needs, s.calls)
# plotCorrectedNeeds(df.corrected)


# Sanity check 15min averages ==? 15min calls
sum(df.corrected[df.corrected$Corrected >= 0 & df.corrected$cuttedTime < "2015-12-30 00:15:00", ]$Corrected) / 15
sum(df.corrected[df.corrected$Corrected < 0 & df.corrected$cuttedTime >= "2015-12-30 00:15:00", ]$Corrected) / 15

#
# Try to merge/join the 4sec data (s.needs) with the minutely corrected data (df.corrected)
#




# ------------------------------------------------------------------------------------------


#'
#' Two special cases:
#' 1. Homogenity: only negative (positive) needs (homogenic needs) but with positive (negative ) calls (in both direction)
#' 2. CrossingZero: With the correction negative (positive) needs are too much corrected and go positive (negative)
#'

# Analzye the occurence of Case 2 within a longer time period
sample.needs <- preprocessOperatingReserveNeeds(getOperatingReserveNeeds("30.12.2015", "30.12.2015"))
sample.calls <- preProcessOperatingReserveCalls(getOperatingReserveCalls('30.12.2015', '30.12.2015', '6', 'SRL'))

# Calculate the corrected needs or approximate the calls. Based on the standard case
sample.corrected <- approxOperatingReserveCalls(sample.needs, sample.calls)

attach(sample.corrected)

# count the switch from positive to negative
count(df.corrected[avg_1min_MW >= 0 & Corrected < 0,])
# count the swith from negative to positive
count(df.corrected[avg_1min_MW < 0 & Corrected >= 0,])






