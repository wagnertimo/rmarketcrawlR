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

# Calculate the 1min average operating reserve needs out of the 4sec data
s.needs.1min <- aggregateXminAVGMW(s.needs, 1)

# Calculate the 15min average operating reserve needs for negative and positive power out of the 1min averages
s.needs.15min.neg <- get15minAVGOff1minAVG(s.needs.1min, "NEG")
s.needs.15min.pos <- get15minAVGOff1minAVG(s.needs.1min, "POS")

# Build up a data.frame with all relevant averages and values to approximate the 1min calls
sample.approx.calls <- buildCorrectingCallsDF(s.needs.1min, s.needs.15min.neg, s.needs.15min.pos, s.calls)



plotAVGMWvs4secMW(sample.approx.calls)


## ----------------

#
# Calculate the corrected needs or approximate the calls. Based on the standard case
#

#' Correction:
#'   standard case for POS 1min avg need: (15min call POS MW -  15min avg POS need MW) * count POS / 15
#'   standard case for NEG 1min avg need: (15min call NEG MW -  15min avg NEG need MW) * count NEG / 15
#'
#' 2 special cases_
#' - Homogenity: only neg or pos needs (homogen need) but there are calls in both direction
#' - XX: there is no call (15min average of neg or pos is 0) but there is at least one 1min avg need in that direction
#'
#'
#'

# 1. aggregate data.frame of 4 sec resolution in 1 min resolution
sample.approx.calls$cuttedTime <- cut(sample.approx.calls$DateTime, breaks = paste("1", "min", sep = " "))
sample.approx.calls$cuttedTime <- as.POSIXct(sample.approx.calls$cuttedTime, tz = "MET")

# Scrape out all the 1min averages of the 4sec data by using unique
df.temp <- unique(sample.approx.calls[, c("cuttedTime","avg_1min_MW", "avg_15min_MW_NEG", "avg_15min_MW_POS", "neg_MW", "pos_MW", "NEG", "POS")])





df.temp$Corrected <- ifelse(df.temp$avg_1min_MW < 0, df.temp$avg_1min_MW + ((df.temp$neg_MW - df.temp$avg_15min_MW_NEG) * (15/df.temp$NEG)), df.temp$avg_1min_MW + ((df.temp$pos_M - df.temp$avg_15min_MW_POS) * (15/df.temp$POS)))

# ajustments to special case
#df.temp$Corrected <- ifelse((df.temp$avg_1min_MW >= 0) | (df.temp$Corrected >= 0), 0, df.temp$Corrected)


# Sanity check 15min averages ==? 15min calls
sum(df.temp[df.temp$Corrected >= 0 & df.temp$cuttedTime < "2015-12-30 00:15:00", ]$Corrected)
sum(df.temp[df.temp$Corrected < 0 & df.temp$cuttedTime >= "2015-12-30 00:15:00", ]$Corrected) / 15




# ------------------


g2 <- ggplot(df.temp, aes(cuttedTime)) +
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







