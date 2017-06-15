#'
#' Testing script to analyze errors and build new functions
#'


#'------------------------------------------------------------------------------------------------------

#' This function is not yet needed
#'

getTheMeanPowerPrice <- function(df, productType, dateFrom) {

  # filter out the austrian offers and get the specific date as well as product type
  df_temp <- filter(df, product_name == productType & date_from == dateFrom & is.na(offers_AT))

  # Sum up the product of power price and called power of all orders in ratio to the overall called power
  return(sum(df_temp$power_price*df_temp$called_power_MW) / sum(df_temp$called_power_MW))

}

getTheMeanPowerPrice(df, 'NEG_NT', '20.03.2017')

#'------------------------------------------------------------------------------------------------------
#' Testing the production script file: minScript.R

auctions.2016 <- getReserveAuctions('01.01.2016', '31.12.2016', '2')
calls.2016 <- getReserveCalls('01.01.2016', '31.12.2016', '6', 'SRL')
needs.2016 <- getReserveNeeds('01.01.2016', '31.12.2016')

auctions <- getReserveAuctions('24.10.2016', '30.10.2016', '2')
calls <- getReserveCalls('30.10.2016', '30.10.2016', '6', 'SRL')
needs <- getReserveNeeds('30.10.2016', '30.10.2016')

approx <- getOneMinuteCalls(needs,calls)

# sample the 2016 data
start <- 1  # start observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)
end <- 672   # end observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)

needs <- needs.2016[(((start - 1)*225) + 1):(end*225),]
calls <- calls.2016[start:end,]
auctions <- auctions.2016

approx.calls <- getOneMinuteCalls(needs,calls)

setLogging(FALSE)
setLogging(TRUE)


# Start 18:05 -> stuck at Cut time -> 18:09 -> calc avg, cut 15min, split 15min sections ... went for run
#mwork <- getMarginalWorkPrices(needs.2016,calls.2016,auctions.2016)

# 10 days (sample of 01.01.2016 - 10.01.2016) parallel for margin calc. with 2 cores --> exe time:  135sec = 2,25min --> ca. 90min

# Old laptop with 2 cores execution time for 10 days (sample of 01.01.2016 - 10.01.2016)
# --> first part: 1min call approximation => ca. 6mins --> for 365 days => ca. 4h
# --> second part: marginal work price computation (parallel comp.) => ca. 1,5min for 365 days => ca. 1h
# estimated total time for 2016 => ca. 5-6h


start.time <- Sys.time()
m <- getMarginalWorkPrices(needs,calls,auctions,2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

mwork <- getMarginalWorkPrices(needs,calls,auctions)

# on old laptop 10 days (sample of 01.01.2016 - 10.01.2016) took 13 minutes --> 365/10*13/60 = 8h
# Looks more like one obs one sec --> 365*24*60/60/60 = 146h = 6d

system.time(getMarginalWorkPrices(needs,calls,auctions,2))



# start.time <- Sys.time()
# mwork <- getMarginalWorkPrices(needs,calls,auctions)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken



#'------------------------------------------------------------------------------------------------------
#
# !!! CAUTION Infinity loop on 2016-10-30 --> daylight savings
#
#       DONE --> but also check the dst flag of POSIXct object maybe this also works --> http://stackoverflow.com/questions/13156836/character-posixct-conversion-in-r-causes-wrong-timezone-values-on-daylight-savin?noredirect=1&lq=1
#
#   ---> BUT NEW ERROR SEEN --> pos_MW in calls do not format correctly they miss the point as delimiter where a comma was
#
#   ----> DONE
#
#'------------------------------------------------------------------------------------------------------


# -----------------------------------------------------------------------------------------------------
#'
#' TODO: IMPROVE THE CRAWLING FUNCTION. SOMETIMES ERROR OCCURS.
#'       ---> Error occurs for operating reserve needs (e.g. on 01.2017, 11.2016 and 04.2016) due to the unziped file name
#'       ---> the date code (e.g. 201701) is missing in the file name so there is just "SRL_Bedarf.csv"
#'
#'       ---> CHANGE CRAWLER FUNCTION: download file, unzip it, change unziped file name to dateCode and then read in csv
#'
#'       DONE
#'
#'------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------
# TODO: calls seem to have some NA (or - ) for the variable df.calls$BETR..NEG. That's why everything is NA
#       ---> filter for the NA and find a solution how to handle this
#
#       DONE --> needed to format the BETR..NEG and BETR..POS variable from factor to numeric with converting the german number style
#




# -----------------------------------------------------------------------------------------------------
#'
#' Two special cases:
#' 1. Homogenity: only negative (positive) needs (homogenic needs) but with positive (negative) calls (in both direction)
#' 2. CrossingZero: With the correction negative (positive) needs are too much corrected and go positive (negative)
#'
#'------------------------------------------------------------------------------------------------------

#'
#' 1. special case: Homogenity
#'
#' --> example for negative homogentity (and positive call) "01.01.2016 08:00:00" - "01.01.2016 08:15:00", sample.corrected[481:496,]
#'                                                          "01.01.2016 00:00:00" - "01.01.2016 00:15:00", sample.corrected[1:15,]
#'
#' --> example for positive homogentity (and negative call) "2016-01-01 00:30:00" - "2016-01-01 00:45:00", sample.corrected[30:45,]
#'

# -------------
# DONE
# -------------

# --------------------------------
#' 2. special case: CrossingZero
#'
#' Occurs e.g. 2016-01-01 12:15:00 -2016-01-01 12:30:00 --> 50th obs of the 15min calls of 01.01.2016
#' --> neg homogenity but second smallest absolute value crosses zero with correction
#'
#' Recursive approach:
#' Crossers keep their new value and new counts of NEG and POS and new calculations of avg_15min_MW_NEG and avg_15min_MW_POS are calculated
#' Recursion stops when those two averages match the neg_MW and pos_MW (rounding and/or computational errors)
#'
#' test sample:
#' look in code snippet below (DETECT CORSSINGZERO CASES) to identify the crossers and set a sample
#' e.g. start = end 61 --> negative CZ 2 times
#'


# -------------
# DONE
# -------------


# ------------------------------------------------------------------------------------------
# DETECT CORSSINGZERO CASES

# Count for every 15min section the number of crossings
# r has to be a data.frame in the type after calling the old approximate Call function with the Corrected colum and Homogenity case
r <- countCrossingZeros(r)
# mark negative and positive crossings
r <- markCrossingZero(r)

# subset all negative and positive crossings (crossings of smallest absolute values in a homogen 15min section are not included)
# list all crossings --> datetime (1min), avg_1min_MW, Corrected, neg_CZ, pos_CZ
library(dplyr)

# subset all negative and positive crossings
crossings.all <- select(filter(r, neg_CZ == 1 | pos_CZ == 1), DateTime, avg_1min_MW, Corrected, neg_CZ, pos_CZ, n_neg_CZ, n_pos_CZ)
# subset only negative CZ
crossings.neg <- select(filter(r, neg_CZ == 1), DateTime, avg_1min_MW, Corrected, neg_CZ, n_neg_CZ)
# subset only positive CZ
crossings.pos <- select(filter(r, pos_CZ == 1), DateTime, avg_1min_MW, Corrected, pos_CZ, n_pos_CZ)

# set the element (observation) of the data.frame which has to be analyzed
d <- crossings.neg[3,]

# Plot a 15min section of the filtered data.frame which contains a CrossingZero element (1min observation)
plotCorrectedNeeds(filter(r, cuttedTime == get15minSection(d$DateTime)))




# --------------------------------------------------------------------------------
# Sample

s.n <- df.prep.needs.2016
s.c <- df.prep.calls.2016
s.a <- df.prep.auctions.2016

# Choose a sample.
# Look in s.c, the 15min calls of the day (e.g. 01.01.2016) and get the observation number
start <- 1  # start observation number of 15min calls (--> 49*15/60 gives the hour of the day)
end <- 96   # end observation number of 15min calls (--> 49*15/60 gives the hour of the day)

# The calculation chooses automatically the corresponding 4sec needs data to the chosen calls observation.
df.needs <- s.n[(((start - 1)*225) + 1):(end*225),]
df.calls <- s.c[start:end,]


# --------------------------------------------------------------------------------
# Approximate Old --> Only homogenity as a special cases is handled. NO ZEROCROSSING till now

# Approximate the calls
r <- approximateCalls(df.needs, df.calls)

# Sanity checks
# 15min averages ==? 15min calls
sum(r[r$Corrected >= 0 & r$DateTime < "2016-01-01 15:15:00", ]$Corrected) / 15
# Plot the correction data to compare its result
plotCorrectedNeeds(r)


# --------------------------------------------------------------------------------
# Approximate New --> At this time all cases are handled (Homogenity and CrossingZero)

# Approximate the calls WITH RECURSION
t <- approximateCallsInRecursion(df.needs, df.calls)

# Sanity checks
# 15min averages ==? 15min calls
sum(t[t$avg_1min_MW < 0 & t$DateTime < "2016-01-01 15:15:00", ]$avg_1min_MW) / 15
sum(t[t$avg_1min_MW < 0 & t$DateTime >= "2016-01-01 15:15:00", ]$avg_1min_MW) / 15
# This plot does not use the variable Corrected since it is omitted in the recursion
plotApproximatedData(t)
# Here a merge with the old approximation function
joint <- merge(r[,!(names(r) %in% c("Corrected"))], select(t, DateTime,Corrected = avg_1min_MW))
plotCorrectedNeeds(joint)
plotCorrectedNeeds(r)



#'------------------------------------------------------------------------------------------------------
#'
#' Testing marginal work price
#'
#'------------------------------------------------------------------------------------------------------

#' marginal work price == the highest bidded work price to which an order (depending on needed MW) gets filled
#'
#' For SRL there are weekly bids. So those bids last only for that weekly timeframe.
#'
#' The matching of the approx. 1min calls with the auctions depends on:
#'      - the timeframe:           the DateTime of the 1min approx call has to fit into the timeframe of the auctions date_from and date_to
#'      - the product:             the 1min approx. call's Tarif has to fit the auction's one --> HT_NEG, NT_NEG, HT_POS, NT_POS
#'      - the corresponding power: the neg. or pos. power needed (minus or plus (matches with NEG or POS Direction of the auction) avg_1min_MW)
#'            - ordered auctions on work price
#'            - summing all auctions till needed power is reached
#'            - last auction's work price == marginal work price for that minute
#'
#'
#' !!! QUESTION !!! --> if needed power is e.g. 1200,783 MW (roundings because of avg 1min calculation)
#'                      --> and last cumulated bid is of power 1200
#'                      --> will the next bid/order get filled with the rest portion of 0,783 or do we ceil the avg_1min_MW????

# --------------------
# DONE
# --------------------


#'------------------------------------------------------------------------------------------------------
#'
#' Testing: Calculation of call probability for each possible marginal work price in data set (get min (for negative work prices) and max in data.frame)
#'
#'------------------------------------------------------------------------------------------------------


# There are probs. for NEG_NT, NEG_HT and POS_NT, POS_HT
# Beginning at a value below the min will lead to 100% call probability !! Be aware of different tarifs

library(dplyr)
library(lubridate)

mwork.parallel$DateClass <- ifelse(isWeekendOrHoliday(mwork.parallel$DateTime) == TRUE, "Weekend", "Workday")

isWeekendOrHoliday <- function(dateTime) {
  library(lubridate)
  library(rmarketcrawlR)

  # week days 2 == monday ... 1 == sunday
  return(ifelse(wday(dateTime) == 7 | wday(dateTime) == 1 | isGermanHoliday(dateTime) == TRUE, TRUE, FALSE))
}


# Optimize getCallProbDataSet() function --> user can choose in array c() which columns of input data.frame should be used for the
# conditioned calculation of the call probabilities. IMPORTANT: the columns/variables should be factors or at least factorizable!!!!!!
# E.g: conditionen by Tarif, Direction and Weekend columns
#       --> output will be: columns for each combination (--> NEG_POS_Weekend or HT_NEG_Workday) and rows are the respective call probability with the price
#


# get the probability vector of the min max sequence
# granularity factor
tr <- getCallProbDataSetOnConditions(m, 1, 0, 100, 0.1, conditionByColumns = c("Tarif", "Direction"))


# seq 0 - 755
# granularity e.g. 0.1 --> 0, 0.1, 0.2, ... 1.0, 1.1, ..., 754.8, 754.8, 755.0 --> n = (755 - 0)*1/granularity + 1




# Plot multiple variables (value) against one target variable (key). The target has to be omitted for the values (2:...)
library(ggplot2)

tt <- tr
plot <- tt %>%
  # Binds rowwise. Every following column gets bind under the last row. The key variabel (here Price) gets repeated
  gather(key, value, 2:ncol(tt)) %>%
  ggplot(aes(x=Price, y=value, colour=key)) +
  geom_line()

plot

m <- filter(mwork.parallel, DateTime >= "2016-01-01 01:00:00" & DateTime < "2016-01-01 15:00:00")
m <- filter(mwork.parallel, DateTime >= "2016-01-05 15:00:00" & DateTime < "2016-01-05 22:00:00")

qplot(m$DateTime,m$marginal_work_price, geom = "line")
qplot(m$DateTime,m$approx_1min_call, geom = "line")

ggplot(m, aes(DateTime)) +
  geom_line(aes(y = marginal_work_price, colour = "marginal work price")) +
  geom_line(aes(y = approx_1min_call, colour = "1min call"))




# Contour Plot with plotly

#'
#' Carpet-Plot „Grenzarbeitspreis“:
#'       X=Tag
#'       Y=Minute des Tages
#'       Grenzarbeitspreis als Farbe (günstigster=grün; teuerster=rot)
#'       --> Clusterung von ähnlichen Abrufen basierend auf Grenzarbeitspreis
#'
#'
#' Idee Graph:
#'        Arbeitspreis fest (z.B. 100 EUR), x=Tag, y=Abrufwahrscheinlichkeit
#'
#' als Carpet-Plot „Abrufwahrscheinlichkeit“:
#'         X = Tag (Minute?)
#'         Y = Abrufwahrscheinlichkeit
#'         Arbeitspreis als Farbe (günstigster=grün; teuerster= rot)
#'         --> Clusterung von Arbeitspreisen mit ähnlicher Abrufwahrscheinlichkeit
#'
#' http://www.netzfrequenzmessung.de/IEWT_Netzfrequenz_Stabilitaet_Paper.pdf, Abb. 11
#'


# Checkout max min of work price of whole data set
range(m$marginal_work_price)

m.mwp.min = min(m$marginal_work_price)
m.mwp.max = max(m$marginal_work_price)


plotMWPTimeSeries(mwp.2016, 20, "POS", 1440)

m3 <- filter(mwp.2016, format(DateTime, "%Y-%m") <= format(as.Date("2016-03-01"), "%Y-%m"))

#'   PLOTLY --> CONTOUR PLOT
week <- filter(mwp.2016, as.Date(format(DateTime, "%Y-%m-%d")) >= as.Date("2016-06-01") & as.Date(format(DateTime, "%Y-%m-%d")) <= as.Date("2016-06-07"))


plotContourCallProb(week, 30, 80, "POS", 1, 60, 2)

pp <- plotContourHourOfDay(mwp.2016, "POS", 60, "contour")
pp

r <- getHighestPriceWithHighestCallProb(m, 0, 80, "POS", 1, 60, 2)

#'
#' TODO Plot hourly total MW power of minutely calls
#'
week$cuttedTime <- cut(week$DateTime, breaks = paste("60", "min", sep = " "))
week$cuttedTime <- as.POSIXct(week$cuttedTime, tz = "Europe/Berlin")
# Get the number of work pries which are less than the given price and based on the conditioned subset
library(dplyr)
library(tidyr)

d <- week %>%
  group_by_(.dots = c("cuttedTime", "Direction")) %>%
  summarise(totalMW = sum(approx_1min_call)) %>%
  spread(Direction, totalMW) %>%
  ggplot(aes(cuttedTime, POS)) +
        geom_line(colour = "blue") +
        labs(x = "Time", y = "Positive Secondary Reserve Calls in MW", title = paste("Hourly Positive Secondary Reserve Calls in MW")) +
        scale_x_datetime(labels = date_format("%Y-%m-%d %H:%M"))

d



#
# Infinity while loop for recursion on 2011-07-26 11:15:00
#
# Values Jumping: -14641.545 vs. 15min NEG calls: -976.103 --> 0 vs. 15min NEG calls: -976.103
# Values: Jumping: 0 vs. 15min POS calls: 0.38 --> 5.7 vs. 15min POS calls: 0.38
#
# --> observation: only 0 values for that 15min interval
#
# BUG in getNumberOfPosOrNegIn15min --> the function counts 0 NEG and 0 POS
#
#
# needs.2011 data 0 values replaced with data of 2010
#
# DONE !!!
#


#
# Bug: 2012: approximateCallsInRecursion(needs.2012, imputed.calls.2012)
#
# DONE !!!



#
# Bug: 2013, 2012, 2011 calculate marginal work price leads to error imply differing row number
#
# split approx calls by month and test calc marginal work price to get closer to the error
#
# --> guess: it has to do something with the auctions for 2011,2012 tail() revealed that last days were missing
#
# DONE !!!
#


#
# Show 1min call approximation --> graphics
#
# ------------------------------------------------------------------------------------------------------------
setLogging(TRUE)

# get sample
# sample the 2016 data
start <- 2  # start observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)
end <- 2   # end observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)

needs <- needs.2016[(((start - 1)*225) + 1):(end*225),]
calls <- calls.2016[start:end,]
auctions <- auctions.2016

# get end result of correction
ap <- approximateCallsInRecursion(needs, calls)

r <- merge(needs, ap, by.x=c("cuttedTime"), by.y=c("DateTime"))
names(r)[names(r) == "avg_1min_MW"] <- "Corrected"
names(r)[names(r) == "TZ.x"] <- "TZ"
r <- dropColsInDF(r, c("cuttedTime", "TZ.y"))
r2 <- aggregateXminAVGMW(r, "1")
r$cuttedTime <- cut(r$DateTime, breaks = paste("1", "min", sep = " "))
r$cuttedTime <- as.POSIXct(r$cuttedTime, tz = "Europe/Berlin")
r <- merge(r, r2, by.x=c("cuttedTime"), by.y=c("DateTime"))

rm(r2)


# Calculating the approximation step by step (manually to see whats going on) --> simulating the functions of the package
# -----

# Calculate the 1min average operating reserve needs out of the 4sec data
df.needs.1min <- aggregateXminAVGMW(needs, 1)
df.needs.1min$cuttedTime <- cut(df.needs.1min$DateTime, breaks = paste("15", "min", sep = " "))
df.needs.1min$cuttedTime <- as.POSIXct(df.needs.1min$cuttedTime, tz = "Europe/Berlin")

t.all = merge(df.needs.1min, calls, by.x=c("TZ", "cuttedTime"), by.y=c("TZ", "DateTime"))


t.all$pos_neg_rel <- t.all$pos_MW / t.all$neg_MW


# Join with 15min calls
# Cut 1min avg needs into 15min for join operation
needs$cuttedTime <- cut(needs$DateTime, breaks = paste("1", "min", sep = " "))
needs$cuttedTime <- as.POSIXct(needs$cuttedTime, tz = "Europe/Berlin")
needs$TZ <- ifelse(needs$TZ == needs[1, "TZ"], 0, 1)
df.needs.1min$TZ <- ifelse(df.needs.1min$TZ == df.needs.1min[1, "TZ"], 0, 1)
t.needs = merge(needs, df.needs.1min, by.x=c("TZ", "cuttedTime"), by.y=c("TZ", "DateTime"))

t.needs$cuttedTime <- cut(t.needs$DateTime, breaks = paste("15", "min", sep = " "))
t.needs$cuttedTime <- as.POSIXct(t.needs$cuttedTime, tz = "Europe/Berlin")
calls$TZ <- ifelse(calls$TZ == calls[1, "TZ"], 0, 1)
t.all = merge(t.needs, calls, by.x=c("TZ", "cuttedTime"), by.y=c("TZ", "DateTime"))

df.needs.1min$cuttedTime <- cut(df.needs.1min$DateTime, breaks = paste("15", "min", sep = " "))
df.needs.1min$cuttedTime <- as.POSIXct(df.needs.1min$cuttedTime, tz = "Europe/Berlin")
t <- getNumberOfPosOrNegIn15min(df.needs.1min)
t <- merge(df.needs.1min, t, by = "cuttedTime")

t <- get15minAVGs(t)
t <- unique(t[, !(names(t) %in% c("DateTime","avg_1min_MW"))])
names(t)[names(t) == "cuttedTime"] <- "DateTime"

t.all = merge(t.all, t, by.x=c("TZ", "cuttedTime"), by.y=c("TZ", "DateTime"))

rm(t, df.needs.1min, t.needs)




# Correction
t.all$cuttedTime2 <- cut(t.all$DateTime, breaks = paste("1", "min", sep = " "))
t.all$cuttedTime2 <- as.POSIXct(t.all$cuttedTime2, tz = "Europe/Berlin")
t.all <- unique(t.all[,5:12])
names(t.all)[names(t.all) == "cuttedTime2"] <- "DateTime"

# optional to show special case --> Use this block if there is homogenity
# ----
t.all$cuttedTime <- cut(t.all$DateTime, breaks = paste("15", "min", sep = " "))
t.all$cuttedTime <- as.POSIXct(t.all$cuttedTime, tz = "Europe/Berlin")
t <- calcHomogenityCorrectness(t.all)
# !!!!!! --> ONLY use this line for the next recursions
t <- calcHomogenityCorrectness(t) # !!!!!! --> ONLY use this line for the next recursions

t <- dropColsInDF(t, c("avg_15min_MW_NEG", "avg_15min_MW_POS"))
t <- get15minAVGs(t)
t <- correctionCalculationForRecursion(t)
names(t)[names(t) == "avg_1min_MW"] <- "Corrected"
t <- cbind(t, avg_1min_MW = t.all$avg_1min_MW)

tc = merge(needs, t, by.x=c("cuttedTime"), by.y=c("DateTime"))

# To simulate another recursion delete some columns (have to be newly computed) and rename corrected
t <- dropColsInDF(t, c("avg_1min_MW", "Homo_NEG", "Homo_POS"))
names(t)[names(t) == "Corrected"] <- "avg_1min_MW"
# --> start at line 501 (t <- calcHomogenityCorrectness(t.all)) again but as input use t

# ----


# normal case again
t.all$Corrected = ifelse(t.all$avg_1min_MW < 0, t.all$avg_1min_MW + ((t.all$neg_MW - t.all$avg_15min_MW_NEG) * (15/t.all$NEG)),
                         t.all$avg_1min_MW + ((t.all$pos_MW - t.all$avg_15min_MW_POS) * (15/t.all$POS)))
t.all = merge(needs, t.all, by.x=c("cuttedTime"), by.y=c("DateTime"))



# Plot the 1min call approximations with:
# 4sec reserve needs, 1min/15min (pos and neg) avg reserve needs, pos/neg reserve calls and the corrected(appprox) value
#----
ggplot(r, aes(DateTime)) +
  geom_line(aes(y = MW, colour="4s reserve need", linetype = "4s reserve need")) +
  geom_step(aes(y = avg_1min_MW, colour="1min reserve need", linetype = "1min reserve need")) +
  geom_step(aes(y = avg_15min_MW_NEG, colour="Neg. 15min reserve need", linetype = "Neg. 15min reserve need")) +
  geom_step(aes(y = avg_15min_MW_POS, colour="Pos. 15min reserve need", linetype = "Pos. 15min reserve need")) +
  geom_step(aes(y = neg_MW, colour="Neg. reserve call", linetype = "Neg. reserve call")) +
  geom_step(aes(y = pos_MW, colour="Pos. reserve call", linetype = "Pos. reserve call")) +
  geom_step(aes(y = Corrected, colour="Approx. 1min call", linetype = "Approx. 1min call")) +
  scale_colour_manual(name = "Legend:", values = c("1min reserve need" = "#515a7b",
                                 "4s reserve need" = "#b6b6b6",
                                 "Approx. 1min call" = "#515a7b",
                                 "Neg. 15min reserve need" = "#fc3927",
                                 "Neg. reserve call" = "#fc3927",
                                 "Pos. 15min reserve need" = "#56c871",
                                 "Pos. reserve call" = "#56c871")) +
  scale_linetype_manual(guide = FALSE, values=c("Neg. reserve call" = 2,
                                 "Pos. reserve call" = 2,
                                 "Approx. 1min call" = 2,
                                 "4s reserve need" = 1,
                                 "1min reserve need" = 1,
                                 "Neg. 15min reserve need" = 1,
                                 "Pos. 15min reserve need" = 1)) +
  labs(x = "Time", y = "Power (in MW)") +
  ggtitle('Approximated 1min Reserve Calls') +
  theme(plot.title = element_text(size = 20, face="bold", margin = margin(10, 0, 10, 0)),
        axis.title.x = element_text(color="black", vjust=-0.35),
        axis.title.y = element_text(color="black" , vjust=0.35)
  ) +
  scale_y_continuous(label = function(x){return(paste( x, " MW"))}) +
  theme_bw() +
  # center title
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 2, 1, 2, 1, 2))))

# ----






# ------------------------------------------------------------------------------------------------------------
















