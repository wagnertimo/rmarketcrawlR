#'
#' Script to build models and analysis (plots, tables)
#'


# Auctions Data

# filter by direction
# and get average of the
library(dplyr)

auctions.2016 %>%
  group_by_(.dots = c("date_from", "Direction")) %>%
  summarise(avg = mean(work_price)) %>%
  ggplot(aes(x = date_from, y = avg, colour = Direction)) +
  geom_line()








#' Look at Marginal Work Prices - Descriptive Statistics
library(ggplot2)
library(dplyr)

mwp.2016.pos <- filter(mwp.2016, Direction == "POS")
mwp.2016.neg <- filter(mwp.2016, Direction == "NEG")


summary(mwp.2016.pos$marginal_work_price)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# 29.80    37.50    44.54    59.80    58.50 20000.00
#
# --> For outliers 1.5x IQR +(-) 3rd(1st) Quartile --> IQR(mwp.2016.pos$marginal_work_price) = 21
# --> Upper level: mwp of 90, and lower level mwp of 6
bench.pos.pos <- as.numeric(quantile(mwp.2016.pos$marginal_work_price, probs = c(0.75))) + 1.5*IQR(mwp.2016.pos$marginal_work_price)
bench.pos.neg <- as.numeric(quantile(mwp.2016.pos$marginal_work_price, probs = c(0.25))) - 1.5*IQR(mwp.2016.pos$marginal_work_price)


summary(mwp.2016.neg$marginal_work_price)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
# -22.10    -2.10     1.90    29.57     9.70 74400.00
#
# --> For outliers 1.5x IQR +(-) 3rd(1st) Quartile --> IQR(mwp.2016.pos$marginal_work_price) = 21
# --> Upper level: mwp of 27.4, and lower level mwp of -19.8
bench.neg.pos <- as.numeric(quantile(mwp.2016.neg$marginal_work_price, probs = c(0.75))) + 1.5*IQR(mwp.2016.neg$marginal_work_price)
bench.neg.neg <- as.numeric(quantile(mwp.2016.neg$marginal_work_price, probs = c(0.25))) - 1.5*IQR(mwp.2016.neg$marginal_work_price)


# Visual outlier detection via boxplot
boxplot(mwp.2016.pos[mwp.2016.pos$marginal_work_price<81,]$marginal_work_price, horizontal = T)
boxplot(mwp.2016.neg[mwp.2016.neg$marginal_work_price<14 & mwp.2016.neg$marginal_work_price>-14,]$marginal_work_price, horizontal = T)


# trim away outliers
data.pos <- mwp.2016.pos[mwp.2016.pos$marginal_work_price<=90 & mwp.2016.pos$marginal_work_price>=6,]
# > nrow(data.pos)/nrow(mwp.2016.pos)
# [1] 0.9464051
#
#

data.neg <- mwp.2016.neg[mwp.2016.neg$marginal_work_price<=27.4 & mwp.2016.neg$marginal_work_price>=-19.8,]
# > nrow(data.neg)/nrow(mwp.2016.neg)
# [1] 0.876775
#
#


# show distribution of marginal work prices --> right skewed
ggplot(data = data.pos, aes(x=marginal_work_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), breaks = seq(0, 100, 1),
                 col="green",
                 fill="green",
                 alpha = .5) +
  labs(title="Histogram for Marginal Work Prices with positive Reserve Power in 2016") +
  labs(x="Marginal Work Price", y="Count")

# Violin Plot
ggplot(data.pos, aes(x=Direction, y=marginal_work_price)) +
  geom_violin(trim=F, fill="green", alpha = 0.7) +
  geom_boxplot(width=0.1, fill="grey")

# transformation of skewed data


str(mwp.2016)

ggplot(mwp.2016, aes(x = Direction, y = marginal_work_price, fill = Tarif)) +
  geom_boxplot(alpha=0.75,
               outlier.colour = "#1F3552", outlier.shape = 20,
               notch = TRUE) +
  scale_y_log10(name="Marginal Work Prices \n in log10") +
  ggtitle("Boxplot of Marginal Work Prices in 2016")


ggplot(mwp.2016, aes(x=Direction, y=marginal_work_price)) +
  geom_violin(trim=F)



needs.2015 = getReserveNeeds('01.01.2015', '31.12.2015')
calls.2015 = getReserveCalls('01.01.2015', '31.12.2015', '6', 'SRL')
auctions.2015 = getReserveAuctions('01.01.2015', '31.12.2015', '2')

needs.2014 = getReserveNeeds('01.01.2014', '31.12.2014')
calls.2014 = getReserveCalls('01.01.2014', '31.12.2014', '6', 'SRL')
auctions.2014 = getReserveAuctions('01.01.2014', '31.12.2014', '2')


needs.2013 = getReserveNeeds('01.01.2013', '31.12.2013')
calls.2013 = getReserveCalls('01.01.2013', '31.12.2013', '6', 'SRL')
auctions.2013 = getReserveAuctions('01.01.2013', '31.12.2013', '2')


needs.2012 = getReserveNeeds('01.01.2012', '31.12.2012')
calls.2012 = getReserveCalls('01.01.2012', '31.12.2012', '6', 'SRL')
auctions.2012 = getReserveAuctions('01.01.2012', '31.12.2012', '2')


needs.2011 = getReserveNeeds('01.07.2011', '31.12.2011')
calls.2011 = getReserveCalls('01.07.2011', '31.12.2011', '6', 'SRL')
auctions.2011 = getReserveAuctions('01.07.2011', '31.12.2011', '2')


# Trying to solve bug why approx.calls.2015 cannot calc mwp

# 28 Tage (Feb)     --> 40.320
# 29 Tage (Feb 16)  --> 41.760
# 30 Tage           --> 43.200
# 31 Tage (Mar)     --> 44.580
# 31 Tage           --> 44.640
# 31 Tage (Oct)     --> 44.700

setLogging(FALSE)

auctions.2015 <- getReserveAuctions('01.01.2015', '31.12.2015', '2')

a.1 <- getReserveAuctions('01.12.2015', '03.01.2016', '2')
c.1 <- getReserveCalls('01.12.2015', '31.12.2015', '6', 'SRL')
n.1 <- getReserveNeeds('01.01.2015', '31.12.2015')

ap.x <- approximateCallsInRecursion(n.1, c.1)
ap.x[is.na(ap.x),]

ap.12 <- ap.x

mp.x <- calcMarginalWorkPrices(ap.x, a.1, 2)
mp.x[is.na(mp.x),]



#'
#' Impute the missing values ---> Compare TSO average with 4sec needs 15min avg
#'

# Get call and need data for 2013, 2012, 2011 // Netzregelverbund (6)
needs.2013 = getReserveNeeds('01.01.2013', '31.12.2013')
calls.2013 = getReserveCalls('01.01.2013', '31.12.2013', '6', 'SRL')
needs.2012 = getReserveNeeds('01.01.2012', '31.12.2012')
calls.2012 = getReserveCalls('01.01.2012', '31.12.2012', '6', 'SRL')
needs.2011 = getReserveNeeds('01.07.2011', '31.12.2011')
calls.2011 = getReserveCalls('01.07.2011', '31.12.2011', '6', 'SRL')


# Get call data for TSOs for 2013, 2012, 2011
# 50Hz (4)
calls.2013.4 = getReserveCalls('01.01.2013', '31.12.2013', '4', 'SRL')
calls.2012.4 = getReserveCalls('01.01.2012', '31.12.2012', '4', 'SRL')
calls.2011.4 = getReserveCalls('01.07.2011', '31.12.2011', '4', 'SRL')
# TenneT (2)
calls.2013.2 = getReserveCalls('01.01.2013', '31.12.2013', '2', 'SRL')
calls.2012.2 = getReserveCalls('01.01.2012', '31.12.2012', '2', 'SRL')
calls.2011.2 = getReserveCalls('01.07.2011', '31.12.2011', '2', 'SRL')
# Amprion (3)
calls.2013.3 = getReserveCalls('01.01.2013', '31.12.2013', '3', 'SRL')
calls.2012.3 = getReserveCalls('01.01.2012', '31.12.2012', '3', 'SRL')
calls.2011.3 = getReserveCalls('01.07.2011', '31.12.2011', '3', 'SRL')
# TransnetBW (1)
calls.2013.1 = getReserveCalls('01.01.2013', '31.12.2013', '1', 'SRL')
calls.2012.1 = getReserveCalls('01.01.2012', '31.12.2012', '1', 'SRL')
calls.2011.1 = getReserveCalls('01.07.2011', '31.12.2011', '1', 'SRL')


# sum of TSOs should be equal to Netzregelverbund
# neg_MW pos_MW
sum.2011 <- data.frame(DateTime = calls.2011.4$DateTime)
sum.2011$neg_MW <- calls.2011.1$neg_MW + calls.2011.2$neg_MW + calls.2011.3$neg_MW + calls.2011.4$neg_MW
sum.2011$pos_MW <- calls.2011.1$pos_MW + calls.2011.2$pos_MW + calls.2011.3$pos_MW + calls.2011.4$pos_MW




# Get the array of missing value dates for the Netzregelverbund calls in 2013, 2012, 2011
missingdates.2013 <- calls.2013[is.na(calls.2013$neg_MW), "DateTime"]
missingdates.2012 <- calls.2012[is.na(calls.2012$neg_MW), "DateTime"]
missingdates.2011 <- calls.2011[is.na(calls.2011$neg_MW), "DateTime"]

# check if years have same missing dates ---> All years have different missing dates!
intersect(missingdates.2013, missingdates.2012) # --> different missing dates
intersect(missingdates.2013, missingdates.2011) # --> different missing dates
intersect(missingdates.2012, missingdates.2011) # --> different missing dates


#
# Impute missing calls with TSO data
#
imputed.calls.2013 <- imputeMissingCallsWithTSO(calls.2013, list.tso.2013)
imputed.calls.2012 <- imputeMissingCallsWithTSO(calls.2012, list.tso.2012)
imputed.calls.2011 <- imputeMissingCallsWithTSO(calls.2011, list.tso.2011)

all.equal(imputed.calls.2013,calls.2013)
all.equal(imputed.calls.2012,calls.2012)
all.equal(imputed.calls.2011,calls.2011)

imputed.calls.2013[is.na(imputed.calls.2013$neg_MW), ]
imputed.calls.2012[is.na(imputed.calls.2012$neg_MW), ]
imputed.calls.2011[is.na(imputed.calls.2011$neg_MW), ]



















