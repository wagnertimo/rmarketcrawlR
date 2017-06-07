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


# Build for 2013,2012,2011 data.frames with TSO data of the missing value dates --> then impute TSO missing values

library(dplyr)

a <- filter(calls.2011.1, DateTime %in% missingdates.2011)
b <- filter(calls.2011.2, DateTime %in% missingdates.2011)
c <- filter(calls.2011.3, DateTime %in% missingdates.2011)
d <- filter(calls.2011.4, DateTime %in% missingdates.2011) # --> has all values

rr <- list(a,b,c,d) %>%
  Reduce(function(dtf1,dtf2) full_join(dtf1,dtf2,by="DateTime"), .)

# Rename the result columns and drop some useless columns
colnames(rr) <- c("DateTime", "neg_MW.1", "pos_MW.1", "TZ.1","neg_MW.2", "pos_MW.2", "TZ.1", "neg_MW.3", "pos_MW.3","TZ.3", "neg_MW.4", "pos_MW.4", "TZ")
rr <- rr[, !(names(rr) %in% c("TZ.1", "TZ.2", "TZ.3"))]

# count unique na per rows --> if only 2 then only only one TSO has a missing value --> 2013 = always one TSO with NA // 2012 = always one TSO with NA // 2011 = 1 and once 3 TSO with NA
unique(apply(rr, 1, function(x) sum(is.na(x))))

# subset rr into two data.frames with negative and positive MW
rrPos <- rr[, c("DateTime", "TZ", "pos_MW.1", "pos_MW.2", "pos_MW.3", "pos_MW.4")]
rrNeg <- rr[, c("DateTime", "TZ", "neg_MW.1", "neg_MW.2", "neg_MW.3", "neg_MW.4")]
# Then melt each subset into long data
library(reshape2)
rrPos <- melt(rrPos, id.vars=c("DateTime", "TZ"))
rrNeg <- melt(rrNeg, id.vars=c("DateTime", "TZ"))
# Sort by Date to have chronological order and not order by TSO
rrPos <- arrange(rrPos, DateTime)
rrNeg <- arrange(rrNeg, DateTime)
# Rename columns for better access
colnames(rrPos) <- c("DateTime", "TZ", "TSO", "pos_MW")
colnames(rrNeg) <- c("DateTime", "TZ", "TSO", "neg_MW")
# Modify the TSO variable --> just give each TSO a number from 1 - 4
rrPos$TSO <- substr(rrPos$TSO, nchar(as.character(rrPos$TSO)), nchar(as.character(rrPos$TSO)))
rrNeg$TSO <- substr(rrNeg$TSO, nchar(as.character(rrNeg$TSO)), nchar(as.character(rrNeg$TSO)))

# Build a unified data.frame
allTSO.2011 <- cbind(rrNeg, pos_MW = rrPos$pos_MW)
test <- allTSO.2011
# sum neg_MW and pos_MW of the 4 TSO by each DateTime
test = aggregate(cbind(neg_MW, pos_MW) ~ DateTime, data = test, sum, na.rm = TRUE)


# Clean up variable environment
rm(a,b,c,d,rr,rrPos,rrNeg)


# Impute TSO missing values

# Get dates and TSO of na
allTSO.2013[is.na(allTSO.2013$neg_MW), c("DateTime","TZ","TSO")]
allTSO.2012
allTSO.2011

test <- calls.2013.2

test[is.na(test$neg_MW), ]
test <- imputeMissingValueOfTSO(test)

list.tso.2013 <- list(calls.2013.1,calls.2013.2,calls.2013.3,calls.2013.4)
list.tso.2012 <- list(calls.2012.1,calls.2012.2,calls.2012.3,calls.2012.4)
list.tso.2011 <- list(calls.2011.1,calls.2011.2,calls.2011.3,calls.2011.4)

test <- imputeCalls(calls.2013, list.tso.2013)
test <- imputeCalls(calls.2012, list.tso.2012)
test <- imputeCalls(calls.2011, list.tso.2011)


list.tso.2013 <- impute(list.tso.2013)

tso <- 4
calls.2013[is.na(calls.2013$neg_MW), ]


calls.2013[calls.2013$DateTime %in% missingdates.2013, "neg_MW"] <- 0

test$neg_MW



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



# Aggregate needs to 15min avg as approx for 15min call
test <- aggregateXminAVGMW(needs.2011, 15)
# Cut 1min avg needs into 15min for join operation
test <- needs.2011
test$cuttedTime <- cut(test$DateTime, breaks = paste("15", "min", sep = " "))
test$cuttedTime <- as.POSIXct(test$cuttedTime, tz = "Europe/Berlin")

names(test)[names(test)=="MW"] <- "avg_1min_MW"

t.2011.pos <- get15minAVGOf1minAVG(test, "POS")
t.2011.neg <- get15minAVGOf1minAVG(test, "NEG")
library(dplyr)
t.2011 <- full_join(t.2011.neg, t.2011.pos)
t.2011 <- merge(t.2011.pos, t.2011.neg, all = TRUE)

t.2011[is.na(t.2011)] <- 0




















