#'
#' Script to build models and analysis (plots, tables)
#'

# Get Data Part
# ------------------------------------------------------------------------------------------------------------------------------------------------


needs.2016 = getReserveNeeds('01.01.2016', '31.12.2016')
calls.2016 = getReserveCalls('01.01.2016', '31.12.2016', '6', 'SRL')
auctions.2016 = getReserveAuctions('01.01.2016', '31.12.2016', '2')

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

needs.2017.05 = getReserveNeeds('01.01.2017', '31.05.2017')
calls.2017.05 = getReserveCalls('01.01.2017', '31.05.2017', '6', 'SRL')
auctions.2017.05 = getReserveAuctions('01.01.2017', '31.05.2017', '2')


mwp.2011.2016 = read.csv("../../Data/mwp.2011.2016.csv", sep = ",", dec = ".", header = TRUE)
mwp.2011.2016$DateTime = as.POSIXct(mwp.2011.2016$DateTime)
mwp.2011.2016 = mwp.2011.2016[,!(names(mwp.2011.2016D) %in% c("X"))]

mwp.2017.05 = read.csv("../../Data/mwp.2017.05.csv", sep = ",", dec = ".", header = TRUE)
mwp.2017.05$DateTime = as.POSIXct(mwp.2017.05$DateTime)
mwp.2017.05 = mwp.2017.05[,!(names(mwp.2017.05) %in% c("X"))]

# !! rbind would create duplicates at the end of the year
auctions.2011.2016 <- getReserveAuctions('01.07.2011', '31.12.2016', '2')
calls.2011.2016 <- rbind(calls.2011, calls.2012, calls.2013, calls.2014, calls.2015, calls.2016)
needs.2011.2016 <- rbind(needs.2011, needs.2012, needs.2013, needs.2014, needs.2015, needs.2016)



# ------------------------------------------------------------------------------------------------------------------------------------------------


#
# Explore Data Part
#


# Auctions Data
# ------------------------------------------------------------------------------------------------------------------------------------------------

# filter by direction
# and get average of the
library(dplyr)

auctions.2016 %>%
  group_by_(.dots = c("date_from", "Direction")) %>%
  summarise(avg = mean(work_price)) %>%
  ggplot(aes(x = date_from, y = avg, colour = Direction)) +
  geom_line()

# ------------------------------------------------------------------------------------------------------------------------------------------------


# Marginal Work Prices
# ------------------------------------------------------------------------------------------------------------------------------------------------
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




# ------------------------------------------------------------------------------------------------------------------------------------------------



# Environment for Hacking-Session
#
# -------------------------------------------------------------------------------------------------------------------------------------------------


# 1)      Überprüfung der minütlich berechneten Grenzarbeitspreise für zufällige Tage
# --> Sicherheit schaffen, da Grundlage für weitere Betrachtungen
#
# 2)      Erzeugung von Graphen, die die Entwicklung der Arbeitspreise über die Jahre darstellen
# --> Trend erkennbar?
#
# 3)      Erzeugung von Graphen, die saisonale Charakteristiken verdeutlichen
# --> Saisonalität erkennbar?
#
# 4)      Erzeugung von Graphen, die die Fluktuation der nachgefragten Regelleistung (Gesamtabruf pro Minute; unabhängig von Arbeitspreis) darstellen
# --> Wird immer gleich viel Regelleistung nachgefragt, wann mehr, wann weniger, lassen sich Patterns erkennen? Was sind die Auswirkungen auf die Abrufe in den Merit-Order-Listen?
#
# 5)      Erzeugung von Graphen, die die Ähnlichkeit/Verschiedenheit der Merit-Order-Listen der einzelnen Wochen verdeutlichen
# --> Sind die Gebote immer ähnlich oder nicht?




# 1)      Überprüfung der minütlich berechneten Grenzarbeitspreise für zufällige Tage (--> zufällige 15min)
# --> Sicherheit schaffen, da Grundlage für weitere Betrachtungen
# ----
# --> see analyzeScript.R --> at the end --> section: Calculating the approximation step by step

date = "2016-01-01 00:15:00"
date <- as.POSIXct(date)

start <- 2  # start observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)
end <- 2   # end observation number of 15min calls (--> e.g. 49*15/60 gives the hour of the day)

needs <- filter(needs.2011.2016, DateTime >= date, DateTime < date + 900)
calls <- filter(calls.2011.2016, DateTime >= date, DateTime < date + 900)
auctions <- auctions.2011.2016

t <- filter(mwp.2011.2016, DateTime >= date, DateTime < date + 900)
t2 <- getMarginalWorkPrices(needs, calls, auctions, 2)



# ----


# 2)      Erzeugung von Graphen, die die Entwicklung der Arbeitspreise über die Jahre darstellen
# --> Trend erkennbar?
# ----



a <- auctions.2011.2016 %>%
      group_by(date_from, Tarif, Direction) %>%
      summarise(max_work_price = max(work_price),
                min_work_price = min(work_price),
                mean_work_price = mean(work_price),
                sd_work_price = sd(work_price),
                q75_work_price = quantile(work_price, probs=0.75),
                q25_work_price = quantile(work_price, probs=0.25),
                median_work_price = quantile(work_price, probs=0.5))


(g <- ggplot(data = a[1:20,], aes(x = date_from, y = mean_work_price, color = Direction, linetype = Tarif, frame = date_from)) +
  geom_line()
)

gganimate(g)


(p <- ggplot(data = a, aes(x = date_from, frame = date_from)) +
  geom_line(mapping = aes(y = max_work_price, color = "max")) +
  geom_line(mapping = aes(y = min_work_price, color = "min")) +
  geom_line(mapping = aes(y = median_work_price, color = "median")) +
  geom_ribbon(aes(ymax = q75_work_price, ymin = q25_work_price), alpha = 0.4, fill = "skyblue") +
  facet_grid(Direction ~ Tarif) +
  scale_color_manual(values = c("max" = "#339966", "median" = "blue", "min" = "#990033")) +
  labs(x = "Date (weekly auctions)",
       y = "Work Price in €/MWh",
       title = "Work Prices of the Auctions from 2011 till 2016")
  #theme_bw()
)

p

# ANIMATIOn

library(gapminder)

theme_set(theme_bw())
(p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
  geom_point() +
  scale_x_log10()
)

gganimate(p)

install.packages("magick")


# ----



# 3)      Erzeugung von Graphen, die saisonale Charakteristiken verdeutlichen --> Arbeitspreise?
# --> Saisonalität erkennbar?



t <- mwp.2011.2016 %>% filter(num_recursions == 2)


ggplot(t[31:45,], aes(DateTime, approx_1min_call)) +
  geom_step()


# -------------------------------------------------------------------------------------------------------------------------------------------------




t <- t.all



ggplot(t.all, aes(x = DateTime)) +
  #geom_line(aes(y = MW), color = "grey") +
  geom_step(aes(y = avg_1min_MW), color = "blue") +
  #geom_step(aes(y = avg_15min_MW_NEG), color = "red") +
  # geom_step(aes(y = avg_15min_MW_POS), color = "green") +
  geom_step(aes(y = neg_MW), color = "red", linetype = 3) +
  geom_step(aes(y = pos_MW), color = "green", linetype = 3)


















