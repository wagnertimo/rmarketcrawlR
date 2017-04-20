# rmarketcrawlR


## Goal

This R package provides functions to crawl the german operating reserve market of the platform https://www.regelleistung.net and contains scripts to analyze this data.
It is build as part of my master thesis to analyze the calling probability of operating reserve power depending on the offered work price. The aim is to predict such call probabilities, for improving existing optimization algorithms which relay on assumptions and are not very accurate.

## Functionality

### Scraping reserve data

The `scrapeData.R` script contains 3 main functions to provide the operating reserve power data (`rl_type`) from the transperant website of the four energy system providers (`uenb_type`). For more information about the method check out the documentation.

* `getOperatingReserveCalls(date_from, date_to, uenb_type, rl_type)`: It retrieves the 15min operating reserve calls for several products and providers. (source: https://www.regelleistung.net/ext/data/)

* `getOperatingReserveAuctions(date_from, date_to, rl_type)`: It retrieves the operating reserve auction results (anonymous order list of energy providers) for a specific product and time period. (source: https://www.regelleistung.net/ext/tender/)

* `getOperatingReserveNeeds(startDate, endDate)`: It retrieves the operating reserve needs for a specific time frame of the Netzregelverbund (NRV) based on a 4 sec resolution. (source: https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf)

### Preprocess the scraped data

There is a `preprocessData.R` script which nicely formats and prepares the scraped data.frames for further use. The main functions:

* `preProcessOperatingReserveCalls(scraped.calls)`

* `preProcessOperatingReserveCalls(scraped.needs)`

* `addTarif(df)`: it adds a *Tarif* variable to an input data.frame with values of *HT* (Hochtarif) or *NT* (Nebentarif). It is based on the hour and date as well as the official federal-state wide german holidays. Read documentation.

* `approximateCalls(preprocessed.needs, preprocessed.calls)`: approximates the operating reserve calls in a higher resolution (1 min instead of 15min). It considers some special cases which can occur. The case of **homogenity** where all 1min needs are homogenly positive (or negative) within a 15min section. This leads to a 15min average need for negative (positive) power of 0. But in the case that the 15min calls of negative (positive) power is not 0, the 1min needs have to be changed. Its smallest absolute value gets the negative (positive) value to fulfill the 15min average call in 1min.


## Miscellaneous

Data of operating reserve calls are available since 2011-06-27 at https://www.regelleistung.net/ext/data/


Version v01 - 05.04.2017
