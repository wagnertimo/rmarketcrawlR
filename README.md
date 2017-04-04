# rmarketcrawlR


## Goal

This R package provides functions to crawl the german operating reserve market of the platform https://www.regelleistung.net and contains scripts to analyze this data.
It is build as part of my master thesis to analyze the calling probability of operating reserve power depending on the offered work price. The aim is to predict such call probabilities, for improving existing optimization algorithms which relay on assumptions and are not very accurate.

## Functionality

### scrapeData.R script

The scrapeData.R script contains 3 main functions to provide the operating reserve power data (rl_type) from the transperant website of the four energy system providers (uenb_type). For more information about the method check out the documentation.

1. getOperatingReserveCalls(date_from, date_to, uenb_type, rl_type): It retrieves the 15min operating reserve calls for several products and providers. (source: [https://www.regelleistung.net/ext/data/])
2. getOperatingReserveAuctions(date_from, date_to, rl_type): It retrieves the operating reserve auction results (anonymous order list of energy providers) for a specific product and time period. (source: [https://www.regelleistung.net/ext/tender/])
3. getOperatingReserveNeeds(startDate, endDate): It retrieves the operating reserve needs for a specific time frame of the Netzregelverbund (NRV) based on a 4 sec resolution. (source: [https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf])

### preprocessData.R script

There is a preprocessData.R script which nicely formats the retrieved data.frames for further use.
It also contains a function approximateOperatingReserveCalls(reserveNeeds) to approximate the operating reserve calls in a higher resolution (1 min instead of 15min).
