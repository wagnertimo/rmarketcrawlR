# rmarketcrawlR <br/> – The R package for crawling data of the german energy reserve market - 


## Goal

This R package provides functions to crawl the german operating reserve market of the platform https://www.regelleistung.net and contains scripts to analyze this data.
It is build as part of my master thesis to analyze the calling probability of operating reserve power depending on the offered work price. The aim is to predict such call probabilities, for improving existing optimization algorithms which relay on assumptions and are not very accurate.

## Get Started

### Installing

When installing this package you should at least use the *R version 3.3.0 (2016-05-03)*. For the library dependecies see the section below. You can easily install this R package by using the `install_github()` function from the `devtools` package:

```r
library(devtools)
install_github("wagnertimo/rmarketcrawlR")
```
### Library dependencies

Before using this R package, please check that you have installed the following R packages. Normally during the installation of the package those dependencies will also be installed. If not you have to do it manually.

- `httr`
- `xml2`
- `XML`
- `zoo`
- `lubridate`
- `timeDate`
- `dplyr`
- `tidyr`
- `magrittr`
- `data.table`
- `ggplot2`
- `doParallel`
- `foreach`
- `logging`
- `plotly`


### Usage

#### Get operating reserve power data

There are three main functions which allows you to crawl operating reserve calls, needs and auction results of the german operating reserve market. The operating reserve power data (specified by the parameter `rl`) is provided by the transperant website (https://www.regelleistung.net) of the four energy system providers (specified by the parameter `uenb`):

* `getReserveCalls(startDate, endDate, uenb, rl)`: It retrieves the 15min operating reserve calls for several products and providers (source: https://www.regelleistung.net/ext/data/). It is important that you do not exceed the time range over several years including the daylight saving date (last sunday of october). Since then the `addTimezone()` function creates false data. This user inconvenience is not yet corrected. Checkout documentation for further information `?getReserveCalls`.

* `getReserveAuctions(startDate, endDate, rl)`: It retrieves the operating reserve auction results (anonymous order list of energy providers; MOL) for a specific product and time period (source: https://www.regelleistung.net/ext/tender/). The data gets formatted. Checkout documentation for further information `?getReserveAuctions`.

* `getReserveNeeds(startDate, endDate)`: It retrieves the operating reserve needs for a specific time frame of the Netzregelverbund (NRV) based on a 4 sec resolution (source: https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf). It is important that you do not exceed the time range over several years including the daylight saving date (last sunday of october). Since then the `addTimezone()` function creates false data. This user inconvenience is not yet corrected. Checkout documentation for further information `?getReserveNeeds`.

> It is important to mention that the function `getReserveNeeds()` will cause an error if in the main project path (working directory) other `.csv` files are located. Such files should be moved to another folder (e.g. data folder).

Below you will find an example code snippet to get started. It is shown how to crawl the operating reserve power data. The auctions data is weekly based from monday till sunday. The `getReserveAuctions()` functions takes care of mapping the input dates to the right weekly beginning and ending. It is also important that you set the logging state in the begining. Till now there is no default value for it. Forgetting to set the log status will break all functions. Every main function (like the get... functions) trigger a log file to be written in the workspace directory (with execution time in its name).

```r
# Activate the package in the workspace
library(rmarketcrawlR)

# You have to set logging to TRUE or FALSE if you want logs printed out and written in a file (Good for Debugging)
# No default yet. Will break if not set.
setLogging(TRUE)

# Get sample data for operating needs, calls and auctions of secondary reserve power from the Netzregelverbund
needs = getReserveNeeds('01.01.2016', '10.01.2016')
calls = getReserveCalls('01.01.2016', '10.01.2016', '6', 'SRL')
auctions = getReserveAuctions('01.01.2016', '10.01.2016', '2')

```




## Notes and Miscellaneous

> Data for **operating reserve calls** are available since `2011-06-27` at https://www.regelleistung.net/ext/data/. 
Data for **operating reserve needs** (4sec data) are available since July 2010 at https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/.



> There are missing values and outliers. Outliers were identified for the operative reserve needs in 2011 and 2012. Those oultliers are characterized by continuous zero values over a longer period of observations. 
There are also missing values for operative reserve needs in 2012 (observations are marked with *"NaN"*) and for operative reserve calls in 2011, 2012 and 2013 (negative and positive power of the *Netzregelverbund*).



Version v04 - 16.06.2017


