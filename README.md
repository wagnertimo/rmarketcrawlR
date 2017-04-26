# rmarketcrawlR


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

### Usage

#### Get operating reserve power data

There are three main functions which allows you to crawl operating reserve calls, needs and auction results of the german operating reserve market. The operating reserve power data (specified by the parameter `rl`) is provided by the transperant website (https://www.regelleistung.net) of the four energy system providers (specified by the parameter `uenb`):

* `getReserveCalls(startDate, endDate, uenb, rl)`: It retrieves the 15min operating reserve calls for several products and providers (source: https://www.regelleistung.net/ext/data/). The data gets formatted. Checkout documentation for further information.

* `getReserveAuctions(startDate, endDate, rl)`: It retrieves the operating reserve auction results (anonymous order list of energy providers; MOL) for a specific product and time period (source: https://www.regelleistung.net/ext/tender/). The data gets formatted. Checkout documentation for further information.

* `getReserveNeeds(startDate, endDate)`: It retrieves the operating reserve needs for a specific time frame of the Netzregelverbund (NRV) based on a 4 sec resolution (source: https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/regelenergie-bedarf-und-abruf). The data gets formatted. Checkout documentation for further information.

Below you will find an example code snippet to get started. It is shown how to crawl the operating reserve power data. It should be mentioned that you have to take care of the time period for the auctions data. The data is weekly based from monday till sunday. So when you want to do operations with it in combination with needs and/or calls, the time periods have to overlap. It is also important that you set the logging state in the begining. Till now there is no default value for it. Forgetting to set the log status will break all functions.

```r
# Activate the package in the workspace
library(rmarketcrawlR)

# You have to set logging to TRUE or FALSE if you want logs printed out (Good for Debugging)
# No default yet. Will break if not set.
setLogging(TRUE)

# Get sample data for operating needs, calls and auctions of secondary reserve power from the Netzregelverbund
needs = getReserveNeeds('01.01.2016', '10.01.2016')
calls = getReserveCalls('01.01.2016', '10.01.2016', '6', 'SRL')
auctions = getReserveAuctions('28.12.2015', '10.01.2016', '2')

```

#### Get the approximated 1min Call data and the 1min marginal work prices

The approximation of the operating reserve calls in a higher resolution (1 min instead of 15min) considers some special cases which can occur. The case of **homogenity** where all averaged 1min reserve needs are homogenly positive (or negative) within a 15min section. This leads to a 15min average need for negative (positive) power of 0. But in the case that the 15min calls of negative (positive) power is not 0, the 1min needs have to be changed. Its smallest absolute value gets the negative (positive) value to fulfill the 15min average call in 1min. Hereby, cases can occur where the newly modified data points cross the zero level; they change their sign (case of **CrossingZero**). Hence the overall 15min average is not equal to the expected 15min call average. Therefore a recursive modification changes iteratively the data points till the averages are equal.

Since calculating the marginal work prices is highly computational, it is recommended to use the parallel computing wrapper by specifying the optional parameter `numCores` in the `getMarginalWorkPrices()` function.

The code snippet below provides you an example to calculate either the 1min approximated calls or the marginal work prices based on the 1min calls.

```r
# Use the crawled data from above. Logging is set to true.

# Calculate the approximated 1min calls from the 4sec operating reserve needs data
approx.calls = getOneMinuteCalls(needs, calls)

# Calculate directly the marginal work price by internally approximate 1min calls
marginal.prices = getMarginalWorkPrices(needs, calls, auctions)

# There is also a wrapper for parallel computing. This makes sense to use if the time period lies over several days
# Therefore set the optional parameter numCores to the amount of processors you want to use. 
marginal.prices.parallel = getMarginalWorkPrices(needs, calls, auctions, numCores = 2)

```

#### Calculate the call probabilities

Now that you have the data set with the 1min approximated calls and their respective marginal work price, you are able to compute call probabilities for different given work prices and product types (Tarif and Direction, e.g. `NT_NEG`) in a specified time period. Therefore the function `getCallProbDataSet()` is implemented. It uses parallel computing, so it is necessary to specify the processors cores parameter `numCores`.

```r
# Use the crawled data from above. Logging is set to true.

# Get the call probabilities for one day (2016-01-01) of the product "NT_POS" and within the price range of 0 to 775 (this is the max marginal work price for that period). Use only one process core for the parallel computation.
call.probs <- getCallProbDataSet(marginal.prices.parallel, 1, 0, 775, "2016-01-01 00:00:00", "2016-01-01 23:59:59", "NT", "POS")

# Plot the call probabilities. Therefore create an array with the price range for the x axis. On the y axis set the computed call.probs
library(ggplot2)
price.range <- seq(0, ceiling(max.mwork))
qplot(price.range, call.probs, geom="line")

```


## Miscellaneous

Data of operating reserve calls are available since 2011-06-27 at https://www.regelleistung.net/ext/data/.

Data of operating reserve needs (4sec data) are available since July 2010 at https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/.


Version v02 - 21.04.2017
