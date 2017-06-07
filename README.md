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

#### Impute missing calls

For 2013, 2012 and 2011 some negative and positive secondary reserve power data is missing. To replace (impute) those `NA` values, the call data of the four TSOs (50Hz, TenneT, Amprion and TransnetBW) is used. The small code example below shows the procedure and the imputation function `imputeMissingCallsWithTSO`. It takes as arguments the data.frame of calls with missing values (e.g. 2013) and a list of the call data.frames of the TSOs.

```r
# Get call data with missing values
calls = getReserveCalls('01.01.2013', '31.12.2013', '6', 'SRL')

# 50Hz (4)
calls.2013.4 = getReserveCalls('01.01.2013', '31.12.2013', '4', 'SRL')
# TenneT (2)
calls.2013.2 = getReserveCalls('01.01.2013', '31.12.2013', '2', 'SRL')
# Amprion (3)
calls.2013.3 = getReserveCalls('01.01.2013', '31.12.2013', '3', 'SRL')
# TransnetBW (1)
calls.2013.1 = getReserveCalls('01.01.2013', '31.12.2013', '1', 'SRL')
# Build the tso list
tso.list <- list(calls.2013.1,calls.2013.2,calls.2013.3,calls.2013.4)

# Impute the data
imputed.calls <- imputeMissingCallsWithTSO(calls, tso.list)
```

#### Get the approximated 1min Call data and the 1min marginal work prices

The approximation of the operating reserve calls in a higher resolution (1 min instead of 15min) is computed by the function `getOneMinuteCalls(needs, calls)` and indirectly by the function `getMarginalWorkPrices(needs, calls, auctions, numCores)`. They consider some special cases which can occur. The case of **homogenity** where all averaged 1min reserve needs are homogenly positive (or negative) within a 15min section. This leads to a 15min average need for negative (positive) power of 0. But in the case that the 15min calls of negative (positive) power is not 0, the 1min needs have to be changed. Its smallest absolute value gets the negative (positive) value to fulfill the 15min average call in 1min. Hereby, cases can occur where the newly modified data points cross the zero level; they change their sign (case of **CrossingZero**). Hence the overall 15min average is not equal to the expected 15min call average. Therefore a recursive modification changes iteratively the data points till the averages are equal.

Since calculating the marginal work prices is highly computational, it is recommended to use the parallel computing wrapper by setting the optional parameter `numCores` in the `getMarginalWorkPrices(needs, calls, auctions, numCores)` function greater than *1*. This parameter is mandatory. If you skip it the code will break.

The code snippet below provides you an example to calculate either the 1min approximated calls or the marginal work prices based on the 1min calls.

```r
# Use the crawled data from above. Logging is set to true.

# Calculate the approximated 1min calls from the 4sec operating reserve needs data
approx.calls = getOneMinuteCalls(needs, calls)

# Calculate directly the marginal work price by internally approximate 1min calls
# There is also a wrapper for parallel computing. This makes sense to use if the time period lies over several days
# Therefore set the optional parameter numCores to the amount of processors you want to use. 
marginal.prices.parallel = getMarginalWorkPrices(needs, calls, auctions, numCores = 2)

```

#### Calculate the call probabilities

Now that you have the data set with the 1min approximated calls and their respective marginal work prices, you are able to compute conditional call probabilities for different given work prices. The condition can be parameterized by an character array `c()`  with e.g. `Tarif` and `Direction`. Hereby, is the variable `Direction` mandatory, since the denominator (number of total observations) for the probability computation depends on `NEG` and `POS` calls. You can add extra columns/variables to the input data set (here `marginal.prices.parallel`) which can be used as conditional parameters. E.g. one can add a column `DateClass` which specifies if the observation is a work day or week end day. The function `getCallProbDataSet(data, numCores, price.seq.start, price.seq.end, granularity, conditionByColumns)` uses parallel computing, so it is necessary to specify the processors cores parameter `numCores`. The input data set has to have the `marginal_work_price` variable. 

```r
# Use the crawled data from above. Logging is set to true.

# Get the conditional call probabilities for the whole data set by using just one processor core. 
# The price range for the probabilities is from 0 to 775. 
# The granularity is set to 1 such that the price range adds up in one steps like 0,1,2,..754,755
# The condition is on the variables Tarif and Direction. But you could add e.g. a DateClass column and condition additionally by e.g. Weekend or Workday
call.probs <- getCallProbDataSetOnConditions(marginal.prices.parallel, 1, 0, 775, 1, c("Tarif", "Direction"))

# Plot multiple variables (value) against one target variable (key). The target has to be omitted for the values (2:...)
library(ggplot2)
plot <- call.probs %>%
        # Binds rowwise. Every following column gets bind under the last row. The key variabel (here Price) gets repeated
        gather(key, value, 2:ncol(call.probs)) %>%
        ggplot(aes(x=Price, y=value, colour=key)) +
        geom_line()

plot

```


## Miscellaneous

Data of operating reserve calls are available since 2011-06-27 at https://www.regelleistung.net/ext/data/.

Data of operating reserve needs (4sec data) are available since July 2010 at https://www.transnetbw.de/de/strommarkt/systemdienstleistungen/.


Version v02 - 21.04.2017
