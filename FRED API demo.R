library(httr)
library(jsonlite)
library(ggplot2)

## Create a Data Directory ##
if(!file.exists("FRED API")){
  dir.create("FRED API")
}

## Setup a developer account and request API key at the Federal Reserve Bank of St. Louis (https://research.stlouisfed.org/).
## The API key is used to access the FRED and ALFRED APIs. 

fred.api.key <- "yourAPIkeyhere"

## The main parameters of the API are the api_key, series_id, and file_type. For the purposes of this demonstration, we will be
## looking at Total Nonfarm Payroll data that is released monthly by the Bureau of Labor Statistics. You can choose to GET your
## data as either xml, json, txt, or xls files. This demonstration uses JSON data.

fred.api.url <- "http://api.stlouisfed.org/fred/series/observations?series_id=PAYEMS&api_key=yourAPIkeyhere&file_type=json"


my.app <- oauth_app("FRED Data", key = fred.api.key, secret = NULL)
signature <- sign_oauth1.0(my.app, token = NULL, token_secret = NULL)

json.data <- GET(fred.api.url, signature)
date.downloaded <- date()

data <- jsonlite::fromJSON(fred.api.url)

values <- as.numeric(data$observations$value)
dates <- as.Date(data$observations$date)
change <- diff(values)

df <- data.frame(dates, values)
df2 <- df[-1, ]
df2 <- data.frame(df2, change)
bush.obama <- df2[-(1:(which(df2$dates == "2007-01-01"))), ]

bush.obama$obama <- 1
bush.obama$obama[bush.obama$dates < "2009-01-01"] <- 0
bush.obama$obama <- factor(bush.obama$obama)
levels(bush.obama$obama) <- c("President Bush", "President Obama")

qplot(dates, values, data = df, 
      geom = "line", 
      main = "Growth in the Labor Market", 
      xlab = "", 
      ylab = "Thousands of Persons") + ylim(0, 160000)

qplot(dates, change, data = df2,
      geom = "line",
      main = "Monthly Change in Nonfarm Payrolls",
      ylab = "Number of Persons")

qplot(dates, change, data = bush.obama,
      geom = "bar",
      stat = "identity",
      fill = factor(obama),
      main = "Monthly Change in Nonfarm Payrolls",
      ylab = "Number of Persons",
      xlab = "") + theme(legend.position = "none")
