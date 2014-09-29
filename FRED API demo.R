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

## The main parameters of the API UFL are the api_key, series_id, and file_type. You can request your API key after registering 
## an account at the Federal Reserve Bank of St. Louis. You can choose to retrieve your data as either xml, json, txt, or xls 
## files. For the purpose of this demonstration we will be looking at JSON data from the payroll survey conducted on a monthly 
## basis by the Bureau of Labor Statistics (series_id = PAYEMS).

fred.api.url <- "http://api.stlouisfed.org/fred/series/observations?series_id=PAYEMS&api_key=yourAPIkeyhere&file_type=json"


my.app <- oauth_app("Establishment Data", key = fred.api.key, secret = NULL)
signature <- sign_oauth1.0(my.app, token = NULL, token_secret = NULL)

establishment.json.data <- GET(fred.api.url, signature)
date.downloaded <- date()

establishment.data <- jsonlite::fromJSON(fred.api.url)

## Now that we have the querried the data from FRED, we must convert the data into a data frame so that we can use R to explore and
## plot the data. We will be attempting to recreate a popular chart showing the monthly change in nonfarm payrolls from the start of
## the Great Recession (December 2007 as dated by the National Bureau of Economic Research) through the present. The chart became
## popular (or infamous), when Democrats began shading the data different colors to correspond to which party was in control of the 
## executive branch.

values <- as.numeric(establishment.data$observations$value)
dates <- as.Date(establishment.data$observations$date)
change <- diff(values)

df <- data.frame(dates, values)
df2 <- df[-1, ]
df2 <- data.frame(df2, change)
bush.obama <- df2[-(1:(which(df2$dates == "2007-12-01"))), ]

bush.obama$obama <- 1
bush.obama$obama[bush.obama$dates < "2009-12-01"] <- 0
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

qplot(x    = dates, 
      y    = change, 
      data = bush.obama,
      geom = "bar",
      stat = "identity",
      fill = factor(obama),
      main = "Monthly Change in Nonfarm Payrolls",
      ylab = "Number of Persons",
      xlab = "") + theme(legend.position = "none")

## Next, lets try to create a bevridge curve which is a graphical relationship between the job vacancy and uemployment rates.
## Some economists have used this curve to argue that US labor market has undergone a structural shift in the aftermath of the
## 2007-2009 recession. We will continue to use JSON data and the series ids for the data are JTSJOR and UNRATE, respectively.

fred.api.url2 <- "http://api.stlouisfed.org/fred/series/observations?series_id=JTSJOR&api_key=yourAPIkeyhere&file_type=json"
fred.api.url3 <- "http://api.stlouisfed.org/fred/series/observations?series_id=UNRATE&api_key=yourAPIkeyhere&file_type=json"

my.app2 <- oauth_app("JOLTS Data", key = fred.api.key, secret = NULL)
signature2 <- sign_oauth1.0(my.app2, token = NULL, token_secret = NULL)
JOLTS.json.data <- GET(fred.api.url2, signature2)

JOLTS.data <- jsonlite::fromJSON(fred.api.url2)

my.app3 <- oauth_app("Household Data", key = fred.api.key, secret = NULL)
signature3 <- sign_oauth1.0(my.app3, token = NULL, token_secret = NULL)
Household.json.data <- GET(fred.api.url3, signature3)

Household.data <- jsonlite::fromJSON(fred.api.url3)

JOLTS.values <- as.numeric(JOLTS.data$observations$value)
JOLTS.dates <- as.Date(JOLTS.data$observations$date)

Household.values <- as.numeric(Household.data$observations$value)
Household.dates <- as.Date(Household.data$observations$date)

JOLTS.df <- data.frame(JOLTS.dates, JOLTS.values)
Household.df <- data.frame(Household.dates, Household.values)
Household.df <- Household.df[-(1:(which(Household.df$Household.dates == min(JOLTS.df$JOLTS.dates)))), ]

df <- cbind(Household.df, JOLTS.df)
df <- df[c(-3)]
df$recession <- 0
df$recession[df$Household.dates > "2007-12-01"] <- 1
df$recession <- factor(df$recession)
levels(df$recession) <- c("Pre-Recession", "Post-Recession")

qplot(x    = Household.values, 
      y    = JOLTS.values, 
      data = df,
      main = "Has there been a structural shift in the US labor market?",
      xlab = "Unemployment Rate",
      ylab = "Job Vacancy Rate") + 
      geom_smooth(method = "lm", aes(colour = factor(recession)))

## As you can see prior to the recession there was a strong negative relationship between the job vacancy rate and unemployment
## rate. As job openings climb, the unemployment rate falls (red line). Since the recession the relationship has weakened and
## the beveridge curve has flattened. There seems to have been a structural shift in the curve where any given job vacancy rate
## is now associated with a higher unemployment rate. 
