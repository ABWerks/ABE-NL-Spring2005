#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#' ABE Newsletter
#' Spring 2025
#' ABE
#' Dependencies:
#' 
#' Description:
#'  ETL for the BTC, SP500, and NASDAQ compsite daily closing prices from 2021
#'  to current.
#' @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
library(lubridate)
library(fpp3)
options(tibble.width = Inf)

# BTC
# https://www.cryptodatadownload.com/data/gemini/
str(inData <- read.csv("./Data/Gemini_BTCUSD_d.csv")[, -1])
inData$Date <- as.Date(inData$date)
summary(inData$Date)
table(duplicated(inData$Date))
plot(inData$Date)
colnames(inData)
dsBTC <- inData[, c("Date", "open", "high", "low", "close", "Volume.BTC", "Volume.USD")]
head(dsBTC)
dsBTC <- dsBTC[!duplicated(dsBTC$Date), ]

# 2021 - current
dsBTC <- dsBTC[dsBTC$Date >= as.Date("2021-01-01"), ]
summary(dsBTC$Date)
colnames(dsBTC) <- c("Date", "Open", "High", "Low", "Close.BTC", "Volume.BTC", "Volume.USD")
str(dsBTC)

# SP500
# https://www.nasdaq.com/market-activity/index/spx/historical?page=1&rows_per_page=10&timeline=y5
inData2 <- read.csv("./Drafts/Spring2025/Data/NASDAQ_SP500.csv")
str(inData2)
inData2$Date <- as.Date(inData2$Date, format = "%m/%d/%Y")
summary(inData2$Date)
table(duplicated(inData2$Date))
plot(inData2$Date)
colnames(inData2)
dsSP500 <- inData2[, c("Date", "Open", "High", "Low", "Close")]
colnames(dsSP500) <- c("Date", "Open", "High", "Low", "Close.SP500")
str(dsSP500)

# 2021 - current
dsSP500 <- dsSP500[dsSP500$Date >= as.Date("2021-01-01"), ]
summary(dsSP500$Date)
head(dsSP500)

# NASDAQ Composite
# https://www.nasdaq.com/market-activity/index/comp/historical?page=1&rows_per_page=10&timeline=y5
inData3 <- read.csv("./Drafts/Spring2025/Data/NASDAQ_COMP.csv")
str(inData3)
inData3$Date <- as.Date(inData3$Date, format = "%m/%d/%Y")
summary(inData3$Date)
table(duplicated(inData3$Date))
plot(inData3$Date)
colnames(inData3)
dsCOMP <- inData3[, c("Date", "Open", "High", "Low", "Close")]
colnames(dsCOMP) <- c("Date", "Open", "High", "Low", "Close.COMP")
str(dsCOMP)

# 2021 - current
dsCOMP <- dsCOMP[dsCOMP$Date >= as.Date("2021-01-01"), ]
summary(dsCOMP$Date)
head(dsCOMP)

# Combine
dsClose <- dsBTC[, c("Date", "Close.BTC")] |>
  inner_join(dsSP500[, c("Date",  "Close.SP500")]) |> 
  inner_join(dsCOMP[, c("Date",  "Close.COMP")]) 
lds <- dsClose |> 
  pivot_longer(!c(Date), names_to = "Series", values_to = "USD")
head(lds)
tail(lds)

# Timeseries by day
tdsDaily <- as_tsibble(lds, key = 'Series', index = 'Date')
tdsDaily

save(dsBTC, dsSP500, dsCOMP, dsClose, tdsDaily, lds, file = "./Drafts/Spring2025/Data/timeseries.Rdata")
