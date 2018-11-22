rm(list = ls())
dev.off()
#### LIBRARIES ####
library(dplyr)
library(forecast)
library(lubridate)
library(xts)
library(ggplot2)
library(imputeTS)
library(TTR)
library(astsa)
#### WD & DATA ####
setwd("C:/Users/David/Google Drive/Github/task-3-1-define-a-data-science-process-dgibert17")

df = read.table("C:/Users/David/Google Drive/Ubiqum/6_EnergyConsumption/power_consumption.txt",
                sep = ";",
                dec = ".",
                col.names = c("date", "time", "global_active_power",
                              "global_reactive_power", "voltage", "global_intensity",
                              "kitchen", "laundry_room", "heater_conditioner"),
                na.strings = c("?", "-", "NA"),
                stringsAsFactors = F,
                header = T)

df = df %>%
  mutate(global_active_power = (global_active_power*1000)/60,
         global_reactive_power = (global_reactive_power*1000)/60,
         total_consump = global_active_power+global_reactive_power,
         #Las tres en Watt hour (Wh)
         total_rest = total_consump -kitchen -laundry_room -heater_conditioner,
         #En Watt hour (Wh)
         
         date = as.Date(date, format = "%d/%m/%Y"),
         month = as.Date(cut(date, breaks = "month")),
         week = as.Date(cut(date, breaks = "week", start.on.monday = T)),
         
         #total rest es el gasto de energia que no contempla submetering
         
         ### IMPORTANTE ###
         ## EL TOTAL REST MUESTRA UN GASTO ENORME QUE NO SABEMOS DE DONDE SALE ##
         
         total_submeter = kitchen + laundry_room + heater_conditioner) %>%
  #Gasto de submetering
  filter(date > "2006-12-31") %>% # & date < "2010-01-01"
  select(-time)

#### NA VALUES BARPLOT ####

barplot(table(df[rowSums(is.na(df)) >= 1 & rowSums(is.na(df)) < length(colnames(df))-1, 1]),
        ylab = "Amount of NA values",
        xlab = "Date",
        main = "Distribution of NA values",
        col = "lightblue")
#Distribution of NA per day

tab = table(df[rowSums(is.na(df)) >= 1 & rowSums(is.na(df)) < length(colnames(df))-1, 1])

#### NA VALUES REPLACEMENT AND GROUP DATA BY MONTH AND AVERAGE ####
df.mean = df %>%
  select(-date, -week, -month) %>%
  na.mean(option = "mean")

df.mean = cbind(df.mean, month = df$month)

df.mean = df.mean %>%
  group_by(month) %>%
  summarise_all(mean) %>%
  mutate(month = as.yearmon(month))

#### DF-TS ####

total.ts = ts(df.mean$total_consump, frequency = 12, start = 2007)
plot(total.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series")

#### TRAINING & TEST TS ####
train = df.mean %>%
  filter(month > "Dec 2006" & month < "Jan 2010")

test = df.mean %>%
  filter(month >= "Jan 2010")

totCons.train.ts = ts(train$total_consump, frequency = 12, start = 2007)
plot(totCons.train.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series\nTraining set")

totCons.test.ts = ts(test$total_consump, frequency = 12, start = 2010)
plot(totCons.test.ts, col = "darkblue", lwd = 3, main = "Total energy consumption Time Series\nTest set")

#### PLOTING YEARS SEASONALITY BY MONTH ####

ggseasonplot(total.ts, year.labels=TRUE, year.labels.left=TRUE) +
  geom_line(size=2) +
  geom_point(size=6) +
  ylab("Amount consumed") +
  ggtitle("Seasonal plot: Energy consumption")

#### FORECASTING MODELS WITH SEASON, TREND AND RANDOM ####
HW.fit <- HoltWinters(totCons.train.ts)
plot(train.fit)
HW.for = forecast(HW.fit, h = 12)

arima.fit = auto.arima(totCons.train.ts)
arima.for = forecast(arima.fit, h = 12)

par(mfrow = c(2,1))
plot(HW.for)
plot(arima.for)

#### CHECKING THAT THE OUTLIER PEAK COMES FROM WHITE NOISE ####

dec = decompose(total.ts)
par(mfrow = c(3,1))
plot(dec$x, lwd = 3, col = "darkblue", main = "TS", ylab = "", xlab = "")
plot(dec$seasonal, lwd = 3, col = "darkblue", main = "Seasonality", ylab = "", xlab = "")
plot(dec$random, lwd = 3, col = "darkblue", main = "White Noise", ylab = "", xlab = "")

#### FORECASTING WITHOUT RANDOM ####
dec$random



#### PLOTING MODELS INTO REAL TEST DATA ####
autoplot(total.ts) +
  geom_line(lwd = 1) +
  geom_point(size = 3) +
  autolayer(fore, PI = F, lwd = 2)

#### ERROR METRICS FOR MODELS ####
accuracy(fore, totCons.test.ts)
