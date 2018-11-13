rm(list = ls())

library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(forecast)
library(zoo)
library(xts)

setwd("C:/Users/David/Google Drive/Github/task-3-1-define-a-data-science-process-dgibert17")
load(file = "DFenergy_featEng.Rdata")

head(df2)

kitchen.df   =  df2[,c("dateTime", "kitchen")]
laundry.df   =  df2[,c("dateTime", "laundry_room")]
heater.df    =  df2[,c("dateTime", "heater_conditioner")]
submeter.df  =  df2[,c("dateTime", "total_submeter")]
total.df     =  df2[,c("dateTime", "total_consump_KWh")]
rest.df      =  df2[,c("dateTime", "total_rest_KWh")]

kitchen.ts   =  xts(kitchen.df$kitchen, kitchen.df$dateTime)
laundry.ts   =  xts(laundry.df$laundry_room, laundry.df$dateTime)
heater.ts    =  xts(heater.df$heater_conditioner, heater.df$dateTime)
submeter.ts  =  xts(submeter.df$total_submeter, submeter.df$dateTime)
total.ts     =  xts(total.df$total_consump_KWh, total.df$dateTime)
rest.ts      =  xts(rest.df$total_rest_KWh, rest.df$dateTime)

rm(kitchen.df, laundry.df, heater.df, submeter.df, total.df, rest.df)

tzone(kitchen.ts)  <- Sys.timezone()
tzone(laundry.ts)  <- Sys.timezone()
tzone(heater.ts)   <- Sys.timezone()
tzone(submeter.ts) <- Sys.timezone()
tzone(total.ts)    <- Sys.timezone()
tzone(rest.ts)     <- Sys.timezone()

write.csv(x = df2,
          file = "C:/Users/David/Google Drive/energy_consumption.csv",
          na = "NA",
          row.names = F
          )

