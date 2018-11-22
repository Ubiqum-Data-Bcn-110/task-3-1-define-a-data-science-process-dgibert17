rm(list = ls())

setwd("C:/Users/David/Google Drive/Github/task-3-1-define-a-data-science-process-dgibert17")
load(file = "DFenergy_featEng.Rdata")

df2 = df2 %>%
  mutate(xdate = date(dateTime)) %>%
  select(-dateTime) %>%
  group_by(year(xdate), month(xdate)) %>%
  summarise_all(mean)


head(df2)

kitchen.ts   =  xts(df2$kitchen, df2$date)
laundry.ts   =  xts(df2$laundry_room, df2$date)
heater.ts    =  xts(df2$heater_conditioner, df2$date)
submeter.ts  =  xts(df2$total_submeter, df2$date)
total.ts     =  xts(df2$total_consump, df2$date)
rest.ts      =  xts(df2$total_rest, df2$date)

autoplot(kitchen.ts)
autoplot(laundry.ts)
autoplot(heater.ts)
autoplot(submeter.ts)
autoplot(total.ts)
autoplot(rest.ts)

