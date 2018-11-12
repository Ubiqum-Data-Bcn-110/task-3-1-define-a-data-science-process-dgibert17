#### DATA DEFINITION ####

# This archive contains 2075259 measurements gathered in a house located in Sceaux
# (7km of Paris, France) between December 2006 and November 2010 (47 months).

# 1.(global_active_power*1000/60 - sub_metering_1 - sub_metering_2 - sub_metering_3)
#     represents the active energy consumed every minute (in watt hour)
#     in the household by electrical equipment not measured in sub-meterings 1, 2 and 3. 
# 2.The dataset contains some missing values in the measurements (nearly 1,25% of the rows).
#     All calendar timestamps are present in the dataset but for some timestamps,
#     the measurement values are missing: a missing value is represented by the absence of value
#     between two consecutive semi-colon attribute separators.
#     For instance, the dataset shows missing values on April 28, 2007.

#### ATTRIBUTE INFORMATION ####

# 1.date: Date in format dd/mm/yyyy 
# 2.time: time in format hh:mm:ss 
# 3.global_active_power: household global minute-averaged active power (in kilowatt) 
# 4.global_reactive_power: household global minute-averaged reactive power (in kilowatt) 
# 5.voltage: minute-averaged voltage (in volt) 
# 6.global_intensity: household global minute-averaged current intensity (in ampere) 
# 7.sub_metering_1: energy sub-metering No. 1 (in watt-hour of active energy).
# It corresponds to the kitchen, containing mainly a dishwasher, an oven and a microwave
# (hot plates are not electric but gas powered). 
# 8.sub_metering_2: energy sub-metering No. 2 (in watt-hour of active energy).
# It corresponds to the laundry room, containing a washing-machine, a tumble-drier,
# a refrigerator and a light. 
# 9.sub_metering_3: energy sub-metering No. 3 (in watt-hour of active energy).
# It corresponds to an electric water-heater and an air-conditioner.

#### FORMAT DATE & TIME ####

rm(list = ls())

setwd("C:/Users/David/Google Drive/Github/task-3-1-define-a-data-science-process-dgibert17")
load(file = "DFenergy.Rdata")

df <- cbind(df,
            paste(df$date, df$time),
            stringsAsFactors = FALSE)

colnames(df)[10] <- "dateTime"

df <- df[, c(ncol(df), 1:(ncol(df)-1))] #Order columns. First dateTime then others.

df$dateTime <- dmy_hms(df$dateTime)
df$date <- as.Date(df$date, "%d/%m/%Y")

df2 = df[,c(1,4:10)]

summary(df2)

df2 = df2 %>%
  mutate(global_active_KWh = (global_active_power*1000)/60, #El global en KWh
         global_rest_KWh = ((global_active_power*1000)/60) -kitchen -laundry_room -heater_conditioner,
         #Global rest es el gasto de energia que no contempla submetering
         ############################ IMPORTANTE #################################
         ## EL GLOBAL REST MUESTRA UN GASTO ENORME QUE NO SABEMOS DE DONDE SALE ##
         #########################################################################
         global_submeter = kitchen + laundry_room + heater_conditioner, #Gasto de submetering
         substract_active_rest = global_active_KWh - global_rest_KWh #Gasto de submetering
         )

write.csv(df2, file = "df2.csv")