library(tidyverse)
library(plyr)
source("Auxiliaries/auxiliaries.R")

df1 <- read.table("Results/Simulation_OOP.csv", header = T, sep = ";")
df2 <- read.table("Results/Simulation_SW.csv", header = T, sep = ";")

df1n <- poly_y(df1, ps = c(40, 85, 90))
df2n <- poly_y(df2, ps = c(40, 85, 90))
df1n <- df1n[, c("Szenario", "Type", "Age", "Mean", "Mean_Exp_Age", "Mean_Smoothed")]
colnames(df1n) <- c("Szenario", "Type", "Age", "Mean_oop", "Mean_Exp_Age", "Mean_Smoothed_oop")

df2n <- df2n[, c("Szenario", "Type", "Age", "Mean", "Mean_Exp_Age", "Mean_Smoothed")]
colnames(df2n) <- c("Szenario", "Type", "Age", "Mean_sw", "Mean_Exp_Age", "Mean_Smoothed_sw")
dfn <- merge(df1n, df2n, by = c("Szenario", "Type", "Age", "Mean_Exp_Age"))
dfn$Years_to_live <- dfn$Mean_Exp_Age - dfn$Age

# raw
dfn$Mean_oop_a <- dfn$Mean_oop / dfn$Years_to_live
dfn$Mean_sw_a <- dfn$Mean_sw / dfn$Years_to_live

dfnr <- dfn %>% subset(Age %in% c(35, 50, 65, 80))
dfnr <- dfnr[, c("Szenario", "Type", "Age", "Mean_oop", "Mean_oop_a", "Mean_sw", "Mean_sw_a")]
dfnr[, 4:7] <- round(dfnr[, 4:7], 0)
dfnr$Type <- factor(dfnr$Type)
dfnr$Type <- recode_factor(dfnr$Type, "all" = "Total Costs", "control" = "Costs G. & Prev.", "perio" = "Costs P.", "disease" = "Costs S. D.")
dfnr <- dfnr[order(dfnr$Szenario, dfnr$Age, dfnr$Type), ]

write.table(dfnr, "Results/Simulated Costs by selected Years.csv", sep = ";", row.names = F)

# smoothed
dfn$Mean_Smoothed_oop_a <- dfn$Mean_Smoothed_oop / dfn$Years_to_live
dfn$Mean_Smoothed_sw_a <- dfn$Mean_Smoothed_sw / dfn$Years_to_live

dfns <- dfn %>% subset(Age %in% c(35, 50, 65, 80))
dfns <- dfns[, c("Szenario", "Type", "Age", "Mean_Smoothed_oop", "Mean_Smoothed_oop_a", "Mean_Smoothed_sw", "Mean_Smoothed_sw_a")]
dfns[, 4:7] <- round(dfns[, 4:7], 0)
dfns$Type <- factor(dfns$Type)
dfns$Type <- recode_factor(dfns$Type, "all" = "Total Costs", "control" = "Costs G. & Prev.", "perio" = "Costs P.", "disease" = "Costs S. D.")
dfns <- dfns[order(dfns$Szenario, dfns$Age, dfns$Type), ]

write.table(dfns, "Results/Smoothed Costs by selected Years.csv", sep = ";", row.names = F)

## return on investment
# total expected mean
count_men <- read.table("Simulation_Data/men_CH.csv", sep = ";", header = T)
count_women <- read.table("Simulation_Data/women_CH.csv", sep = ";", header = T)

count_pop <- data.frame(Age = count_men$Alter, Number = count_men$Anzahl + count_women$Anzahl)
count_pop[101, 2] <- count_pop[101, 2] + count_pop[102, 2]
df_pop <- count_pop %>% subset(Age %in% 35:100)
sum(df_pop$Number)

dfnr_pop <- merge(dfn[, c("Szenario", "Type", "Age", "Mean_oop", "Mean_Smoothed_oop", "Mean_sw", "Mean_Smoothed_sw")], df_pop, by = "Age")
df_tot <- ddply(dfnr_pop, .(Szenario, Type), summarize, raw_oop = sum(Mean_oop * Number), smoothed_oop = sum(Mean_Smoothed_oop * Number),
                raw_sw = sum(Mean_sw * Number), smoothed_sw = sum(Mean_Smoothed_sw * Number))
df_tot$Type <- recode_factor(df_tot$Type, "all" = "Total Costs", "control" = "Costs G. & Prev.", "perio" = "Costs P.", "disease" = "Costs S. D.")
df_tot[, 3:6] <- round(df_tot[, 3:6])

write.table(df_tot, "Results/Total expected Costs CH.csv", sep = ";", row.names = F)

# roi
df_tot_up <- rbind(df_tot[1:4, 3:6], df_tot[1:4, 3:6])
df_tot_low <- df_tot[5:12, ]
df_tot_low[, 3:6] <- df_tot_low[, 3:6] - df_tot_up
df_tot_low

write.table(df_tot_low, "Results/ROI Scenarios.csv", sep = ";", row.names = F)
