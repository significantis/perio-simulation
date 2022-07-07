library(tidyverse)
source("Auxiliaries/auxiliaries.R")

df1 <- read.table("Results/Simulation_OOP.csv", header = T, sep = ";")
df2 <- read.table("Results/Simulation_SW.csv", header = T, sep = ";")

# Mean total Costs until Death
df1n <- poly_y(df1, ps = c(40, 85, 90))
df1nn <- rbind(df1n[, c(1:6, 8:11)], df1n[, c(1:6, 8:11)])
df1nn$Mean <- c(df1n$Mean, df1n$Mean_Smoothed)
df1nn$Mean_Type <- rep(c("Simulated Mean", "Smoothed Mean"), each = dim(df1n)[1])
df1nn$Mean_Type <- factor(df1nn$Mean_Type, levels = c("Simulated Mean", "Smoothed Mean"))
df1nn$Szenario <- factor(df1nn$Szenario)
df1nn$Szenario <- recode_factor(df1nn$Szenario, "1" = "Scenario 1", "2" = "Scenario 2", "3" = "Scenario 3")

df2n <- poly_y(df2, ps = c(40, 85, 90))
df2nn <- rbind(df2n[, c(1:6, 8:11)], df2n[, c(1:6, 8:11)])
df2nn$Mean <- c(df2n$Mean, df2n$Mean_Smoothed)
df2nn$Mean_Type <- rep(c("Simulated Mean", "Smoothed Mean"), each = dim(df2n)[1])
df2nn$Mean_Type <- factor(df2nn$Mean_Type, levels = c("Simulated Mean", "Smoothed Mean"))
df2nn$Szenario <- factor(df2nn$Szenario)
df2nn$Szenario <- recode_factor(df2nn$Szenario, "1" = "Scenario 1", "2" = "Scenario 2", "3" = "Scenario 3")

dfnn <- rbind(df1nn, df2nn)
dfnn$Pay_Type <- factor(c(rep("Out of the Pocket", dim(df1nn)[1]), rep("Social Welfare", dim(df2nn)[1])))

png("Results/Total Costs over Time.png", width = 3600, height = 1800, res = 300)
g1 <- ggplot(dfnn %>% subset(Type == "all"), aes(x = Age, y = Mean)) +
  geom_line(aes(linetype = Pay_Type, color = Szenario), cex = 1.5) + theme_bw() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + 
  facet_grid(~ Mean_Type) + scale_y_continuous(breaks = seq(0, 18000, 2000), limits = c(0, 18000)) + 
  ggtitle("Mean total Costs until Death by Age") +
  scale_linetype_manual(values = c("Out of the Pocket" = "solid", "Social Welfare" = "dotted"))
g1
g1$labels$x <- "Age [y]"
g1$labels$y <- "Mean total Costs until Death [CHF]"
g1$labels$colour <- ""
g1$labels$linetype <- ""
g1
dev.off()

# By Cost Type OOP
df1nn1 <- df1nn %>% subset(Type != "all")  %>% subset(Mean_Type == "Smoothed Mean")
df1nn1$Type <- factor(df1nn1$Type)
df1nn1$Type <- recode_factor(df1nn1$Type, "control" = "Gingivitis & Prevention", "disease" = "Secondary Damage", "perio" = "Periodontitis")
df1nn1$Type <- factor(df1nn1$Type, levels = c("Gingivitis & Prevention", "Periodontitis", "Secondary Damage"))

png("Results/Total Costs by Cost Type OOP.png", width = 3600, height = 1800, res = 300)
g2 <- ggplot(df1nn1, aes(x = Age, y = Mean, fill = Type)) + geom_area(alpha = 0.8) + theme_bw() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + 
  facet_grid(~Szenario) + scale_y_continuous(breaks = seq(0, 18000, 2000), limits = c(0, 18000)) + 
  ggtitle("Mean total Costs by Cost Type out of the Pocket until Death") + scale_fill_manual(values=c("#e3c1d0", "#E69F00", "#56B4E9"))
g2
g2$labels$x <- "Age [y]"
g2$labels$y <- "Mean total Costs until Death [CHF]"
g2$labels$fill <- ""
g2
dev.off()

# By Cost Type SW
df2nn1 <- df2nn %>% subset(Type != "all")  %>% subset(Mean_Type == "Smoothed Mean")
df2nn1$Type <- factor(df2nn1$Type)
df2nn1$Type <- recode_factor(df2nn1$Type, "control" = "Gingivitis & Prevention", "disease" = "Secondary Damage", "perio" = "Periodontitis")
df2nn1$Type <- factor(df2nn1$Type, levels = c("Gingivitis & Prevention", "Periodontitis", "Secondary Damage"))

png("Results/Total Costs by Cost Type SW.png", width = 3600, height = 1800, res = 300)
g2 <- ggplot(df2nn1, aes(x = Age, y = Mean, fill = Type)) + geom_area(alpha = 0.8) + theme_bw() + theme(legend.position = "top", plot.title = element_text(hjust = 0.5)) + 
  facet_grid(~Szenario) + scale_y_continuous(breaks = seq(0, 18000, 2000), limits = c(0, 18000)) + 
  ggtitle("Mean total Costs by Cost Type paid by Social Welfare until Death") + scale_fill_manual(values=c("#e3c1d0", "#E69F00", "#56B4E9"))
g2
g2$labels$x <- "Age [y]"
g2$labels$y <- "Mean total Costs until Death [CHF]"
g2$labels$fill <- ""
g2
dev.off()
