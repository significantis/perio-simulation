library(Rcpp)
library(RcppArmadillo)
library(tidyverse)
library(gcookbook)
sourceCpp("Auxiliaries/cpp_all_final.cpp")

# aux-functions
source("Auxiliaries/auxiliaries.R")

# probabilities and disutilities and costs
source("Scenarios/PDC3.R")

# death tables for men and women
death_men <- read.table("Simulation_Data/death_tables_men_CH.csv", sep = ";", header = T)
death_women <- read.table("Simulation_Data/death_tables_women_CH.csv", sep = ";", header = T)

# number of men and women is switzerland by age 0 to 101
count_men <- read.table("Simulation_Data/men_CH.csv", sep = ";", header = T)
count_women <- read.table("Simulation_Data/women_CH.csv", sep = ";", header = T)

prevalence_and_steady_state(death_men, death_women, count_men, count_women)
