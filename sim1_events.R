library(Rcpp)
library(RcppArmadillo)
sourceCpp("Auxiliaries/cpp_all_final.cpp")

# death tables for men and women
death_men <- read.table("Simulation_Data/death_tables_men_CH.csv", sep = ";", header = T)
death_women <- read.table("Simulation_Data/death_tables_women_CH.csv", sep = ";", header = T)

# start sim
summ_mat <- NULL

# sim
B <- 200000
set.seed(8828)

# loop over scenarios
for(j in 1:3) {

  print(paste0("Scenario = ", j))

  # probabilities and costs
  path <- paste0("Scenarios/PDC", j, ".R")
  source(path)

  # loop over starting ages
  for(k in 35:100){

    print(paste0("Age = ", k))
    age <- k

    # simulation parameters
    heal <- NULL
    ungin <- NULL
    mangin <- NULL
    perio <- NULL
    start_perio <- NULL
    count_out <- NULL
    start_count_out <- NULL
    undiagnosedperio <- NULL
    step1 <- NULL
    step2 <- NULL
    step3 <- NULL
    step4 <- NULL
    unmanagedperio <- NULL
    unmanagedperio_step <- NULL
    start_unmanagedperio <- NULL
    death_age <- NULL

    for(i in 1:B) {

      sim <- Life_Simulation(age, T, T, T, T, surv_men = death_men$Q, surv_women = death_women$Q, probabilities = P)
      heal[i] <- sim$healthy
      ungin[i] <- sim$undiagnosedgingi
      mangin[i] <- sim$managedgingi
      perio[i] <- sim$perio
      start_perio[i] <- sim$start_perio
      count_out[i] <- sim$count_out
      start_count_out[i] <- sim$start_count_out
      undiagnosedperio[i] <- sim$perio_count_undiagnosedperio
      step1[i] <- sim$perio_count_step1
      step2[i] <- sim$perio_count_step2
      step3[i] <- sim$perio_count_step3
      step4[i] <- sim$perio_count_step4
      unmanagedperio[i] <- sim$perio_is_unmanaged
      unmanagedperio_step[i] <- sim$perio_count_unmanagedperio
      start_unmanagedperio[i] <- sim$start_count_unmanagedperio
      death_age[i] <- sim$death_age

    }

    dff <- data.frame(heal, ungin, mangin, perio, start_perio, count_out, start_count_out, undiagnosedperio, step1, step2, step3, step4, unmanagedperio, unmanagedperio_step, start_unmanagedperio, death_age)

    save_path <- paste0("Results/Events_per_Age/sim_scenario_", j, "_age_", k, ".rds")

    saveRDS(dff, save_path)

  }

}

