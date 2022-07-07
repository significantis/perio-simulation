# start sim
summ_mat <- NULL

# loop over scenarios
for(j in 1:3) {
  
  print(paste0("Scenario = ", j))
  
  # loop over starting ages
  for(k in 35:100){
    
    print(paste0("Age = ", k))
    
    path1 <- paste0("Results/Events_per_Age/sim_scenario_", j, "_age_", k, ".rds")
    df <- readRDS(path1)
    
    path2 <- paste0("Scenarios/PDC", j, ".R")
    source(path2)
    
    costs <- (
      C$Healthy * df$heal +
        C$Ungin * df$ungin +
        C$Mangin * df$mangin +
        C$Undiagperio * df$undiagnosedperio +
        C$Step1 * df$step1  +
        C$Step2 * df$step2  +
        C$Step3 * df$step3  +
        C$Step4 * df$step4  +
        C$Unmanperio * df$unmanagedperio_step
    )
    
    costs_control <- (
      C$Healthy * df$heal +
        C$Ungin * df$ungin +
        C$Mangin * df$mangin
    )
    
    costs_perio <- (
      C$Undiagperio * df$undiagnosedperio +
        C$Step1 * df$step1  +
        C$Step2 * df$step2  +
        C$Step3 * df$step3  +
        C$Step4 * df$step4  +
        C$Unmanperio * df$unmanagedperio_step
    )
    
    disease_costs <- c(
      SC$Healthy * df$heal +
        SC$Ungin * df$ungin +
        SC$Mangin * df$mangin +
        SC$Undiagperio * df$undiagnosedperio +
        SC$Step1 * df$step1  +
        SC$Step2 * df$step2  +
        SC$Step3 * df$step3  +
        SC$Step4 * df$step4  +
        SC$Unmanperio * df$unmanagedperio_step
    )
    
    death_age <- df$death_age
    count_out <- df$count_out
    
    summ_vec <- c(summary(costs + disease_costs), sd = round(sd(costs + disease_costs), 2), UCB = quantile(costs + disease_costs, .95), mean_exp_age = round(mean(death_age[count_out != 1]), 2))
    summ_vec2 <- c(summary(costs_control), sd = round(sd(costs_control), 2), UCB = quantile(costs_control, .95), mean_exp_age = round(mean(death_age[count_out != 1]), 2))
    summ_vec3 <- c(summary(costs_perio), sd = round(sd(costs_perio), 2), UCB = quantile(costs_perio, .95), mean_exp_age = round(mean(death_age[count_out != 1]), 2))
    summ_vec4 <- c(summary(disease_costs), sd = round(sd(disease_costs), 2), UCB = quantile(disease_costs, .95), mean_exp_age = round(mean(death_age[count_out != 1]), 2))
    summ_tot <- round(rbind(summ_vec, summ_vec2, summ_vec3, summ_vec4), 2)
    summ_tot <- cbind(Szenario = j, Type = c("all", "control", "perio", "disease"), age = k, summ_tot)
    colnames(summ_tot) <- c("Szenario", "Type", "Age", "Min", "Q1", "Median", "Mean", "Q3", "Max", "SD", "UCB_95", "Mean_Exp_Age")
    summ_mat <- rbind(summ_mat, data.frame(summ_tot))
    
  }
  
}

write.table(summ_mat, "Results/Simulation_OOP.csv", sep = ";", row.names = F)
