pot <- function(x, grade = 4) {

  out <- NULL

  for(i in 0:grade) {

    out <- c(x^i, out)

  }

  out

}

poly_y <- function(df, ps = c(50, 90)) {

  adds <- length(ps)
  grade1 <- 3 + adds - 1
  df_all <- df
  df_new <- NULL

  df <- df[order(df$Szenario, df$Type, df$Age), ]

  for(k in unique(df_all$Szenario)) {

    for(j in unique(df_all$Type)) {

      df <- df_all %>% subset(Szenario == k) %>% subset(Type == j)

      pot_vec <- c(pot(64.5, grade = grade1), pot(35, grade = grade1), pot(100, grade = grade1))
      b1 <- c(df$Mean[df$Age == 64] + (df$Mean[df$Age == 65] - df$Mean[df$Age == 64])/2,
              df$Mean[df$Age == 35],
              df$Mean[df$Age == 100])

      for(i in 1:length(ps)) {

        pot_vec <- c(pot_vec, pot(ps[i], grade = grade1))
        b1 <- c(b1, df$Mean[df$Age == ps[i]])

      }

      m1 <- matrix(pot_vec, ncol = grade1 + 1 , byrow = T)
      s <- solve(m1, b1)
      ss <- rev(s)
      y1 <- 0

      for(i in 0:(length(ss) - 1)) {

        y1 <- y1 + ss[i + 1] * df$Age^i

      }

      df$Mean_Smoothed <- y1
      df_new <- rbind(df_new, df)

    }

  }

  df_new

}

prevalence_and_steady_state <- function(death_men, death_women, count_men, count_women, SP = P, young = T, old = T, absorbing_summary = T, steady_state = T, delta = 1, set_seed = 2252, B = 100000) {
  
  # prevalence df
  df_prevalences <- NULL
  
  # steady state df 
  df_ss <- NULL
  
  # absorbing summary
  df_as <- NULL
  
  # young people
  if(young) {
    
    # number of young men and women (35-64)
    cmen1 <- count_men$Anzahl[count_men$Alter %in% 35:64] 
    cwomen1 <- count_women$Anzahl[count_women$Alter %in% 35:64] 
    
    # probabilities to draw a man/woman of age x in the pool of 35-64 years old
    p1 <- (cmen1 + cwomen1) / (sum(cmen1) + sum(cwomen1))
    
    set.seed(set_seed)
    
    # simulation parameters
    heal <- NULL
    ungin <- NULL
    mangin <- NULL
    perio <- NULL
    start_perio <- NULL
    count_out <- NULL
    start_count_out <- NULL
    start_undiagnosedperio <- NULL
    undiagnosedperio <- NULL
    step1 <- NULL
    step2 <- NULL
    step3 <- NULL
    step4 <- NULL
    unmanagedperio_count <- NULL
    is_unmanaged <- NULL
    start_unmanagedperio <- NULL
    death_age <- NULL
    sim_age <- NULL
    
    # simulation of young patients
    for(i in 1:B) {
      
      # sample a person (age) from 35 to 64 years old
      age <- sample(35:64, 1, F, prob = p1)
      
      sim <- Life_Simulation(age, T, T, F, F, surv_men = death_men$Q, surv_women = death_women$Q, probabilities = P)
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
      unmanagedperio_count[i] <- sim$perio_count_unmanagedperio
      is_unmanaged[i] <- sim$perio_is_unmanaged
      start_unmanagedperio[i] <- sim$start_count_unmanagedperio
      
    }
    
    # simulated prevalences
    hgg_young <- round(100 * 
                         c(sum(heal) / (sum(mangin) + sum(heal) + sum(ungin)), 
                           sum(ungin) / (sum(mangin) + sum(heal) + sum(ungin)),
                           sum(mangin) / (sum(mangin) + sum(heal) + sum(ungin))), 1) 
    perio_prev_young <- round(100 * sum(perio) / B, 1) 
    perio_prev_start_young <- round(100 * sum(start_perio) / B, 1)
    perio_count_out_young <- round(100 * sum(count_out)/ B, 1) 
    perio_start_count_out_young <- round(100 * sum(start_count_out) / B, 1) 
    perio_prev_unm_young <- round(100 * sum(is_unmanaged) / sum(perio), 1) 
    perio_prev_start_unm_young <- round(100 * sum(start_unmanagedperio) / sum(start_perio), 1) 
    pc_young <- round(100 * c(sum(undiagnosedperio) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                              sum(step1) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                              sum(step2) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                              sum(step3) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                              sum(step4) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4))), 1)
    
    df_p <- data.frame(Age_Category = rep("35-64", 13),
                       State = c("Perio", "Type II Perio", "Non-Perio", "Healthy", "Undiag. Gingi", "Diag. Gingi", "Unman. Perio", "Man. Perio", "Undiag. Perio", "Step 1", "Step 2", "Step 3", "Step 4"),
                       Level = c(rep("Population", 3), rep("Non-Perio", 3), rep("Perio", 2), rep("Man. Perio", 5)),
                       Simulated_Prevalence = c(perio_count_out_young + perio_prev_young, perio_prev_young, 100 - (perio_count_out_young + perio_prev_young), hgg_young, perio_prev_unm_young, 100 - perio_prev_unm_young, pc_young))
    
    df_prevalences <- rbind(df_prevalences, df_p)
    
    # check for steady state
    if(steady_state) {
      
      start_prev_young <- 100 * c(1 - (SP$p_start_undiagnosedgingi_young + SP$p_start_managedgingi_young), SP$p_start_undiagnosedgingi_young, SP$p_start_managedgingi_young,
                                  SP$p_start_undiagnosedperio, SP$p_start_step1_young, 1 - SP$p_start_undiagnosedperio - SP$p_start_step1_young - SP$p_start_step3 - SP$p_start_step4, SP$p_start_step3, SP$p_start_step4)
      
      df_sss <- df_p %>% subset(State %in% c("Healthy", "Undiag. Gingi", "Diag. Gingi", "Undiag. Perio", "Step 1", "Step 2", "Step 3", "Step 4"))
      df_sss$Starting_Prevalence <- start_prev_young
      df_sss$Steady_State <- ifelse(abs(df_sss$Starting_Prevalence - df_sss$Simulated_Prevalence) <= delta, "Steady", "Shift")
      
      df_ss <- rbind(df_ss, df_sss)
      
    }
    
    # absorbing states
    if(absorbing_summary) {
      
      start_abs_young <- 100 * c(SP$p_start_perio_young, SP$p_start_perio_young * SP$prevalence_type_II, SP$p_start_unmanaged_when_perio)
      df_ass <- df_p %>% subset(State %in% c("Perio", "Type II Perio", "Unman. Perio"))
      df_ass$Starting_Prevalence <- start_abs_young
      
      df_as <- rbind(df_as, df_ass)
      
    }
    
  }
  
  if(old) {
    
    # number of old men and women (65-100)
    cmen2 <- count_men$Anzahl[count_men$Alter %in% 65:100] 
    cwomen2 <- count_women$Anzahl[count_women$Alter %in% 65:100] 
    
    # probabilities to draw a man/woman of age x in the pool of 65-100 years old
    p2 <- (cmen2 + cwomen2) / (sum(cmen2) + sum(cwomen2))
    
    set.seed(set_seed)
    
    # simulation parameters
    heal <- NULL
    ungin <- NULL
    mangin <- NULL
    perio <- NULL
    start_perio <- NULL
    count_out <- NULL
    start_count_out <- NULL
    start_undiagnosedperio <- NULL
    undiagnosedperio <- NULL
    step1 <- NULL
    step2 <- NULL
    step3 <- NULL
    step4 <- NULL
    unmanagedperio_count <- NULL
    is_unmanaged <- NULL
    start_unmanagedperio <- NULL
    death_age <- NULL
    sim_age <- NULL
    
    # simulation of old patients
    for(i in 1:B) {
      
      # sample a person (age) from 65 to 100 years old
      age <- sample(65:100, 1, F, prob = p2)
      
      sim <- Life_Simulation(age, F, F, T, T, surv_men = death_men$Q, surv_women = death_women$Q, probabilities = P)
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
      unmanagedperio_count[i] <- sim$perio_count_unmanagedperio
      is_unmanaged[i] <- sim$perio_is_unmanaged
      start_unmanagedperio[i] <- sim$start_count_unmanagedperio
      
    }
    
    # simulated prevalences
    hgg_old <- round(100 * 
                       c(sum(heal) / (sum(mangin) + sum(heal) + sum(ungin)), 
                         sum(ungin) / (sum(mangin) + sum(heal) + sum(ungin)),
                         sum(mangin) / (sum(mangin) + sum(heal) + sum(ungin))), 1) 
    perio_prev_old <- round(100 * sum(perio) / B, 1) 
    perio_prev_start_old <- round(100 * sum(start_perio) / B, 1) 
    perio_count_out_old <- round(100 * sum(count_out)/ B, 1) 
    perio_start_count_out_old <- round(100 * sum(start_count_out) / B, 1) 
    perio_prev_unm_old <- round(100 * sum(is_unmanaged) / sum(perio), 1) 
    perio_prev_start_unm_old <- round(100 * sum(start_unmanagedperio) / sum(start_perio), 1) 
    pc_old <- round(100 * c(sum(undiagnosedperio) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                            sum(step1) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                            sum(step2) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                            sum(step3) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4)),
                            sum(step4) / (sum(undiagnosedperio) + sum(step1) + sum(step2) + sum(step3) + sum(step4))), 1)
    
    df_p <- data.frame(Age_Category = rep("65+", 13),
                       State = c("Perio", "Type II Perio", "Non-Perio", "Healthy", "Undiag. Gingi", "Diag. Gingi", "Unman. Perio", "Man. Perio", "Undiag. Perio", "Step 1", "Step 2", "Step 3", "Step 4"),
                       Level = c(rep("Population", 3), rep("Non-Perio", 3), rep("Perio", 2), rep("Man. Perio", 5)),
                       Simulated_Prevalence = c(perio_count_out_old + perio_prev_old, perio_prev_old, 100 - (perio_count_out_old + perio_prev_old), hgg_old, perio_prev_unm_old, 100 - perio_prev_unm_old, pc_old))
    
    df_prevalences <- rbind(df_prevalences, df_p)
    
    # check for steady state
    if(steady_state) {
      
      start_prev_old <- 100 * c(1 - (SP$p_start_undiagnosedgingi_old + SP$p_start_managedgingi_old), SP$p_start_undiagnosedgingi_old, SP$p_start_managedgingi_old,
                                SP$p_start_undiagnosedperio, SP$p_start_step1_old, 1 - SP$p_start_undiagnosedperio - SP$p_start_step1_old - SP$p_start_step3 - SP$p_start_step4, SP$p_start_step3, SP$p_start_step4)
      
      df_sss <- df_p %>% subset(State %in% c("Healthy", "Undiag. Gingi", "Diag. Gingi", "Undiag. Perio", "Step 1", "Step 2", "Step 3", "Step 4"))
      df_sss$Starting_Prevalence <- start_prev_old
      df_sss$Steady_State <- ifelse(abs(df_sss$Starting_Prevalence - df_sss$Simulated_Prevalence) <= delta, "Steady", "Shift")
      
      df_ss <- rbind(df_ss, df_sss)
      
    }
    
    # absorbing states
    if(absorbing_summary) {
      
      start_abs_old <- 100 * c(SP$p_start_perio_old, SP$p_start_perio_old * SP$prevalence_type_II, SP$p_start_unmanaged_when_perio)
      df_ass <- df_p %>% subset(State %in% c("Perio", "Type II Perio", "Unman. Perio"))
      df_ass$Starting_Prevalence <- start_abs_old
      
      df_as <- rbind(df_as, df_ass)
      
    }
    
  }
  
  list(Prevalences = df_prevalences,
       Steady_States = df_ss,
       Absorbing_States = df_as)
  
}
