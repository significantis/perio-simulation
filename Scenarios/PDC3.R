# Probabilities
P <- list(
  
  # starting states
  p_start_perio_young = 0.05, # 1/2
  p_start_perio_old = 0.15, # 1/2
  p_start_undiagnosedgingi_young = 0.222, # new steady state
  p_start_managedgingi_young = 0.222, # new steady state
  p_start_undiagnosedgingi_old = 0.1875, # new steady state
  p_start_managedgingi_old = 0.1875, # new steady state
  prevalence_type_II = 0.50,
  p_start_unmanaged_when_perio = 0.15,
  p_start_undiagnosedperio = 0.20,
  p_start_step1_young = 0.01,
  p_start_step1_old = 0.007,
  p_start_step3 = 0.12,
  p_start_step4 = 0.667,
  
  # hg-cycle
  healthy_to_undiagnosedgingi_young = 0.38, # much better
  healthy_to_undiagnosedgingi_old = 0.285, # much better
  undiagnosedgingi_to_newperio_young = 0.0055, # better
  undiagnosedgingi_to_newperio_old = 0.0081, # better
  managedgingi_to_undiagnosedgingi = 0.05, # much better
  
  # p-triage
  undiagnosedperio_to_step1_young = 0.041,
  undiagnosedperio_to_step1_old = 0.015,
  undiagnosedperio_to_unmanagedperio_young = 0.029, # bit higher
  undiagnosedperio_to_unmanagedperio_old = 0.025,
  
  # p-cycle
  step_unmanagedperio = 0.0018,
  step1_to_step2 = 0.617,
  step1_to_step4 = 0.127,
  step2_to_step4 = 0.81,
  step3_to_step4 = 0.783,
  step4_to_step4 = 0.858
)

# OOP primary costs
C <- list(
  Healthy = 50,
  Ungin = 100, # higher
  Mangin = 100, # higher
  Undiagperio = 0,
  Step1 = 1200,
  Step2 = 2500,
  Step3 = 3000,
  Step4 = 150,
  Unmanperio = 0
)

# OOP secondary costs
SC <- list(
  Healthy = 0,
  Ungin = 20,
  Mangin = 20,
  Undiagperio = 150,
  Step1 = 150,
  Step2 = 150,
  Step3 = 150,
  Step4 = 150,
  Unmanperio = 250
)

# # SW primary costs
# C <- list(
#   Healthy = 40,
#   Ungin = 80,
#   Mangin = 80,
#   Undiagperio = 0,
#   Step1 = 600,
#   Step2 = 800,
#   Step3 = 1000,
#   Step4 = 120,
#   Unmanperio = 0
# )
# 
# # SW secondary costs
# SC <- list(
#   Healthy = 0,
#   Ungin = 15,
#   Mangin = 15,
#   Undiagperio = 120,
#   Step1 = 120,
#   Step2 = 120,
#   Step3 = 120,
#   Step4 = 120,
#   Unmanperio = 200
# )