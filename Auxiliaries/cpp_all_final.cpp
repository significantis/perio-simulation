// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadilloExtensions/sample.h>

using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector cpp_healthy_jump( int age = 35,
                                  double healthy_to_undiagnosedgingi_young = 0.756,
                                  double healthy_to_undiagnosedgingi_old = 0.503
) {
  
  CharacterVector states1 = {"healthy", "undiagnosedgingi"};
  CharacterVector sam(0);
  
  if(age < 65) {
    
    NumericVector prob1 = {1 - healthy_to_undiagnosedgingi_young, healthy_to_undiagnosedgingi_young};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  } else {
    
    NumericVector prob1 = {1 - healthy_to_undiagnosedgingi_old, healthy_to_undiagnosedgingi_old};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  }
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector cpp_undiagnosedgingi_jump( int age = 35,
                                           double undiagnosedgingi_to_newperio_young = 0.007,
                                           double undiagnosedgingi_to_newperio_old = 0.011
                                             
) {
  
  CharacterVector states1 = {"managedgingi", "newperio"};
  CharacterVector sam(0);
  
  if(age < 65) {
    
    NumericVector prob1 = {1 - undiagnosedgingi_to_newperio_young, undiagnosedgingi_to_newperio_young};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  } else {
    
    NumericVector prob1 = {1 - undiagnosedgingi_to_newperio_old, undiagnosedgingi_to_newperio_old};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  }
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector cpp_managedgingi_jump( double managedgingi_to_undiagnosedgingi = 0.5
) {
  
  CharacterVector states1 = {"healthy", "undiagnosedgingi"};
  NumericVector prob1 = {1 - managedgingi_to_undiagnosedgingi, managedgingi_to_undiagnosedgingi};
  CharacterVector sam(0);
  
  sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector HG_Cycle( CharacterVector state,
                          int age,
                          double healthy_to_undiagnosedgingi_young = 0.756,
                          double healthy_to_undiagnosedgingi_old = 0.503,
                          double undiagnosedgingi_to_newperio_young = 0.007,
                          double undiagnosedgingi_to_newperio_old = 0.011,
                          double managedgingi_to_undiagnosedgingi = 0.5
) {
  
  CharacterVector sam(0);
  
  if(state[0] == "healthy") {
    
    sam = cpp_healthy_jump(age, 
                           healthy_to_undiagnosedgingi_young,
                           healthy_to_undiagnosedgingi_old);
    
  }
  
  if(state[0] == "undiagnosedgingi") {
    
    sam = cpp_undiagnosedgingi_jump(age,
                                    undiagnosedgingi_to_newperio_young,
                                    undiagnosedgingi_to_newperio_old);
    
  }
  
  if(state[0] == "managedgingi") {
    
    sam = cpp_managedgingi_jump(managedgingi_to_undiagnosedgingi);
    
  }
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector cpp_undiagnosedperio_jump( int age,
                                           double undiagnosedperio_to_step1_young = 0.043,
                                           double undiagnosedperio_to_step1_old = 0.015,
                                           double undiagnosedperio_to_unmanagedperio_young = 0.028,
                                           double undiagnosedperio_to_unmanagedperio_old = 0.023
) {
  
  CharacterVector states1 = {"undiagnosedperio", "step1", "unmanagedperio"};
  CharacterVector sam(0);
  
  if(age < 65) {
    
    NumericVector prob1 = {1 - undiagnosedperio_to_step1_young - undiagnosedperio_to_unmanagedperio_young, undiagnosedperio_to_step1_young, undiagnosedperio_to_unmanagedperio_young};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  } else {
    
    NumericVector prob1 = {1 - undiagnosedperio_to_step1_old - undiagnosedperio_to_unmanagedperio_old, undiagnosedperio_to_step1_old, undiagnosedperio_to_unmanagedperio_old};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  }
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector cpp_newperio_handler( int age,
                                      double prevalence_type_II = 0.50,
                                      double undiagnosedperio_to_step1_young = 0.043,
                                      double undiagnosedperio_to_step1_old = 0.015,
                                      double undiagnosedperio_to_unmanagedperio_young = 0.028,
                                      double undiagnosedperio_to_unmanagedperio_old = 0.023
) {
  
  CharacterVector sam(0);
  CharacterVector perio(0);
  CharacterVector states1 = {"newperio", "out"};
  NumericVector prob1 = {prevalence_type_II, 1 - prevalence_type_II};
  perio = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  if(perio[0] == "newperio") {
    
    sam = cpp_undiagnosedperio_jump(age,
                                    undiagnosedperio_to_step1_young,
                                    undiagnosedperio_to_step1_old,
                                    undiagnosedperio_to_unmanagedperio_young,
                                    undiagnosedperio_to_unmanagedperio_old);
    
  } else {
    
    sam = "out";
    
  }
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector cpp_step1_jump( double step_unmanagedperio = 0.0018,
                                double step1_to_step2 = 0.617,
                                double step1_to_step4 = 0.127
) {
  
  CharacterVector states1 = {"step", "unmanagedperio"};
  CharacterVector sam1(0);
  CharacterVector states2 = {"step1", "step2", "step4"};
  CharacterVector sam2(0);
  
  NumericVector prob1 = {1 - step_unmanagedperio, step_unmanagedperio};
  sam1 = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  if(sam1[0] == "step") {
    
    NumericVector prob2 = {1 - step1_to_step2 - step1_to_step4, step1_to_step2, step1_to_step4};
    sam2 = RcppArmadillo::sample(states2, 1, FALSE, prob2); 
    
  } else {
    
    sam2 = "unmanagedperio";
    
  }
  
  return sam2;
  
}

// [[Rcpp::export]]
CharacterVector cpp_step2_jump( double step_unmanagedperio = 0.0018,
                                double step2_to_step4 = 0.81
) {
  
  CharacterVector states1 = {"step", "unmanagedperio"};
  CharacterVector sam1(0);
  CharacterVector states2 = {"step3", "step4"};
  CharacterVector sam2(0);
  
  NumericVector prob1 = {1 - step_unmanagedperio, step_unmanagedperio};
  sam1 = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  if(sam1[0] == "step") {
    
    NumericVector prob2 = {1 - step2_to_step4, step2_to_step4};
    sam2 = RcppArmadillo::sample(states2, 1, FALSE, prob2); 
    
  } else {
    
    sam2 = "unmanagedperio";
    
  }
  
  return sam2;
  
}

// [[Rcpp::export]]
CharacterVector cpp_step3_jump( double step_unmanagedperio = 0.0018,
                                double step3_to_step4 = 0.783
) {
  
  CharacterVector states1 = {"step", "unmanagedperio"};
  CharacterVector sam1(0);
  CharacterVector states2 = {"step3", "step4"};
  CharacterVector sam2(0);
  
  NumericVector prob1 = {1 - step_unmanagedperio, step_unmanagedperio};
  sam1 = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  if(sam1[0] == "step") {
    
    NumericVector prob2 = {1 - step3_to_step4, step3_to_step4};
    sam2 = RcppArmadillo::sample(states2, 1, FALSE, prob2); 
    
  } else {
    
    sam2 = "unmanagedperio";
    
  }
  
  return sam2;
  
}

// [[Rcpp::export]]
CharacterVector cpp_step4_jump( double step_unmanagedperio = 0.0018,
                                double step4_to_step4 = 0.858
) {
  
  CharacterVector states1 = {"step", "unmanagedperio"};
  CharacterVector sam1(0);
  CharacterVector states2 = {"step3", "step4"};
  CharacterVector sam2(0);
  
  NumericVector prob1 = {1 - step_unmanagedperio, step_unmanagedperio};
  sam1 = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  if(sam1[0] == "step") {
    
    NumericVector prob2 = {1 - step4_to_step4, step4_to_step4};
    sam2 = RcppArmadillo::sample(states2, 1, FALSE, prob2); 
    
  } else {
    
    sam2 = "unmanagedperio";
    
  }
  
  return sam2;
  
}

// [[Rcpp::export]]
CharacterVector P_Cycle( CharacterVector state,
                         int age, 
                         double undiagnosedperio_to_step1_young = 0.043,
                         double undiagnosedperio_to_step1_old = 0.015,
                         double undiagnosedperio_to_unmanagedperio_young = 0.028,
                         double undiagnosedperio_to_unmanagedperio_old = 0.023, 
                         double step_unmanagedperio = 0.0018,
                         double step1_to_step2 = 0.617,
                         double step1_to_step4 = 0.127,
                         double step2_to_step4 = 0.81,
                         double step3_to_step4 = 0.783,
                         double step4_to_step4 = 0.858
) {
  
  CharacterVector sam(0);
  
  if(state[0] == "undiagnosedperio") {
    
    sam = cpp_undiagnosedperio_jump(age,
                                    undiagnosedperio_to_step1_young,
                                    undiagnosedperio_to_step1_old,
                                    undiagnosedperio_to_unmanagedperio_young,
                                    undiagnosedperio_to_unmanagedperio_old);
    
  }
  
  if(state[0] == "step1") {
    
    sam = cpp_step1_jump(step_unmanagedperio,
                         step1_to_step2,
                         step1_to_step4);
    
  }
  
  if(state[0] == "step2") {
    
    sam = cpp_step2_jump(step_unmanagedperio, 
                         step2_to_step4);
    
  }
  
  if(state[0] == "step3") {
    
    sam = cpp_step3_jump(step_unmanagedperio,
                         step3_to_step4);
    
  }
  
  if(state[0] == "step4") {
    
    sam = cpp_step4_jump(step_unmanagedperio,
                         step4_to_step4);
    
  }
  
  if(state[0] == "unmanagedperio") {
    
    sam = "unmanagedperio";
    
  }
  
  return sam;
  
}

// [[Rcpp::export]]
CharacterVector Starting_State( int age,
                                double p_start_perio_young = 0.1,
                                double p_start_perio_old = 0.3,
                                double p_start_undiagnosedgingi_young = 0.375,
                                double p_start_managedgingi_young = 0.375,
                                double p_start_undiagnosedgingi_old = 0.333,
                                double p_start_managedgingi_old = 0.333,
                                double prevalence_type_II = 0.50,
                                double p_start_unmanaged_when_perio = 0.15,
                                double p_start_undiagnosedperio = 0.20,
                                double p_start_step1_young = 0.01,
                                double p_start_step1_old = 0.007,
                                double p_start_step3 = 0.12,
                                double p_start_step4 = 0.667
) {
  
  CharacterVector states1 = {"non_perio", "startperio"};
  CharacterVector states2 = {"healthy", "undiagnosedgingi", "managedgingi"};
  CharacterVector states31 = {"perio", "out"};
  CharacterVector states32 = {"cycle", "unmanaged"};
  CharacterVector states33 = {"undiagnosedperio", "step1", "step2", "step3", "step4"};
  CharacterVector sam(0);
  CharacterVector subsam(0);
  CharacterVector subsubsam(0);
  CharacterVector out(0);
  
  if(age < 65) {
    
    NumericVector prob1 = {1 - p_start_perio_young, p_start_perio_young};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  } else {
    
    NumericVector prob1 = {1 - p_start_perio_old, p_start_perio_old};
    sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
    
  }
  
  if(sam[0] == "non_perio") {
    
    if(age < 65) {
      
      NumericVector prob2 = {1 - p_start_undiagnosedgingi_young - p_start_managedgingi_young, p_start_undiagnosedgingi_young, p_start_managedgingi_young};
      out = RcppArmadillo::sample(states2, 1, FALSE, prob2);
      
    } else {
      
      NumericVector prob2 = {1 - p_start_undiagnosedgingi_old - p_start_managedgingi_old, p_start_undiagnosedgingi_old, p_start_managedgingi_old};
      out = RcppArmadillo::sample(states2, 1, FALSE, prob2);
      
    }
    
  } else {
    
    NumericVector prob31 = {prevalence_type_II, 1 - prevalence_type_II};
    subsam = RcppArmadillo::sample(states31, 1, FALSE, prob31);
    
    if(subsam[0] == "perio") {
      
      NumericVector prob32 = {1 - p_start_unmanaged_when_perio, p_start_unmanaged_when_perio};
      subsubsam = RcppArmadillo::sample(states32, 1, FALSE, prob32);
      
      if(subsubsam[0] == "cycle") {
        
        if(age < 65) {
          
          NumericVector prob33 = {p_start_undiagnosedperio, p_start_step1_young, 1 - p_start_undiagnosedperio - p_start_step1_young - p_start_step3 - p_start_step4, p_start_step3, p_start_step4};
          out = RcppArmadillo::sample(states33, 1, FALSE, prob33);
          
        } else {
          
          NumericVector prob33 = {p_start_undiagnosedperio, p_start_step1_old, 1 - p_start_undiagnosedperio - p_start_step1_old - p_start_step3 - p_start_step4, p_start_step3, p_start_step4};
          out = RcppArmadillo::sample(states33, 1, FALSE, prob33);
          
        }
        
      } else {
        
        out = "unmanagedperio";
        
      }
      
    } else {
      
      out = "out";
      
    }
    
  }
  
  return out;
  
}

// // [[Rcpp::export]]
// bool Is_Alive( CharacterVector sex,
//                int age,
//                double turnus,
//                NumericVector surv_men,
//                NumericVector surv_women
// ) {
//   
//   bool out = TRUE;
//   CharacterVector sam(0);
//   double s_current = 0;
//   double s_year_now_m = surv_men[age];
//   double s_year_next_m = surv_men[age + 1]; 
//   double s_year_now_w = surv_women[age];
//   double s_year_next_w = surv_women[age + 1];
//   
//   if(age < 100) {
//     
//     if(sex[0] == "male") {
//       
//       s_current = s_year_now_m + (s_year_next_m - s_year_now_m) / 3 * turnus;
//       
//     }
//     
//     if(sex[0] == "female") {
//       
//       s_current = s_year_now_w + (s_year_next_w - s_year_now_w) / 3 * turnus;
//       
//     }
//     
//   } else {
//     
//     s_current = 1;
//     
//   }
//   
//   CharacterVector states1 = {"dead", "alive"};
//   NumericVector prob1 = {s_current, 1 - s_current};
//   sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
//   
//   if(sam[0] == "dead") {
//     
//     out = FALSE;
//     
//   }
//   
//   return out;
//   
// }

// [[Rcpp::export]]
bool Is_Alive( CharacterVector sex,
               int age,
               NumericVector surv_men,
               NumericVector surv_women
) {
  
  bool out = TRUE;
  CharacterVector sam(0);
  double s_current = 0;
  double s_year_now_m = surv_men[age - 1];
  double s_year_now_w = surv_women[age - 1];
  
  if(age < 100) {
    
    if(sex[0] == "male") {
      
      s_current = s_year_now_m;
      
    }
    
    if(sex[0] == "female") {
      
      s_current = s_year_now_w;
      
    }
    
  } else {
    
    s_current = 1;
    
  }
  
  CharacterVector states1 = {"dead", "alive"};
  NumericVector prob1 = {s_current, 1 - s_current};
  sam = RcppArmadillo::sample(states1, 1, FALSE, prob1);
  
  if(sam[0] == "dead") {
    
    out = FALSE;
    
  }
  
  return out;
  
}

// [[Rcpp::export]]
List Life_Simulation( int age,
                      bool young_HG,
                      bool young_perio,
                      bool old_HG,
                      bool old_perio,
                      NumericVector surv_men,
                      NumericVector surv_women,
                      List probabilities) {
  
  double p_start_perio_young = probabilities["p_start_perio_young"];
  double p_start_perio_old = probabilities["p_start_perio_old"];
  double p_start_undiagnosedgingi_young = probabilities["p_start_undiagnosedgingi_young"];
  double p_start_managedgingi_young = probabilities["p_start_managedgingi_young"];
  double p_start_undiagnosedgingi_old = probabilities["p_start_undiagnosedgingi_old"];
  double p_start_managedgingi_old = probabilities["p_start_managedgingi_old"];
  double prevalence_type_II = probabilities["prevalence_type_II"];
  double p_start_unmanaged_when_perio = probabilities["p_start_unmanaged_when_perio"];
  double p_start_undiagnosedperio = probabilities["p_start_undiagnosedperio"];
  double p_start_step1_young = probabilities["p_start_step1_young"];
  double p_start_step1_old = probabilities["p_start_step1_old"];
  double p_start_step3 = probabilities["p_start_step3"];
  double p_start_step4 = probabilities["p_start_step4"];
  
  CharacterVector state = Starting_State(age,
                                         p_start_perio_young,
                                         p_start_perio_old,
                                         p_start_undiagnosedgingi_young,
                                         p_start_managedgingi_young,
                                         p_start_undiagnosedgingi_old,
                                         p_start_managedgingi_old,
                                         prevalence_type_II,
                                         p_start_unmanaged_when_perio,
                                         p_start_undiagnosedperio,
                                         p_start_step1_young,
                                         p_start_step1_old,
                                         p_start_step3,
                                         p_start_step4);

  CharacterVector sexstates = {"male", "female"};
  NumericVector sexprobs = {0.5, 0.5};
  CharacterVector sex(0);
  sex = RcppArmadillo::sample(sexstates, 1, FALSE, sexprobs);
  
  int count_healthy = 0;
  int start_count_healthy = 0;
  int count_undiagnosedgingi = 0;
  int start_count_undiagnosedgingi = 0;
  int count_managedgingi = 0;
  int start_count_managedgingi = 0;
  int count_out = 0;
  int start_count_out = 0;
  int count_perio = 0;
  int start_count_perio = 0;
  int start_count_unmanagedperio = 0;

  int turnus = 0;
  bool alive = TRUE;
  
  CharacterVector perio_state(0);
  int count_perio_sub = 0;
  int perio_count_undiagnosedperio = 0;
  int perio_count_step1 = 0;
  int perio_count_step2 = 0;
  int perio_count_step3 = 0;
  int perio_count_step4 = 0;
  int perio_count_unmanagedperio = 0;
  int perio_is_unmanaged = 0;
  
  double death_age = 0;

  if(state[0] == "healthy") {
    
    count_healthy = count_healthy + 1;
    start_count_healthy = 1;
    
  } else {
    
    if(state[0] == "undiagnosedgingi") {
      
      count_undiagnosedgingi = count_undiagnosedgingi + 1;
      start_count_undiagnosedgingi = 1;
      
    } else {
      
      if(state[0] == "managedgingi") {
        
        count_managedgingi = count_managedgingi + 1;
        start_count_managedgingi = 1;
        
      } else {
        
        if(state[0] == "out") {
          
          count_out = count_out + 1;
          start_count_out = 1;
          
        } else {
          
          perio_state = state;
          count_perio = count_perio + 1;
          start_count_perio = 1;
          count_perio_sub = count_perio_sub + 1;
          
          if(perio_state[0] == "unmanagedperio") {
            
            start_count_unmanagedperio = 1;
            perio_count_unmanagedperio = perio_count_unmanagedperio + 1;
            perio_is_unmanaged = 1;
            
          }
          
        }
        
      }
      
    }
    
  }
  
  double healthy_to_undiagnosedgingi_young = probabilities["healthy_to_undiagnosedgingi_young"];
  double healthy_to_undiagnosedgingi_old = probabilities["healthy_to_undiagnosedgingi_old"];
  double undiagnosedgingi_to_newperio_young = probabilities["undiagnosedgingi_to_newperio_young"];
  double undiagnosedgingi_to_newperio_old = probabilities["undiagnosedgingi_to_newperio_old"];
  double managedgingi_to_undiagnosedgingi = probabilities["managedgingi_to_undiagnosedgingi"];
  double undiagnosedperio_to_step1_young = probabilities["undiagnosedperio_to_step1_young"];
  double undiagnosedperio_to_step1_old = probabilities["undiagnosedperio_to_step1_old"];
  double undiagnosedperio_to_unmanagedperio_young = probabilities["undiagnosedperio_to_unmanagedperio_young"];
  double undiagnosedperio_to_unmanagedperio_old = probabilities["undiagnosedperio_to_unmanagedperio_old"];
  
  if(young_HG) {
    
    while(alive && (age < 65) && ((state[0] == "healthy") || (state[0] == "undiagnosedgingi") || (state[0] == "managedgingi"))) {
      
      turnus = turnus + 1;
      
      if(turnus == 2) {
        
        alive = Is_Alive(sex, age, surv_men, surv_women);
        
      }
      
      if(turnus == 3) {
        
        turnus = 0;
        age = age + 1;
        
      }

      state = HG_Cycle(state,
                       age,
                       healthy_to_undiagnosedgingi_young,
                       healthy_to_undiagnosedgingi_old,
                       undiagnosedgingi_to_newperio_young,
                       undiagnosedgingi_to_newperio_old,
                       managedgingi_to_undiagnosedgingi
      );
      
      if(state[0] == "healthy") {
        
        count_healthy = count_healthy + 1;
        
      }
      
      if(state[0] == "undiagnosedgingi") {
        
        count_undiagnosedgingi = count_undiagnosedgingi + 1;
        
      }
      
      if(state[0] == "managedgingi") {
        
        count_managedgingi = count_managedgingi + 1;
        
      }
      
      if(state[0] == "newperio") {
        
        state = cpp_newperio_handler(age,
                                     prevalence_type_II,
                                     undiagnosedperio_to_step1_young,
                                     undiagnosedperio_to_step1_old,
                                     undiagnosedperio_to_unmanagedperio_young,
                                     undiagnosedperio_to_unmanagedperio_old);
        
        if(state[0] == "out") {
          
          count_out = count_out + 1;

        } else {
          
          perio_state = state;
          count_perio = count_perio + 1;
          count_perio_sub = count_perio_sub + 1;
          
        }
        
      }
      
    }
    
  }
  
  double step_unmanagedperio = probabilities["step_unmanagedperio"];
  double step1_to_step2 = probabilities["step1_to_step2"];
  double step1_to_step4 = probabilities["step1_to_step4"];
  double step2_to_step4 = probabilities["step2_to_step4"];
  double step3_to_step4 = probabilities["step3_to_step4"];
  double step4_to_step4 = probabilities["step4_to_step4"];
  
  if(young_perio) {
    
    while(alive && (age < 65) && (count_perio_sub > 0)) {
      
      turnus = turnus + 1;
      
      if(turnus == 2) {
        
        alive = Is_Alive(sex, age, surv_men, surv_women);
        
      }
      
      if(turnus == 3) {
        
        turnus = 0;
        age = age + 1;
        
      }

      perio_state = P_Cycle(perio_state,
                            age, 
                            undiagnosedperio_to_step1_young,
                            undiagnosedperio_to_step1_old,
                            undiagnosedperio_to_unmanagedperio_young,
                            undiagnosedperio_to_unmanagedperio_old, 
                            step_unmanagedperio,
                            step1_to_step2,
                            step1_to_step4,
                            step2_to_step4,
                            step3_to_step4,
                            step4_to_step4);
      
      if(perio_state[0] == "undiagnosedperio") {
        
        perio_count_undiagnosedperio = perio_count_undiagnosedperio + 1;
        
      }
      
      if(perio_state[0] == "step1") {
        
        perio_count_step1 = perio_count_step1 + 1;
        
      }
      
      if(perio_state[0] == "step2") {
        
        perio_count_step2 = perio_count_step2 + 1;
        
      }
      
      if(perio_state[0] == "step3") {
        
        perio_count_step3 = perio_count_step3 + 1;
        
      }
      
      if(perio_state[0] == "step4") {
        
        perio_count_step4 = perio_count_step4 + 1;
        
      }
      
      if(perio_state[0] == "unmanagedperio") {
        
        perio_count_unmanagedperio = perio_count_unmanagedperio + 1;
        perio_is_unmanaged = 1;

      }
      
    }
    
  }
  
  if(old_HG) {
    
    while(alive && (age >= 65) && ((state[0] == "healthy") || (state[0] == "undiagnosedgingi") || (state[0] == "managedgingi"))) {
      
      turnus = turnus + 1;
      
      if(turnus == 2) {
        
        alive = Is_Alive(sex, age, surv_men, surv_women);
        
      }
      
      if(turnus == 3) {
        
        turnus = 0;
        age = age + 1;
        
      }

      state = HG_Cycle(state,
                       age,
                       healthy_to_undiagnosedgingi_young,
                       healthy_to_undiagnosedgingi_old,
                       undiagnosedgingi_to_newperio_young,
                       undiagnosedgingi_to_newperio_old,
                       managedgingi_to_undiagnosedgingi);
      
      if(state[0] == "healthy") {
        
        count_healthy = count_healthy + 1;
        
      }
      
      if(state[0] == "undiagnosedgingi") {
        
        count_undiagnosedgingi = count_undiagnosedgingi + 1;
        
      }
      
      if(state[0] == "managedgingi") {
        
        count_managedgingi = count_managedgingi + 1;
        
      }
      
      if(state[0] == "newperio") {
        
        state = cpp_newperio_handler(age,
                                     prevalence_type_II,
                                     undiagnosedperio_to_step1_young,
                                     undiagnosedperio_to_step1_old,
                                     undiagnosedperio_to_unmanagedperio_young,
                                     undiagnosedperio_to_unmanagedperio_old);
        
        if(state[0] == "out") {
          
          count_out = count_out + 1;
          
        } else {
          
          perio_state = state;
          count_perio = count_perio + 1;
          count_perio_sub = count_perio_sub + 1;
          
        }
        
      }
      
    }
    
  }
  
  if(old_perio) {
    
    while(alive && (age >= 65) && (count_perio_sub > 0)) {
      
      turnus = turnus + 1;
      
      if(turnus == 2) {
        
        alive = Is_Alive(sex, age, surv_men, surv_women);
        
      }
      
      if(turnus == 3) {
        
        turnus = 0;
        age = age + 1;
        
      }

      perio_state = P_Cycle(perio_state,
                            age, 
                            undiagnosedperio_to_step1_young,
                            undiagnosedperio_to_step1_old,
                            undiagnosedperio_to_unmanagedperio_young,
                            undiagnosedperio_to_unmanagedperio_old, 
                            step_unmanagedperio,
                            step1_to_step2,
                            step1_to_step4,
                            step2_to_step4,
                            step3_to_step4,
                            step4_to_step4);
      
      if(perio_state[0] == "undiagnosedperio") {
        
        perio_count_undiagnosedperio = perio_count_undiagnosedperio + 1;
        
      }
      
      if(perio_state[0] == "step1") {
        
        perio_count_step1 = perio_count_step1 + 1;
        
      }
      
      if(perio_state[0] == "step2") {
        
        perio_count_step2 = perio_count_step2 + 1;
        
      }
      
      if(perio_state[0] == "step3") {
        
        perio_count_step3 = perio_count_step3 + 1;
        
      }
      
      if(perio_state[0] == "step4") {
        
        perio_count_step4 = perio_count_step4 + 1;
        
      }
      
      if(perio_state[0] == "unmanagedperio") {
        
        perio_count_unmanagedperio = perio_count_unmanagedperio + 1;
        perio_is_unmanaged = 1;
        
      }
      
    }
    
  }
  
  death_age = age;
  
  List L = List::create(Named("healthy") = count_healthy, 
                        _["start_healthy"] = start_count_healthy,
                        _["undiagnosedgingi"] = count_undiagnosedgingi,
                        _["start_undiagnosedgingi"] = start_count_undiagnosedgingi,
                        _["managedgingi"] = count_managedgingi,
                        _["start_managedgingi"] = start_count_managedgingi,
                        _["perio"] = count_perio,
                        _["start_perio"] = start_count_perio,
                        _["count_out"] = count_out,
                        _["start_count_out"] = start_count_out,
                        _["start_count_unmanagedperio"] = start_count_unmanagedperio,
                        _["perio_count_undiagnosedperio"] = perio_count_undiagnosedperio,
                        _["perio_count_step1"] = perio_count_step1,
                        _["perio_count_step2"] = perio_count_step2,
                        _["perio_count_step3"] = perio_count_step3,
                        _["perio_count_step4"] = perio_count_step4,
                        _["perio_count_unmanagedperio"] = perio_count_unmanagedperio,
                        _["perio_is_unmanaged"] = perio_is_unmanaged,
                        _["death_age"] = death_age
  );
  
  return L;
}

// // [[Rcpp::export]]
// NumericVector Life_Simulation2( int age,
//                                 bool young_HG,
//                                 bool young_perio,
//                                 bool old_HG,
//                                 bool old_perio,
//                                 NumericVector surv_men,
//                                 NumericVector surv_women
// ) {
//   
//   CharacterVector state = Starting_State(age);
//   
//   CharacterVector sexstates = {"male", "female"};
//   NumericVector sexprobs = {0.5, 0.5};
//   CharacterVector sex(0);
//   sex = RcppArmadillo::sample(sexstates, 1, FALSE, sexprobs);
//   
//   int count_healthy = 0;
//   int start_count_healthy = 0;
//   int count_undiagnosedgingi = 0;
//   int start_count_undiagnosedgingi = 0;
//   int count_managedgingi = 0;
//   int start_count_managedgingi = 0;
//   int count_out = 0;
//   int start_count_out = 0;
//   int count_perio = 0;
//   int start_count_perio = 0;
//   int start_count_unmanagedperio = 0;
//   
//   int turnus = 0;
//   bool alive = TRUE;
//   
//   CharacterVector perio_state(0);
//   int count_perio_sub = 0;
//   int perio_count_undiagnosedperio = 0;
//   int perio_count_step1 = 0;
//   int perio_count_step2 = 0;
//   int perio_count_step3 = 0;
//   int perio_count_step4 = 0;
//   int perio_count_unmanagedperio = 0;
//   int perio_is_unmanaged = 0;
//   
//   double death_age = 0;
//   
//   if(state[0] == "healthy") {
//     
//     count_healthy = count_healthy + 1;
//     start_count_healthy = 1;
//     
//   } else {
//     
//     if(state[0] == "undiagnosedgingi") {
//       
//       count_undiagnosedgingi = count_undiagnosedgingi + 1;
//       start_count_undiagnosedgingi = 1;
//       
//     } else {
//       
//       if(state[0] == "managedgingi") {
//         
//         count_managedgingi = count_managedgingi + 1;
//         start_count_managedgingi = 1;
//         
//       } else {
//         
//         if(state[0] == "out") {
//           
//           count_out = count_out + 1;
//           start_count_out = 1;
//           
//         } else {
//           
//           perio_state = state;
//           count_perio = count_perio + 1;
//           start_count_perio = 1;
//           count_perio_sub = count_perio_sub + 1;
//           
//           if(perio_state[0] == "unmanagedperio") {
//             
//             start_count_unmanagedperio = 1;
//             perio_count_unmanagedperio = perio_count_unmanagedperio + 1;
//             perio_is_unmanaged = 1;
//             
//           }
//           
//         }
//         
//       }
//       
//     }
//     
//   }
//   
//   if(young_HG) {
//     
//     while(alive && (age < 65) && ((state[0] == "healthy") || (state[0] == "undiagnosedgingi") || (state[0] == "managedgingi"))) {
//       
//       turnus = turnus + 1;
//       
//       if(turnus == 2) {
//         
//         alive = Is_Alive(sex, age, surv_men, surv_women);
//         
//       }
//       
//       if(turnus == 3) {
//         
//         turnus = 0;
//         age = age + 1;
//         
//       }
//       
//       state = HG_Cycle(state, age);
//       
//       if(state[0] == "healthy") {
//         
//         count_healthy = count_healthy + 1;
//         
//       }
//       
//       if(state[0] == "undiagnosedgingi") {
//         
//         count_undiagnosedgingi = count_undiagnosedgingi + 1;
//         
//       }
//       
//       if(state[0] == "managedgingi") {
//         
//         count_managedgingi = count_managedgingi + 1;
//         
//       }
//       
//       if(state[0] == "newperio") {
//         
//         state = cpp_newperio_handler(age);
//         
//         if(state[0] == "out") {
//           
//           count_out = count_out + 1;
//           
//         } else {
//           
//           perio_state = state;
//           count_perio = count_perio + 1;
//           count_perio_sub = count_perio_sub + 1;
//           
//         }
//         
//       }
//       
//     }
//     
//   }
//   
//   if(young_perio) {
//     
//     while(alive && (age < 65) && (count_perio_sub > 0)) {
//       
//       turnus = turnus + 1;
//       
//       if(turnus == 2) {
//         
//         alive = Is_Alive(sex, age, surv_men, surv_women);
//         
//       }
//       
//       if(turnus == 3) {
//         
//         turnus = 0;
//         age = age + 1;
//         
//       }
//       
//       perio_state = P_Cycle(perio_state, age);
//       
//       if(perio_state[0] == "undiagnosedperio") {
//         
//         perio_count_undiagnosedperio = perio_count_undiagnosedperio + 1;
//         
//       }
//       
//       if(perio_state[0] == "step1") {
//         
//         perio_count_step1 = perio_count_step1 + 1;
//         
//       }
//       
//       if(perio_state[0] == "step2") {
//         
//         perio_count_step2 = perio_count_step2 + 1;
//         
//       }
//       
//       if(perio_state[0] == "step3") {
//         
//         perio_count_step3 = perio_count_step3 + 1;
//         
//       }
//       
//       if(perio_state[0] == "step4") {
//         
//         perio_count_step4 = perio_count_step4 + 1;
//         
//       }
//       
//       if(perio_state[0] == "unmanagedperio") {
//         
//         perio_count_unmanagedperio = perio_count_unmanagedperio + 1;
//         perio_is_unmanaged = 1;
//         
//       }
//       
//     }
//     
//   }
//   
//   if(old_HG) {
//     
//     while(alive && (age >= 65) && ((state[0] == "healthy") || (state[0] == "undiagnosedgingi") || (state[0] == "managedgingi"))) {
//       
//       turnus = turnus + 1;
//       
//       if(turnus == 2) {
//         
//         alive = Is_Alive(sex, age, surv_men, surv_women);
//         
//       }
//       
//       if(turnus == 3) {
//         
//         turnus = 0;
//         age = age + 1;
//         
//       }
//       
//       state = HG_Cycle(state, age);
//       
//       if(state[0] == "healthy") {
//         
//         count_healthy = count_healthy + 1;
//         
//       }
//       
//       if(state[0] == "undiagnosedgingi") {
//         
//         count_undiagnosedgingi = count_undiagnosedgingi + 1;
//         
//       }
//       
//       if(state[0] == "managedgingi") {
//         
//         count_managedgingi = count_managedgingi + 1;
//         
//       }
//       
//       if(state[0] == "newperio") {
//         
//         state = cpp_newperio_handler(age);
//         
//         if(state[0] == "out") {
//           
//           count_out = count_out + 1;
//           
//         } else {
//           
//           perio_state = state;
//           count_perio = count_perio + 1;
//           count_perio_sub = count_perio_sub + 1;
//           
//         }
//         
//       }
//       
//     }
//     
//   }
//   
//   if(old_perio) {
//     
//     while(alive && (age >= 65) && (count_perio_sub > 0)) {
//       
//       turnus = turnus + 1;
//       
//       if(turnus == 2) {
//         
//         alive = Is_Alive(sex, age, surv_men, surv_women);
//         
//       }
//       
//       if(turnus == 3) {
//         
//         turnus = 0;
//         age = age + 1;
//         
//       }
//       
//       perio_state = P_Cycle(perio_state, age);
//       
//       if(perio_state[0] == "undiagnosedperio") {
//         
//         perio_count_undiagnosedperio = perio_count_undiagnosedperio + 1;
//         
//       }
//       
//       if(perio_state[0] == "step1") {
//         
//         perio_count_step1 = perio_count_step1 + 1;
//         
//       }
//       
//       if(perio_state[0] == "step2") {
//         
//         perio_count_step2 = perio_count_step2 + 1;
//         
//       }
//       
//       if(perio_state[0] == "step3") {
//         
//         perio_count_step3 = perio_count_step3 + 1;
//         
//       }
//       
//       if(perio_state[0] == "step4") {
//         
//         perio_count_step4 = perio_count_step4 + 1;
//         
//       }
//       
//       if(perio_state[0] == "unmanagedperio") {
//         
//         perio_count_unmanagedperio = perio_count_unmanagedperio + 1;
//         perio_is_unmanaged = 1;
//         
//       }
//       
//     }
//     
//   }
//   
//   death_age = age;
//   NumericVector outt(4);
//   outt[0] = count_healthy + count_undiagnosedgingi + count_managedgingi + perio_count_undiagnosedperio +
//     perio_count_step1 + perio_count_step2 + perio_count_step3 + perio_count_step4 + perio_count_unmanagedperio;
//   outt[2] = death_age;
//   outt[3] = count_out;
//   
//   return outt;
// }
// 
// // [[Rcpp::export]]
// NumericVector Life_Simulation_Loop( int age,
//                                     int B,
//                                     NumericVector surv_men,
//                                     NumericVector surv_women
// ) {
// 
//   NumericVector heal(B);
// 
//   for (int i = 0; i <= B; ++i) {
//     NumericVector LL = Life_Simulation2(age, TRUE, TRUE, TRUE, TRUE, surv_men, surv_women);
// 
//     heal[i] = LL[0];
//   }
// 
//   return heal;
// }