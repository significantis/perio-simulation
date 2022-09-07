# Periodontitis Simulation Scripts

These are the simulation scripts used in the research article "Cost savings in the Swiss healthcare system resulting from professional periodontal care" by Ramseier et al. 2022. A free print of the article is available here: https://www.swissdentaljournal.org/magazin/artikel/cost-savings-in-the-swiss-healthcare-system-resulting-from-professional-periodontal-care.html. 

## Current Structure

Other researchers are free to check and improve the results by adding their codes. Currently - as of 2022-09-07 - the simulation structure is as follows:

### Main Scripts

The first main script is <tt>`sim1_events.R`</tt>. In this script, the life of 200'000 virtual citizens is simulated per age group (35-100) and per scenario (1-3). The result for each age-scenario-combination is an RDS file with 200'000 entries containing frequencies for each state and the citizen's death age. Note that packages <tt>`Rcpp RcppArmadillo`</tt> are needed.

```bash
sim1_events.R
|-- Auxiliaries
    |-- cpp_all_final.cpp
|-- Simulation_Data
    |-- death_tables_men_CH.csv
    |-- death_tables_women_CH.csv
|-- Scenarios
    |-- PDC1.R
    |-- PDC2.R
    |-- PDC3.R
|-----> Results/Events_per_Age/sim_scenario_X_age_Y.rds     
```

The second main script is <tt>`sim2_costs.R`</tt>. In this script, the simulated RDS files in <tt>`Results/Events_per_Age/sim_scenario_X_age_Y.rds`</tt> are reloaded and total costs until death are calculated for each line (i.e., virtual citizen). Finally, summary statistics including mean costs are calculated for each age-scenario-combination.

```bash
sim2_costs.R
|-- Results
    |-- Events_per_Age
        |-- sim_scenario_X_age_Y.rds 
|-- Scenarios
    |-- PDC1.R
    |-- PDC2.R
    |-- PDC3.R
|-----> Results/Simulation_OOP.csv
|-----> Results/Simulation_SW.csv
```

### Scenarios

The three scenarios are stored as R-scripts in the folder <tt>`Scenario`</tt>. Each R-script <tt>`PCDX.R`</tt> contains lists with starting and transition probability blocks on top and costs OOP and when paid by social welfare at the bottom. <u>Note that for cost simulations OOP all objects containing costs paid by social welfare must be uncommented in each scenario script</u>.

### Visualization

Tables and graphics for raw and smoothed data are created using scripts <tt>`tables.R`</tt> and <tt>`graphics.R`</tt>, respectively. Note that packages <tt>`tidyverse plyr`</tt> are needed.

### Auxiliary Functions

Auxiliary functions for R are stored in <tt>`Auxiliaries/auxiliaries.R`</tt>. Auxiliary functions for C++ are stored in <tt>`Auxiliaries/cpp_all_final.R`</tt>

## Create your own Scenario

Of course you can create your own scenario. Simply copy one of the three scenarios in <tt>`Scenario`</tt> and make sure it is loaded into the main scripts. Use the script <tt>`prevalence_checker.R`</tt> to check whether the prevalences you defined in your new scenario script do not shift over time (here, the average time spend in a state is calculated for an average Swiss citizen at his/her death). Note that packages <tt>`Rcpp RcppArmadillo tidyverse gcookbook`</tt> are needed for the prevalence check.
