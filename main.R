library(ggplot2)

# first load helper functions
source("./util_func.R")


# First enter the crystal price 
refine_lvl_1_4_cost <- 18360
refine_lvl_5_8_cost <- 54960
protect_lvl_1_4_cost <- 0
protect_lvl_5_8_cost <- 67800
blessed_lvl_1_4_cost <- 0 
blessed_lvl_5_8_cost <- 167040

# Generate prob/cost table
prob_table <- Generate_prob_table(refine_lvl_1_4_cost,
                                  refine_lvl_5_8_cost,
                                  protect_lvl_1_4_cost,
                                  protect_lvl_5_8_cost,
                                  blessed_lvl_1_4_cost,
                                  blessed_lvl_5_8_cost)
head(prob_table)

# run simulation cost. Refine 5 -> 6
set.seed(1234)
nn <- 1000
r6_no_protect = Run_simulation_cost(nn, 5, "no_protect", prob_table)
r6_protect = Run_simulation_cost(nn, 5, "protect", prob_table)

r6_combined = rbind(r6_no_protect, r6_protect)
summary(r6_no_protect$total_cost)
summary(r6_protect$total_cost)

# get histogram
ggplot(r6_combined, aes(x = total_cost)) +
  geom_histogram(alpha = 0.4, aes(color = strategy,
                                  fill = strategy),
                 position = "dodge") 



############
##### simulation 2
# r4 protect 780840, no protect 360096
set.seed(3333)
nn <- 1000
r4_protect = Run_simulation_budget(nn, 780840, 4, "protect", prob_table)
r4_no_protect = Run_simulation_budget(nn, 780840, 4, "no_protect", prob_table)
r4_combined = rbind(r4_no_protect, r4_protect)
table(r4_protect$final_lvl)
table(r4_no_protect$final_lvl)
# get histogram
ggplot(r4_combined, aes(x = final_lvl)) +
  geom_histogram(alpha = 0.4, aes(color = strategy,
                                  fill = strategy),
                 position = "dodge") 


#### simulation 3: risk management