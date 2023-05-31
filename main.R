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
r4_protect = Run_simulation_budget(nn, 1265538, 5, "protect", prob_table)
r4_no_protect = Run_simulation_budget(nn, 1265538, 5, "no_protect", prob_table)
r4_combined = rbind(r4_no_protect, r4_protect)
table(r4_protect$final_lvl)
table(r4_no_protect$final_lvl)
# get histogram
ggplot(r4_combined, aes(x = final_lvl)) +
  geom_histogram(alpha = 0.4, aes(color = strategy,
                                  fill = strategy),
                 position = "dodge") 


#### simulation 3: risk management
#### simulate given the budget, what's failure rate at given lvl
set.seed(6666)
sim_result = lapply(c(1e6 + (0:10)*2*1e5), function(x) {
  set_lvl <- 5
  temp <- Run_simulation_budget(3000, x, set_lvl, "no_protect", prob_table)
  data.frame(budget = x, budget_in_m = x/1e6, fail_rate = mean(temp$final_lvl < set_lvl))
})

# options(scipen=999)
gg <- do.call(rbind, sim_result)
gg
ggplot(gg, aes(fail_rate, budget_in_m)) +
  geom_smooth(method = "loess") + 
  xlab("downgrade_rate") + 
  ylab("Budget ($MM)") +
  scale_x_binned(breaks = c(0.025, 0.05, 0.075, 0.1, 0.125, 0.15))
  


#
summary(r6_no_protect$total_iteration)
mean(r6_no_protect$total_iteration <= 4)
mean(r6_no_protect$total_iteration > 4 & r6_no_protect$total_iteration <= 10 )
mean(r6_no_protect$total_iteration >= 10)


rr = Run_simulation_cost(nn, 7, "no_protect", prob_table)
summary(rr$total_iteration)
mean(rr$total_iteration >= 30)
mean(rr$total_iteration >= 100)
