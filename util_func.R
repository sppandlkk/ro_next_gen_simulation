### helper function that generate the prob table and calculate costs from input
Generate_prob_table <- function(refine_lvl_1_4_cost,
                                refine_lvl_5_8_cost,
                                protect_lvl_1_4_cost = 0,
                                protect_lvl_5_8_cost,
                                blessed_lvl_1_4_cost = 0,
                                blessed_lvl_5_8_cost){
  
  # generate the probably table with corresponding costs for no protection case
  no_protect <- data.frame(strategy = "no_protect",
                           curr_lvl = 0:7,
                           next_lvl = 1:8,
                           p_up = c(1, 1/2, 1/3, 1/4, 1/4, 1/4, 1/5, 1/5),
                           p_fail = c(0, 1/2, 2/3, 3/4, 0.4, 0.4, 0.4, 0.4),
                           p_down = c(0, 0, 0, 0, 1/4, 1/4, 1/4, 1/4),
                           p_damage = c(0, 0, 0, 0, 0.1, 0.1, 0.15, 0.15),
                           refine_cost = c(rep(refine_lvl_1_4_cost, 4), rep(refine_lvl_5_8_cost, 4)),
                           blessed_cost = c(rep(blessed_lvl_1_4_cost, 4), rep(blessed_lvl_5_8_cost, 4)))
  # same for protection case
  protect <- data.frame(strategy = "protect",
                        curr_lvl = 0:7,
                        next_lvl = 1:8,
                        p_up = c(1, 1/2, 1/3, 1/4, 1/5, 1/7.5, 1/10, 1/15),
                        p_fail = c(0, 1/2, 2/3, 3/4, 0.6, 0.6166666, 0.6, 0.5833333),
                        p_down = 0,
                        p_damage = c(0, 0, 0, 0, 0.2, 0.25, 0.3, 0.35),
                        refine_cost = c(rep(refine_lvl_1_4_cost + protect_lvl_1_4_cost, 4), rep(refine_lvl_5_8_cost + protect_lvl_5_8_cost, 4)),
                        blessed_cost = c(rep(blessed_lvl_1_4_cost, 4), rep(blessed_lvl_5_8_cost, 4)))
  
  rbind(no_protect, protect)
  
}



### help function to simulate the refinement results
Refine_once <- function(curr_lvl, strategy, prob_table){
  # extrat the prob table with the specified curr lvl and strategy
  prob <- prob_table[(prob_table$strategy == strategy) & (prob_table$curr_lvl == curr_lvl), ]
  
  # not simulate the results. 1 success, 0 fail and no move, -1 downgrade, 0.1 damange
  result <- sample(c(1, 0, -1, 0.1), 1, prob = prob[, c("p_up", "p_fail", "p_down", "p_damage")])
  
  # total cost is the cost of refinement Plus damange cost if damaged
  cost <- prob$refine_cost + ifelse(result == 0.1, prob$blessed_cost, 0)
  
  # finally outupt the result
  return(data.frame(refine_result = result,
                    # take floor function on result, so damange is at the same level
                    output_lvl = curr_lvl + floor(result),
                    output_cost = cost))
}


### helper function to simulate case 1: run until upgrade
Run_until_upgrade <- function(curr_lvl, strategy, prob_table){
  # extrat the prob table with the specified curr lvl and strategy
  prob <- prob_table[(prob_table$strategy == strategy) & (prob_table$curr_lvl == curr_lvl), ]
  
  # initialize index
  iteration <- 0
  total_cost <- 0
  history <- c()
  sim_lvl <- curr_lvl
  
  # use while loop to keep running until we hit the next level
  while(sim_lvl < prob$next_lvl){
    
    # refine once and get result
    temp_result <- Refine_once(sim_lvl, strategy, prob_table)
    
    # after refinement, update the parameter
    sim_lvl <- temp_result$output_lvl
    total_cost <- total_cost + temp_result$output_cost
    # I keep history of refine to QA
    history <- c(history, temp_result$refine_result)
    iteration <- iteration + 1
  }

  # output
  return(data.frame(iteration = iteration,
              total_cost = total_cost,
              refine_history = paste(history, collapse = ", ")))

}

# helper function to run simulation of total cost
Run_simulation_cost <- function(num_sim, curr_lvl, strategy, prob_table){
  ans <- lapply(1:num_sim, function(x) {
    
    Run_until_upgrade(curr_lvl, strategy, prob_table)
    
  })
  # get the columns that we need
  temp <- do.call(rbind, ans)
  
  return(data.frame(strategy = strategy, 
                    total_iteration = temp$iteration,
                    total_cost = temp$total_cost))
  
}  

# helper function for run until budget exhaust case
Run_until_budget <- function(budget, curr_lvl, strategy, prob_table) {
  # extrat the prob table with the specified curr lvl and strategy
  prob <- prob_table[(prob_table$strategy == strategy) & (prob_table$curr_lvl == curr_lvl), ]
  
  # initialize index
  iteration <- 0
  total_cost <- 0
  history <- c()
  sim_lvl <- curr_lvl
  can_roll <- (budget - total_cost) >= prob$refine_cost
  
  # use while loop to keep running until we hit the next level OR we run out of $$
  while(sim_lvl < prob$next_lvl & can_roll){
    
    # refine once and get result
    temp_result <- Refine_once(sim_lvl, strategy, prob_table)
    
    # after refinement, update the parameter
    sim_lvl <- temp_result$output_lvl
    total_cost <- total_cost + temp_result$output_cost
    # I keep history of refine to QA
    history <- c(history, temp_result$refine_result)
    iteration <- iteration + 1
    
    # check if we still have enough money to refine another time
    can_roll <- (budget - total_cost) >= prob$refine_cost
  }
  
  # output
  return(data.frame(budget = budget,
                    iteration = iteration,
                    total_cost = total_cost,
                    final_lvl = ifelse(iteration == 0, curr_lvl, temp_result$output_lvl),
                    refine_history = paste(history, collapse = ", ")))
  
}


# helper function to run simulation
Run_simulation_budget <- function(num_sim, budget, curr_lvl, strategy, prob_table){
  ans <- lapply(1:num_sim, function(x) {
    
    Run_until_budget(budget, curr_lvl, strategy, prob_table)
    
  })
  # get the columns that we need
  temp <- do.call(rbind, ans)
  
  return(data.frame(strategy = strategy, 
                    final_lvl = temp$final_lvl,
                    total_iteration = temp$iteration,
                    total_cost = temp$total_cost))
  
}



