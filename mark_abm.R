


#Mark (1998) social differentiation ABM
social_differentiation <- function(N, max_mem, t_max = 500, r_max = 30) {
  
  #Store outputs
  output <- tibble(iteration = rep(1:t_max, r_max),
                   homogeneity = as.numeric(rep(NA, t_max*r_max)),
                   groups = as.numeric(rep(NA, t_max*r_max)),
                   K = as.numeric(rep(NA, t_max*r_max)), #Known facts in system
                   run = as.numeric(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    #initialize population
    memory <- matrix(0, ncol = N, nrow = N*t_max)
    
    #initialize first fact known by all
    memory[1,] <- max_mem
    
    #first unknown fact
    new_fact <- 2
    
    #Turn facts to 1/0
    known <- memory
    known[known > 0] <- 1
    
    #Calculate fact overlap
    social_structure <- (t(known) %*% known)
    
    #Iterate
    for (t in 1:t_max) {
      
      #Randomize interaction order
      order <- sample(1:N, replace = FALSE)
      
      for (p in 1:length(order)) {
        #Select Interaction Partners
        chooser <- order[p]
        partner <- sample(1:N, 1, replace = TRUE, 
                          prob = social_structure[chooser,])
        
        #If solo interaction
        if (chooser == partner) {
          #Enact one of your pieces by self
          facts <- which(known[,chooser] == 1)
          if (length(facts) == 1) {
            memory[facts,chooser] <- max_mem + 1
          } else {
            fact <- sample(facts, 1)
            memory[facts,chooser] <- max_mem + 1
          }
        } else { #If interaction
          facts <- which(rowSums(known[,c(chooser, partner)]) > 0)
          fact <- sample(c(facts, new_fact), 1)
          
          memory[fact,chooser] <- max_mem + 1
          memory[fact,partner] <- max_mem + 1
          
          if (fact == new_fact) {
            new_fact <- new_fact + 1
          }
          
        }
        #print(paste(chooser, partner, fact, sep = " "))
        
      }
      
      #Forgetting
      memory[memory > 0] <- memory[memory > 0] - 1
      known <- memory
      known[known > 0] <- 1
      
      social_structure <- (t(known) %*% known)
      
      #Calculating summary statistics
      #Cultural homogeneity
      K <- sum(rowSums(known) > 0)
      output[output$iteration == t & output$run == r, ]$homogeneity <- 
        sum(social_structure[upper.tri(social_structure)]) / 
        ((K * N * (N-1))/2)
      
      #Number of groups in system
      output[output$iteration == t & output$run == r, ]$groups <- count_components(
        graph_from_adjacency_matrix(social_structure, mode = "undirected", 
                                    diag = FALSE))
      
    }
    
  }
  output
}


M <- social_differentiation(6, max_mem = 3, t_max = 500, r_max = 60)

#Results are slightly off....
M %>% filter(iteration == 500) %>%
  summarise(homogeneity = mean(homogeneity), groups = mean(groups))

