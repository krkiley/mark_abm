
# Constructural model from Carley's "A Theory of Group Stability"


# Seems like I should set up the initial matrix outside the function
# Function just iterates the matrix over and over again...

mat <- matrix(c(1,1,0,0,0,
                1,1,1,0,0,
                0,1,1,1,1,
                0,0,1,1,1), nrow = 4, ncol = 5, byrow = TRUE)

group_stability <- function(mat, t_max = 500, r_max = 30) {
  
  M <- mat
  N <- nrow(mat)
  K <- ncol(mat)
  
  #Store outputs
  output <- tibble(iteration = rep(1:t_max, r_max),
                   homogeneity = as.numeric(rep(NA, t_max*r_max)),
                   run = as.numeric(rep(1:r_max, each = t_max)))
  
  for (r in 1:r_max) {
    #calcualte social structure
    social_structure <- (M %*% t(M))
    
    #Iterate
    for (t in 1:t_max) {

      #Randomize interaction order
      order <- sample(1:N, replace = FALSE)
      
      i <- 1
      #No restriction on interaction partners in a
      #round.
      for (p in 1:length(order)) {
        chooser <- order[p]

        #Draw interaction partners in proportion to fact overlap
        partner <- sample(1:N, 1, replace = TRUE, 
                          prob = social_structure[chooser,])

        #Chooser communicates to partner
        fact <- sample(1:K, 1, replace = TRUE, prob = M[chooser,])
        M[partner,fact] <- 1

        #Partner communicates to chooser
        fact <- sample(1:K, 1, replace = TRUE, prob = M[partner,])
        M[chooser,fact] <- 1
      }
      social_structure <- (M %*% t(M))
      output[output$iteration == t & output$run == r, ]$homogeneity <- 
        sum(social_structure[upper.tri(social_structure)]) / 
        ((K * N * (N-1))/2)
      
    }
  }
  output
}


generate_matrix <- function(N, K, p) {
  mat <- matrix(sample(c(1, 0), N*K, replace = TRUE, prob = c(p, 1-p)),
         nrow = N, ncol = K)
  mat[,1] <- 1
  mat
}

mat <- generate_matrix(1000, 100, .2)

data_model <- group_stability(mat, t_max = 500, r_max = 5)


ggplot(data_model, aes(x = iteration, y = homogeneity, fill = as.factor(run))) + 
  geom_line() + 
  theme_bw() + 
  labs(y = "homogeneity")




#Re-calculate structure for next iteration
social_structure <- (t(known) %*% known)
K <- sum(rowSums(known) > 0)


#Facts in system
output[output$iteration == t & output$run == r, ]$K <- K

#Cultural homogeneity
output[output$iteration == t & output$run == r, ]$homogeneity <- 
  sum(social_structure[upper.tri(social_structure)]) / 
  ((K * N * (N-1))/2)

#Number of groups in system
output[output$iteration == t & output$run == r, ]$groups <- count_components(
  graph_from_adjacency_matrix(social_structure, mode = "undirected", 
                              diag = FALSE))


