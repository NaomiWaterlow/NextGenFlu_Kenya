# sensitivity changing the efficacies to the actual values. 

# needs to be changed for current and limited extra vaccines
# i.e. scenarios 4 and 28


efficacy_H1_exact <-  c(62,72,67,72,65,65,49,52,54, 
                        64,23,24,41,40,40,50,65,65,67)
efficacy_H3_exact <- c(62,72,67,72,39,39,47,52,52,
                       6,13,22,58,40,43,10,25,25,9)
efficacy_B_exact <- c(62,72,50,72,58,58,67,54,52,
                      23,23,46,55,40,73,57,42,42,34)

vaccine_scenarios[[4]]$efficacy_H1 <- matrix(
  rep(c(efficacy_H1_exact,NA,NA),18)/100, ncol = 21, byrow = T)

vaccine_scenarios[[4]]$efficacy_H3 <- matrix(
  rep(c(efficacy_H3_exact,NA,NA),18)/100, ncol = 21, byrow = T)

vaccine_scenarios[[4]]$efficacy_B <- matrix(
  rep(c(efficacy_B_exact,NA,NA),18)/100, ncol = 21, byrow = T)

vaccine_scenarios[[28]]$efficacy_H1 <- matrix(
  rep(c(efficacy_H1_exact,NA,NA),18)/100, ncol = 21, byrow = T)

vaccine_scenarios[[28]]$efficacy_H3 <- matrix(
  rep(c(efficacy_H3_exact,NA,NA),18)/100, ncol = 21, byrow = T)

vaccine_scenarios[[28]]$efficacy_B <- matrix(
  rep(c(efficacy_B_exact,NA,NA),18)/100, ncol = 21, byrow = T)

