

porpoise.pop <- function(){ # POPULATION DYNAMICS MODEL
  
  Ny = 100 # HOW MANY TRIALS
  S0 = 0.789 # CALF SURVIVAL
  S1 = 0.95 # 1+ SURVIVAL
  
  
  K = 2500 # CARRYING CAPACITY
  x <- 6 # MAXIMUM AGE (+ GROUP)
  
  N.vec <- rep(NA, x+1)
  N <- matrix(rep(NA, Ny*(x+1)), nrow=Ny) # POP MATRIX

  # need to solve for initial structure, given K and prop. mature
  # did this by hand, basically 1250 = 625*f*(1+0.789*(1+0.95+0.95^2+0.95^3+0.95^4))
  f = 0.4376619

  N.vec[1] <- K*.5*.5 * f # CALVES 
  N.vec[2] <- N.vec[1]*S0 # YOTY
  N.vec[7] <- K/2 # MATURE

  for (i in 3:6){
    N.vec[i] <- N.vec[i-1]*S1 # INITIAL JUVENILES
  } 
  
  N[1,] <- N[100,]
  
  # fecundity of females
  fmin = 0.165
  fmax = 0.5
  z = 3
  # Checking that this fecundity works 
  
  for (t in 1:(Ny-1)){
    
    f <- fmin+((fmax-fmin)*(1-(sum(N[t,])/K)^z))
    
    N[t+1, x+1] <- N[t, x]*.95 + N[t, x+1]*.95 # MATURE + NEW MATURE
    
    N[t+1, 1] <- N[t+1, x+1] * .5 * f # CALVES
    
    N[t+1, 2] <- N[t, 1]*S0 # YOTY SURVIVORS
    
    for (i in 3:6){
      N[t+1, i] <- N[t, i-1]*S1
    }
    
  }
  plot(rowSums(N))

# Now to create stochasticity from YR1 to YR2
  
N.vec <- c(150, 115, 110, 105, 100, 95, 1825)
N.sto <- rep(NA, 10000)

f <- 0.165
for (i in 1:10000){
  
  nA <- sum(rbinom(N.vec[6], 1, S1)) + sum(rbinom(N.vec[7], 1, S1)) 
  n0 <- sum(rbinom(sum(rbinom(N.vec[7], 1, 0.5)), 1, f))
  n1 <- sum(rbinom(N.vec[1], 1, S0))
  n2 <- sum(rbinom(N.vec[2], 1, S1))
  n3 <- sum(rbinom(N.vec[3], 1, S1))
  n4 <- sum(rbinom(N.vec[4], 1, S1))
  n5 <- sum(rbinom(N.vec[5], 1, S1))
  
  N.sto[i] <- sum(nA + n0 + n1 + n2 + n3 + n4 + n5)
  
  }
  
  # CV = 0.007
  sd(N.sto)/mean(N.sto)
  
}

return(N)

} # end function