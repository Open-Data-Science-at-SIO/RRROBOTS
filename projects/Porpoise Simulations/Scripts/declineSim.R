
# The problem: fixed underlying animal density, fixed grid of impact
# % decline can vary from year to year, but is fixed over 10 yrs 

########## USER INPUTS ##########
# no. years in the study
nyr <- 10

# overall decline of the population
target = 0.5

# this is the grid of noise levels or some other disturbance
# it's a 7x7 bullseye, with greatest impact in the center (score = 1)

levels <- c(rep(0.25, 7), 
            0.25, rep(0.5, 5), 0.25,
            0.25, 0.5, rep(0.75, 3), 0.5, 0.25,
            0.25, 0.5, 0.75, 1, 0.75, 0.5, 0.25,
            0.25, 0.5, rep(0.75, 3), 0.5, 0.25,
            0.25, rep(0.5, 5), 0.25,
            rep(0.25, 7))

impact <- matrix(1/levels, nrow=7, ncol=7)/sum(1/levels) # this adds to 1

# this is an arbitrary density grid, with a hotspot in the upper left corner
# density is in animals per sq km
# same dimensions as the impact grid

animals <- c(1, 1, 1, 0.8, 0.8, 0.7, 0.6,
             1, 1, 0.8, 0.8, 0.7, 0.6, 0.5,
             1, 0.8, 0.8, 0.7, 0.6, 0.5, 0.4,
             0.8, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,
             0.8, 0.7, 0.6, 0.5, 0.4,0.3, 0.2,
             0.7, 0.6, 0.5, 0.4,0.3, 0.2, 0.1,
             0.6, 0.5, 0.4,0.3, 0.2, 0.1, 0)

density <- matrix(animals, nrow=7, ncol=7)

########## END USER INPUTS ##########

decline <- function(density, impact, target, nyr){
  
  # First calculate the avg. per year decline
  davg <- target^(1/(nyr-1))
  
  # generate the decline in each year
  # this method forces the decline to be exactly 50%
  # but that could be changed so that ON AVERAGE over the 1000 sims
  # the decline is 50% but each individual sim might vary
  dvals <- rnorm(nyr-2, mean=davg, sd=0.1) # sd can be changed
  dvals[nyr-1] <- 0.5/prod(dvals[1:(nyr-2)]) # solve for final decline
  dvals <- dvals[sample(1:9, 9)] # randomize order
  
  # check that the product of the declines is the target
  print(paste("Total decline = ", round(prod(dvals), digits=4)))

  # Run forward in time, storing the population matrices for each year
  P <- list(density) # output list where original density is the first year
  
  for (i in 1:(nyr-1)){
    
    d <- dvals[i] # current decline value
     
    X <- d*sum(P[[i]])/sum(P[[i]]*impact) # solve for multiplier
    
    P[[i+1]] <- P[[i]]*impact*X # generate population in yr i + 1
    
    # show that this worked
    print(paste("target yr", i, "decline = ", round(d, digits=4), 
                ", actual = ", round(sum(P[[i]]*impact*X)/sum(P[[i]]), digits=4)))
  }
  
  # check that the population declined by target
  print(paste("Total decline = ", sum(P[[10]])/sum(P[[1]])))
  
  return(P)
}

# run the function
P <- decline(density, impact, target, nyr)

# plot
require(ggplot2)

for (i in 1:10){
  p <- data.frame(expand.grid(X=1:7, Y=1:7), "D"=matrix(P[[i]]))
  
  g <- ggplot(p, aes(x=X, y=Y, color=D))+
    geom_point(size=25)+
    scale_color_gradient(low="yellow", high="red")
  
  print(g)
  
}
