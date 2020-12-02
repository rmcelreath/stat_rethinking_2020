# Statistical Rethinking Winter 2020/2021
# Discussion Seminar 1
# Points
# (1) Procedural questions
# (2) Software setup problems
# (3) Homework review - solutions and broader concepts
# (4) Prepare for next week - Chapter 4!

## Problem 1

# define grid
p_grid <- seq( from=0 , to=1 , length.out=1000 )
# define prior
prior <- rep( 1 , 1000 )
# compute likelihood at each value in grid
likelihood <- dbinom( 4 , size=15 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="l" , xlab="proportion water" , ylab="posterior probability" )

## Problem 2

# define grid
p_grid <- seq( from=0 , to=1 , length.out=1000 )
# define prior
prior <- c( rep( 0 , 500 ) , rep( 2 , 500 ) )
# compute likelihood at each value in grid
likelihood <- dbinom( 4 , size=15 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="l" , xlab="proportion water" , ylab="posterior probability" )

## Problem 3

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot( samples , ylim=c(0,1) , xlab="samples" , ylab="proportion water" )
PI( samples , 0.89 )
HPDI( samples , 0.89)
