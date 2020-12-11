# WEEK 2 - SOLUTIONS

# Main concepts:
# Structure and assumptions of simple linear models
# Bayesian inference with more than one parameter
# Prior simulation
# Posterior predictions
# NONE OF THIS IS CAUSAL INFERENCE (yet)

# Problem 1

library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
xbar <- mean(d2$weight)

m4.3 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( weight - xbar ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
), data=d2 )

#45kg prediction

# step 1 : extract posterior samples
post <- extract.samples(m4.3)

# step 2 : compute 'mu' for each set of samples
mu <- post$a + post$b*( 45 - xbar )

# step 3 : compute distribution of observable heights
sim_h <- rnorm( 1e4 , mu , post$sigma )

# compare to Prior
prior <- extract.prior(m4.3)
mu_prior <- prior$a + prior$b*( 45 - xbar )

dens( mu , xlim=c(140,175) , xlab="predicted height" )
dens( sim_h , add=TRUE , col="red" )
dens( mu_prior , add=TRUE , col="blue" )

mean(sim_h)
PI(sim_h)

# problem 2

d$log_weight <- log( d$weight )

plot( d$log_weight , d$height )

xbar2 <- mean(d$log_weight)

m2 <- quap(
    alist(
        height ~ dnorm( mu , sigma ) ,
        mu <- a + b*( log_weight - xbar2 ) ,
        a ~ dnorm( 178 , 20 ) ,
        b ~ dlnorm( 0 , 1 ) ,
        sigma ~ dunif( 0 , 50 )
), data=d )

# plot posterior predictions

lw_seq <- seq( from=1 , to=5 , length.out=30 )
pred_h <- sim( m2 , data=list(log_weight=lw_seq) )

plot( d$log_weight , d$height )
mu <- apply( pred_h , 2 , mean )
lines( lw_seq , mu , lwd=3 )
CI <- apply( pred_h , 2 , PI )
shade( CI , lw_seq )

# now plot model predictions on kg scale

plot( d$weight , d$height )
lines( exp(lw_seq) , mu , lwd=3 )
shade( CI , exp(lw_seq) )

# problem 3

# see solutions PDF
