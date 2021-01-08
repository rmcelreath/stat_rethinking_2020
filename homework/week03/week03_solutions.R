# WEEK 3 SOLUTIONS
# Key concepts:
#  Prediction and inference are different tasks
#  Estimates are not automatically causal estimates
#  Adding variables (controls) can help, but also hurt
#  Causal inference only possible with explicit causal model
#  Recognize confounds and colliders in DAGs
#  Learn how to detect and close back door paths
#  There are more causal beasts to come (instruments, missing data, measurement error, bias amplification, selection bias, yadda yadda)

library(rethinking)
data(foxes)
d <- foxes
d$W <- standardize(d$weight)
d$A <- standardize(d$area)
d$F <- standardize(d$avgfood)
d$G <- standardize(d$groupsize)

# 1 

m1 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + bA*A,
        a ~ dnorm(0,0.2),
        bA ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )

# prior predictive simulation

N <- 1000
a_prior <- rnorm(N,0,0.2)
bA_prior <- rnorm(N,0,0.5)
sigma_prior <- rexp(N,1)

seq_A <- seq( from=-2 , to=2 , length.out=30 )
prior <- extract.prior(m1)
mu <- link( m1 , data=list(A=seq_A) , post=prior )
mu_mean <- apply( mu , 2 , mean )

plot( NULL , xlim=c(-2,2) , ylim=c(-2.5,2.5) , xlab="Area (std)" , ylab="Weight (std)"  )

for ( i in 1:100 ) lines( seq_A , mu[i,] )

# 2

m2 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + bF*F,
        a ~ dnorm(0,0.2),
        bF ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )

# 3

m3 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + bF*F + bG*G,
        a ~ dnorm(0,0.2),
        c(bF,bG) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )

m4 <- quap(
    alist(
        W ~ dnorm( mu , sigma ),
        mu <- a + bF*F + bG*G + bA*A,
        a ~ dnorm(0,0.2),
        c(bF,bG,bA) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )

# implied conditional independencies

library(dagitty)
fox_dag <- dagitty( "dag {
    A -> F
    G <- F -> W
    G -> W
}")
impliedConditionalIndependencies(fox_dag)

# (1) A _||_ W | F

m5 <- quap(
    alist(
        A ~ dnorm( mu , sigma ),
        mu <- a + bW*W + bF*F,
        a ~ dnorm(0,0.2),
        c(bF,bW) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )

# (2) A _||_ G | F

m6 <- quap(
    alist(
        A ~ dnorm( mu , sigma ),
        mu <- a + bG*G + bF*F,
        a ~ dnorm(0,0.2),
        c(bF,bG) ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=d )

