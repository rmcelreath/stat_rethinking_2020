# week 10
# key concepts
#   Gaussian processes as smoothing with continuous categories
#   How to include measurement error & missing data in DAGs
#   How to include measurement error & missing data in models

# 1

# X -> Y -> Z

N <- 1000
X <- rnorm(N,0,1)   # X has no parents, so just normal(0,1)
Y <- rnorm(N,X,1) # X -> Y
Z <- rnorm(N,Y,1) # Y -> Z
dat <- list(Y=Y,X=X,Z=Z)

library(rethinking)
m1.1 <- quap(
    alist(
        Y ~ dnorm(mu,sigma),
        mu <- a + bX*X,
        c(a,bX) ~ dnorm(0,1),
        sigma ~ dexp(1)
    ), data=dat )

m1.2 <- quap(
    alist(
        Y ~ dnorm(mu,sigma),
        mu <- a + bX*X + bZ*Z,
        c(a,bX,bZ) ~ dnorm(0,1),
        sigma ~ dexp(1)
    ), data=dat )

# case-control bias (selection bias)
# cloudy -> rain -> ground wet

# 2

library(rethinking)
set.seed(73)
N <- 500
U_sim <- rnorm( N )
Q_sim <- sample( 1:4 , size=N , replace=TRUE )
E_sim <- rnorm( N , U_sim + Q_sim )
W_sim <- rnorm( N , U_sim + 0*E_sim )
dat_sim <- list(
    W=standardize(W_sim) ,
    E=standardize(E_sim) ,
    Q=standardize(Q_sim) )

m14.6b <- ulam(
    alist(
        W ~ normal( muW , sigmaW ),
        E ~ normal( muE , sigmaE ),
        muW <- aW + bEW*E + bUW*U[i],
        muE <- aE + bQE*Q + bUE*U[i],

        vector[500]:U ~ normal( 0 , 1 ),
        
        c(aW,aE) ~ normal( 0 , 0.2 ),
        c(bEW,bQE) ~ normal( 0 , 0.5 ),
        c(bUE,bUW) ~ normal( 0 , 0.5 ),
        c(sigmaW,sigmaE) ~ exponential( 1 )
    ), data=dat_sim , chains=4 , cores=4 , cmdstan=TRUE )

precis(m14.6b)

traceplot(m14.6b,pars=c("bUE","bUW"))

post <- extract.samples(m14.6b)
dens(post$bUE)

# 3

y <- c( 18 , 19 , 22 , NA , NA , 19 , 20 , 22 )

# required assumptions:
# (1) what was total number of spins?
#   S > 120

# (2) what prior for the probabilities of each value?

library(gtools)
p <- rdirichlet( 1e3 , alpha=rep(4,8) )
plot( NULL , xlim=c(1,8) , ylim=c(0,0.3) , xlab="outcome" , ylab="probability" )
for ( i in 1:10 ) lines( 1:8 , p[i,] , type="b" , col=grau() , lwd=2 )

twicer <- function( p ) {
    o <- order( p )
    if ( p[o][8]/p[o][1] > 2 ) return( TRUE ) else return( FALSE )
}

p <- rdirichlet( 1e3 , alpha=rep(4,8) )
sum( apply( p , 1 , twicer ) )

# then marginalize over each possible combination of counts for 4 and 5, weighed by probability computed form (1) and (2)

code15H7 <- '
data{
    int N;
    int y[N];
    int y_max; // consider at most this many spins for y4 and y5
    int S_mean;
}
parameters{
    simplex[N] p;   // probabilities of each outcome
}
model{
    vector[(1+y_max)*(1+y_max)] terms;
    int k = 1;

    p ~ dirichlet( rep_vector(50,N) );

    // loop over possible values for unknown cells 4 and 5
    // this code updates posterior of p
    for ( y4 in 0:y_max ) {
        for ( y5 in 0:y_max ) {
            int Y[N] = y;
            Y[4] = y4;
            Y[5] = y5;
            terms[k] = poisson_lpmf(y4+y5|S_mean-120) 
                       + multinomial_lpmf(Y|p);
            k = k + 1;
        }//y5
    }//y4
    target += log_sum_exp(terms);
}
generated quantities{
    matrix[y_max+1,y_max+1] P45; // prob y4,y5 takes joint values
    // now compute Prob(y4,y5|p)
    {
        matrix[(1+y_max),(1+y_max)] terms;
        int k = 1;
        real Z;
        for ( y4 in 0:y_max ) {
            for ( y5 in 0:y_max ) {
              int Y[N] = y;
              Y[4] = y4;
              Y[5] = y5;
              terms[y4+1,y5+1] = poisson_lpmf(y4+y5|S_mean-120) 
                                 + multinomial_lpmf(Y|p);
            }//y5
        }//y4
        Z = log_sum_exp( to_vector(terms) );
        for ( y4 in 0:y_max )
            for ( y5 in 0:y_max )
                P45[y4+1,y5+1] = exp( terms[y4+1,y5+1] - Z );
    }
}
'

y <- c(18,19,22,-1,-1,19,20,22)
dat <- list(
    N = length(y),
    y = y,
    S_mean = 160,
    y_max = 40 )

m15H7 <- cstan( model_code=code15H7 , data=dat , chains=4 , cores=4 )

post <- extract.samples(m15H7)

y_max <- dat$y_max
plot( NULL , xlim=c(10,y_max-10) , ylim=c(10,y_max-10) ,
    xlab="number of 4s" , ylab="number of 5s" )
mtext( "posterior distribution of 4s and 5s" )
for ( y4 in 0:y_max ) for ( y5 in 0:y_max ) {
    k <- grau( mean( post$P45[,y4+1,y5+1] )/0.01 )
    points( y4 , y5 , col=k , pch=16 , cex=1.5 )
}

