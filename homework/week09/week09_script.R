# Week 9 homework
# Key concepts:
#  How to build and interpret varying slopes
#  Using covariance to regularize across parameter types
#  See varying effects as latent variables

# 1

library(rethinking)
data(bangladesh)
d <- bangladesh
dat_list <- list(
    C = d$use.contraception,
    did = as.integer( as.factor(d$district) ),
    urban = d$urban
)

m1.1 <- ulam(
    alist(
        C ~ bernoulli( p ),
        logit(p) <- a[did] + b[did]*urban,
        # a[did] ~ normal( abar , sigma_a ),
        # b[did] ~ normal( bbar , sigma_b ),
        c(a,b)[did] ~ multi_normal( c(abar,bbar) , Rho , Sigma ),
        abar ~ normal(0,1),
        bbar ~ normal(0,0.5),
        Rho ~ lkj_corr(2),
        Sigma ~ exponential(1)
        # c(sigma_a,sigma_b) ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )

post <- extract.samples(m1.1)
a <- apply( post$a , 2 , mean )
b <- apply( post$b , 2 , mean )
plot( a, b , xlab="a (intercept)" , ylab="b (urban slope)" )
abline( h=0 , lty=2 )
abline( v=0 , lty=2 )
library(ellipse)
R <- apply( post$Rho , 2:3 , mean )
s <- apply( post$Sigma , 2 , mean )
S <- diag(s) %*% R %*% diag(s)
ll <- c( 0.5 , 0.67 , 0.89 , 0.97 )
for ( l in ll ) {
    el <- ellipse( S , centre=c( mean(post$abar) , mean(post$bbar) ) , level=l )
    lines( el , col="black" , lwd=0.5 )
}

# 2

dat_list$children <- standardize( d$living.children )
dat_list$age <- standardize( d$age.centered )

m2.1 <- ulam(
    alist(
        C ~ bernoulli( p ),
        logit(p) <- a[did] + b[did]*urban + bA*age,
        c(a,b)[did] ~ multi_normal( c(abar,bbar) , Rho , Sigma ),
        abar ~ normal(0,1),
        c(bbar,bA) ~ normal(0,0.5),
        Rho ~ lkj_corr(2),
        Sigma ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )

m2.2 <- ulam(
    alist(
        C ~ bernoulli( p ),
        logit(p) <- a[did] + b[did]*urban + bK*children + bA*age,
        c(a,b)[did] ~ multi_normal( c(abar,bbar) , Rho , Sigma ),
        abar ~ normal(0,1),
        c(bbar,bK,bA) ~ normal(0,0.5),
        Rho ~ lkj_corr(2),
        Sigma ~ exponential(1)
    ) , data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )

precis(m2.1)
precis(m2.2)

# 3

dat_list$K <- d$living.children
dat_list$alpha <- rep(2,3)

m3.1 <- ulam(
    alist(
        C ~ bernoulli( p ),
        logit(p) <- a[did] + b[did]*urban + bK*sum( delta_shell[1:K] ) + bA*age,
        c(a,b)[did] ~ multi_normal( c(abar,bbar) , Rho , Sigma ),
        abar ~ normal(0,1),
        c(bbar,bK,bA) ~ normal(0,0.5),
        Rho ~ lkj_corr(2),
        Sigma ~ exponential(1),
        vector[4]: delta_shell <<- append_row( 0 , delta ),
        simplex[3]: delta ~ dirichlet( alpha )
    ) , data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )

precis(m3.1)

precis(m3.1,pars="delta",2)
