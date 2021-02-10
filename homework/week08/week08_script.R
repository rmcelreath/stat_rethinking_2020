# week 8 - multilevel models 1
# key concepts
#   partial pooling as automatic regularization
#   how to code partial pooling
#   divergent transitions and centered/non-centered parameterization

# 1

library(rethinking)
data(reedfrogs)
d <- reedfrogs

dat <- list(
    S = d$surv,
    n = d$density,
    tank = 1:nrow(d),
    pred = ifelse( d$pred=="no" , 0L , 1L ),
    size_ = ifelse( d$size=="small" , 1L , 2L )
)

# basic model, no predictors

m1.1 <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a[tank],
        a[tank] ~ normal( a_bar , sigma ),
        a_bar ~ normal( 0 , 1.5 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# models with predictors

m1.2 <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a[tank] + bp*pred,
        a[tank] ~ normal( a_bar , sigma ),
        bp ~ normal( -0.5 , 1 ),
        a_bar ~ normal( 0 , 1.5 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# size
m1.3 <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a[tank] + s[size_],
        a[tank] ~ normal( a_bar , sigma ),
        s[size_] ~ normal( 0 , 0.5 ),
        a_bar ~ normal( 0 , 1.5 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# pred + size
m1.4 <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a[tank] + bp*pred + s[size_],
        a[tank] ~ normal( a_bar , sigma ),
        bp ~ normal( -0.5 , 1 ),
        s[size_] ~ normal( 0 , 0.5 ),
        a_bar ~ normal( 0 , 1.5 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# pred + size + interaction
m1.5 <- ulam(
    alist(
        S ~ binomial( n , p ),
        logit(p) <- a_bar + z[tank]*sigma + bp[size_]*pred + s[size_],
        z[tank] ~ normal( 0 , 1 ),
        bp[size_] ~ normal( -0.5 , 1 ),
        s[size_] ~ normal( 0 , 0.5 ),
        a_bar ~ normal( 0 , 1.5 ),
        sigma ~ exponential( 1 )
    ), data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# compare

compare( m1.1 , m1.2 , m1.3 , m1.4 , m1.5 )

plot( coeftab( m1.1 , m1.2 , m1.3 , m1.4 , m1.5 ), pars="sigma" )

# 2

library(rethinking)
data(bangladesh)
d <- bangladesh
d$district_id <- as.integer(as.factor(d$district))

dat_list <- list(
    C = d$use.contraception,
    did = d$district_id
)

# fixed model

m2.1 <- ulam(
    alist(
        C ~ bernoulli( p ),
        logit(p) <- a[did],
        a[did] ~ normal( 0 , 1.5 )
    ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# pooling model

m2.2 <- ulam(
    alist(
        C ~ bernoulli( p ),
        logit(p) <- a[did],
        a[did] ~ normal( a_bar , sigma ),
        a_bar ~ normal( 0 , 1.5 ),
        sigma ~ exponential( 1 )
    ) , data=dat_list , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

post1 <- extract.samples( m2.1 )
post2 <- extract.samples( m2.2 )

p1 <- apply( inv_logit(post1$a) , 2 , mean )
p2 <- apply( inv_logit(post2$a) , 2 , mean )

nd <- max(dat_list$did)
plot( NULL , xlim=c(1,nd) , ylim=c(0,1) , ylab="prob use contraception" , 
    xlab="district" )
points( 1:nd , p1 , pch=16 , col=rangi2 )
points( 1:nd , p2 )
abline( h=mean(inv_logit(post2$a_bar)) , lty=2 )

# 3

data(Trolley)
d <- Trolley

dat <- list(
    R = d$response,
    A = d$action,
    I = d$intention,
    C = d$contact )

# model without individual intercepts
m3.1 <- ulam(
    alist(
        R ~ dordlogit( phi , cutpoints ),
        phi <- bA*A + bC*C + BI*I ,
        BI <- bI + bIA*A + bIC*C ,
        c(bA,bI,bC,bIA,bIC) ~ dnorm( 0 , 0.5 ),
        cutpoints ~ dnorm( 0 , 1.5 )
    ) , data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

# model with individual intercepts (and pooling)

m3.2 <- ulam(
    alist(
        R ~ dordlogit( phi , cutpoints ),
        phi <- a[id] + bA*A + bC*C + BI*I ,
        BI <- bI + bIA*A + bIC*C ,
        a[id] ~ normal( 0 , sigma ),
        c(bA,bI,bC,bIA,bIC) ~ dnorm( 0 , 0.5 ),
        cutpoints ~ dnorm( 0 , 1.5 ),
        sigma ~ exponential(1)
    ) , data=dat , chains=4 , cores=4 , log_lik=TRUE , cmdstan=TRUE )

