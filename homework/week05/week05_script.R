# WEEK 5
# Key concepts:
#   Causal relations and interaction effects
#   How to build and interpret interactions
#   Purpose and use of Markov chain Monte Carlo
#   How to check MCMC output

# 1

library(rethinking)
data(Wines2012)
d <- Wines2012

dat_list <- list(
    S = standardize(d$score),
    jid = as.integer(d$judge),
    wid = as.integer(d$wine) )

m1 <- ulam(
    alist(
        S ~ dnorm( mu , sigma ),
        mu <- a[jid] + w[wid],
        a[jid] ~ dnorm(0,0.5),
        w[wid] ~ dnorm(0,0.5),
        sigma ~ dexp(1)
    ), data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )

precis(m1,2)

plot( precis(m1,2) )

# 2

# indicator (dummy) variables

dat_list2 <- list(
    S = standardize(d$score),
    W = d$wine.amer,
    J = d$judge.amer,
    R = ifelse(d$flight=="red",1L,0L) )

m2a <- ulam(
    alist(
        S ~ dnorm( mu , sigma ),
        mu <- a + bW*W + bJ*J + bR*R,
        a ~ dnorm( 0 , 0.2 ),
        c(bW,bJ,bR) ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp(1)
    ), data=dat_list2 , chains=4 , cores=4 , cmdstan=TRUE )

plot( precis(m2a,omit="sigma") )

# index variables now

dat_list2b <- list(
    S = standardize(d$score),
    wid = d$wine.amer + 1L,
    jid = d$judge.amer + 1L,
    fid = ifelse(d$flight=="red",1L,2L) )

m2b <- ulam(
    alist(
        S ~ dnorm( mu , sigma ),
        mu <- w[wid] + j[jid] + f[fid],
        w[wid] ~ dnorm( 0 , 0.5 ),
        j[wid] ~ dnorm( 0 , 0.5 ),
        f[wid] ~ dnorm( 0 , 0.5 ),
        sigma ~ dexp(1)
    ), data=dat_list2b , chains=4 , cores=4 , cmdstan=TRUE )

plot( precis(m2b,depth=2,omit="sigma") )

post <- extract.samples(m2b)
diff_w <- post$w[,2] - post$w[,1]
precis( diff_w )

precis( m2a ) # compare bW to diff_w

# 3

dat_list2 <- list(
    S = standardize(d$score),
    W = d$wine.amer,
    J = d$judge.amer,
    R = ifelse(d$flight=="red",1L,0L) )

m3 <- ulam(
    alist(
        S ~ dnorm( mu , sigma ),
        mu <- a + bW*W + bJ*J + bR*R +
              bWJ*W*J + bWR*W*R + bJR*J*R,
        a ~ dnorm(0,0.2),
        c(bW,bJ,bR) ~ dnorm(0,0.5),
        c(bWJ,bWR,bJR) ~ dnorm(0,0.25),
        sigma ~ dexp(1)
    ), data=dat_list2 , chains=4 , cores=4 , cmdstan=TRUE )

plot(precis(m3,omit="sigma"))

pred_dat <- data.frame(
    W = rep( 0:1 , times=4 ),
    J = rep( 0:1 , each=4 ),
    R = rep( c(0,0,1,1) , times=2 ) )

mu <- link( m3 , data=pred_dat )

row_labels <- paste( ifelse(pred_dat$W==1,"A","F") , 
                 ifelse(pred_dat$J==1,"A","F") , 
                 ifelse(pred_dat$R==1,"R","W") , sep="" )

plot( precis( list(mu=mu) , 2 ) , labels=row_labels )

# what about 3-way interactions?

dat_list2b <- list(
    S = standardize(d$score),
    wid = d$wine.amer + 1L,
    jid = d$judge.amer + 1L,
    fid = ifelse(d$flight=="red",1L,2L) )

m3b <- ulam(
    alist(
        S ~ dnorm( mu , sigma ),
        mu <- w[wid,jid,fid],
        real['2,2,2']:w ~ normal(0,0.5),
        sigma ~ dexp(1)
    ), data=dat_list2b , chains=4 , cores=4 , cmdstan=TRUE )

row_labels = c("FFR","FFW","FAR","FAW","AFR","AFW","AAR","AAW" )
plot( precis( m3b , 3 , pars="w" ) , labels=row_labels )

