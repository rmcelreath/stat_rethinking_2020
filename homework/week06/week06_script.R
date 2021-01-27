# week 6 - maxent and GLMs
# key concepts
#   outcome distributions determined by CONSTRAINTS on variable + maxent
#   this is a form of prior knowledge
#   it is often OKAY to ignore some prior knowledge, but do better with it
#   GLMs and link functions: parameters not on outcome scale anymore
#   binomial and Poisson models

# 1

library(rethinking)
data(NWOGrants)
d <- NWOGrants
dat_list <- list(
    awards = as.integer(d$awards),
    apps = as.integer(d$applications),
    gid = ifelse( d$gender=="m" , 1L , 2L ) )

dat_list$disc <- as.integer(d$discipline)

m1 <- ulam(
    alist(
        awards ~ binomial( apps , p ),
        logit(p) <- a[gid],
        a[gid] ~ normal(0,1.5)
    ),
    data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )

precis(m1,2)

post <- extract.samples(m1)

p_diff <- sapply( 1:2000 , function(i) 
             inv_logit( post$a[i,1] ) - inv_logit( post$a[i,2] ) )
precis(p_diff)

# now with discipline

m2 <- ulam(
    alist(
        awards ~ binomial( apps , p ),
        logit(p) <- a[gid] + d[disc],
        a[gid] ~ normal(0,1.5),
        d[disc] ~ normal(0,1)
    ),
    data=dat_list , chains=4 , cores=4 , cmdstan=TRUE )


# 2

set.seed(1913)
N <- 1000
G <- rbern(N)
S <- rbern(N)
D <- rbern( N , p=inv_logit( G + S ) )
A <- rbern( N , p=inv_logit( 0.25*G + D + 2*S - 2 ) )
dat_sim <- list( G=G , D=D , A=A )

# 3 

library(rethinking)
library(MASS)
data(eagles)
d <- eagles
d$pirateL <- ifelse( d$P=="L" , 1 , 0 )
d$victimL <- ifelse( d$V=="L" , 1 , 0 )
d$pirateA <- ifelse( d$A=="A" , 1 , 0 )

dat <- list( y=d$y , n=d$n , pirateL=d$pirateL , pirateA=d$pirateA , victimL=d$victimL )

# no interactions
m3 <- ulam(
    alist(
        y ~ binomial( n , p ),
        logit(p) <- a + bP*pirateL + bA*pirateA + bV*victimL,
        a ~ normal(0,1.5),
        c(bV,bP,bA) ~ normal(0,1)
    ),
    data=dat , chains=4 , cmdstan=TRUE  )

precis(m3,2)

# interactions
m3b <- ulam(
    alist(
        y ~ binomial( n , p ),
        logit(p) <- a + bP*pirateL + bA*pirateA + bV*victimL +
                    bAV*pirateA*victimL,
        a ~ normal(0,1.5),
        c(bV,bP,bA,bAV) ~ normal(0,1)
    ),
    data=dat , chains=4 , cmdstan=TRUE  )

precis(m3b,2)




