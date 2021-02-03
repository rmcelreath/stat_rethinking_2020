# Week 7 - Ordered categories

# Key concepts
#   GLM strategy extended by:
#   (1) embedding distributions to create mixtures
#   (2) using odd link functions to handle odd variables
#   Understand beta-binomial and gamma-Poisson as *overdispersed* binomial and Poisson models - simple "robust" GLMs, like Student-t regression instead of Normal
#   Ordered categories as a common (in social sciences) problem with common solution
#   Get used to building more complex models, doing it in pieces to manage complexity

# 1

library(rethinking)
data(Trolley)
d <- Trolley

# recode these in order
edu_levels <- c( 6 , 1 , 8 , 4 , 7 , 2 , 5 , 3 )
d$edu_new <- edu_levels[ d$edu ]

idx <- 1:nrow(d)
dat <- list(
    y = d$response[idx] ,
    A = d$action[idx],
    I = d$intention[idx],
    C = d$contact[idx],
    E = as.integer( d$edu_new[idx] ),
    edu_norm = normalize( d$edu_new[idx] ),
    age = standardize( d$age[idx] ),
    alpha = rep(2,7) )

m1 <- ulam(
    alist(
        y ~ ordered_logistic( phi , cutpoints ),
        phi <- bE*sum( delta_shell[1:E] ) + bA*A + bC*C + BI*I + bAge*age,
        BI <- bI + bIA*A + bIC*C ,
        c(bA,bI,bC,bIA,bIC,bE,bAge) ~ normal( 0 , 0.5 ),
        cutpoints ~ normal( 0 , 1.5 ),
        vector[8]: delta_shell <<- append_row( 0 , delta ),
        simplex[7]: delta ~ dirichlet( alpha )
    ), data=dat , chains=4 , cores=4 , cmdstan=TRUE )

# X -> Y <- Z
# Z -> X -> Y

# blavaan

# 2

library(dagitty)
dag2 <- dagitty("dag{
    E -> R <- A
    A -> E
    G -> E
    G -> R
}")
drawdag(dag2)
adjustmentSets( dag2 , exposure="E" , outcome="R" , effect="total" )

dat$female <- ifelse( d$male==1 , 0L , 1L )
m2 <- ulam(
    alist(
        y ~ ordered_logistic( phi , cutpoints ),
        phi <- bE*sum( delta_shell[1:E] ) + bA*A + bC*C + BI*I + 
               bAge*age + bF*female,
        BI <- bI + bIA*A + bIC*C ,
        c(bA,bI,bC,bIA,bIC,bE,bAge,bF) ~ normal( 0 , 0.5 ),
        cutpoints ~ normal( 0 , 1.5 ),
        vector[8]: delta_shell <<- append_row( 0 , delta ),
        simplex[7]: delta ~ dirichlet( alpha )
    ), data=dat , chains=4 , cores=4 , cmdstan=TRUE )

