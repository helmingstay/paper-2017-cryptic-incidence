library(data.table)
source('run.params.R')

.key = c('Disease','placename')

## grab observations
.fit <- subset(fit.df, 
    select=c(
        Disease, placename, 
        pop.rates, reprate.rates, 
        Nobs, persist, n.persist, n.zero, eff.pop
    )
)
.fit = data.table(.fit, key=.key)

#warning('!!subset boot.reprate')
#browser()
#bb0 <- subset(boot.reprate.rates, index<=1000)
.boot = boot.reprate.rates
## percap rates based bootstraps
.boot = data.table(.boot, key=.key)
.boot <- .boot[, by=c('placename','Disease'), j={
    list(boot.reprate=boot.reprate, index=1:length(boot.reprate))
}]
setkeyv(.boot, .key)

## combine
## na.omit - measles has full persist for some cities
## save empirical est of eff.pop for late merge
.boot = na.omit(merge(
    subset(.fit, select=-eff.pop), .boot, 
    all=T
))

## bootstrapped eff.pop
.boot[, eff.pop := pop.rates*boot.reprate]

## get reprate from permuted draws

.boot.by <- c('Disease','placename')
## (within disease/city)
## get reprate from independent draw
## with new name
.newboot.a <- .boot[, by=.boot.by, j=list(
    boot.reprate.a=boot.reprate,
    index=mk.rotate(index,1)
)]
.newboot.b <- .boot[, by=.boot.by, j=list(
    boot.reprate.b=boot.reprate,
    index=mk.rotate(index,2)
)]

.boot <- merge(.boot,.newboot.a, by=c('Disease','index','placename'))
.boot <- merge(.boot,.newboot.b, by=c('Disease','index','placename'))

## use boot.reprate (in eff.pop) for model
## use boot.reprate.a and .b for prediction
## for each index, fit model, simulate and predict
.pred = .boot[,by=c('Disease', 'index'), j={
    #.other <- (unique(.SD$index)+1) %% (.imax1)
    mod <-  glm(form_base, .SD, family=the_family)
    ## first, simulate observed persist
    ## one sim for each bootstrap draw / index
    #draws <- simulate(mod, nsim=1)[[1]]
    #n0 <- draws[,'n.zero']
    #n1 <- draws[,'n.persist']
    ## return obj
    ret <- subset(.SD, select=c(placename,boot.reprate,eff.pop))
    ## draws as proportion
    #ret[, boot.persist := n1/(n0+n1)]
    ## next, simulate predicted
    .pred <- .SD
    ## newdata predictor: "eff.pop at full reporting"
    .pred$eff.pop <- .pred$pop.rates * (boot.reprate.a / reprate.rates)
    #.pred$eff.pop <- .pred$pop.rates * (boot.reprate.a / boot.reprate.b)
    ## predicted persist
    mod.pred <- predict(mod, newdata=.pred, type='response')
    ## simulate pred persist for this model
    ## one draw for each place, 
    ret[,
        boot.predict := rbinom(rep(1, length(mod.pred)), Nobs, mod.pred)/Nobs
    ]
    ## finally, difference
    ret[, boot.crypt := boot.predict - persist]
    ret
}]

## summarize
.alpha = 0.05
.probs <- c(0.5, .alpha/2, 1-.alpha/2)
.pred.quant = .pred[,by=c('Disease','placename'), j={
    new.eff.pop <- quantile(eff.pop, probs=.probs)  
    pred <- quantile(boot.predict, probs=.probs)   
    crypt <- quantile(boot.crypt, probs=.probs)
    #eff.pop <- quantile(eff.pop, probs=.probs)
    ret <- data.table(probs=.probs, 
       new.eff.pop=new.eff.pop, Estimated=pred, Cryptic=crypt
    )
}]
    
## merge back observations
bootmod <- merge(.pred.quant, .fit, all=F, by=c('Disease','placename'))
bootmod[, Observed := persist]
#bootmod[, eff.pop := pop.rates * reprate.rates]
## no longer needed?
bootmod <- subset(bootmod, 
    select=c(-reprate.rates, -persist, -n.persist, -n.zero)
)
