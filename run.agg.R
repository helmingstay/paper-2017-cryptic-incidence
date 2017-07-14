#source('run.init.R')
#source('run.params.R')

agg.pred = ldply(agg.weeks, function(.agg) {
    .fit = mk.newdist(city.reprates, city.casereps, agg = .agg, agg.na.rm=F)
    .fit$agg <- .agg
    .fit
})


agg.pred <- within(agg.pred, {
    Weeks <- factor(agg)
    group <- paste(Disease, agg)
})

agg.pred = data.table(agg.pred)[,
    keyby=c('Disease', 'agg', 'pop.rates')
]

lmod <- dlply(agg.pred, 'Disease', function(.df){
    ret <- dlply(.df, 'Weeks', function(.indf){
        ret <- glm(form_base, .indf, family=the_family)
    })
    names(ret) <- paste0('w',names(ret))
    ret
})

## for each disease/agg level
## pull model rsq, nobs
agg.rsq <- ldply(lmod, function(.ll) {
    ldply(.ll, function(.mod) {
        ret=data.frame(
            rsq=mk.prop.dev(.mod),
            max.obs=max(.mod$data$Nobs),
            ncity=nrow(.mod$data),
            AIC=AIC(.mod)
        )
        ret
    })
})
agg.rsq <- data.table(agg.rsq)

## for all weeks together, fit model
## then get model predict from  
agg.pred <- agg.pred[, by='Disease', j={
    .df <- .SD
    mod = glm(form_agg, .SD, family=the_family)
    ret <- within(.SD, {
        AIC <- AIC(mod)
        obs.fit = predict(mod, type='response')
        obs.fit.link <- predict(mod, type='link')
        obs.y.trans <- mod$family$linkfun(persist)
    })
    est.x <- .SD
    ## substitute cols for predict
    est.x[[form_vars$obs]] <- est.x[[form_vars$est]] 
    ret <- within(ret, {
        #quant.cases <- quant.cases.full
        est.fit = predict(mod, est.x, type='response')
        est.fit.link <- predict(mod, est.x, type='link')
        crypt.fit <- est.fit - obs.fit
    })
    ret
}]

agg.pred.1 <- subset(agg.pred, agg==1)
