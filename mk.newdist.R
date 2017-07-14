my.na.sum <- function(x, ...) {
    if (all(is.na(x))) { 
        return(NA) 
    } else {
        return(sum(x, ...))
    }
}

    

mk.newdist <- function(
    reprates, cases, by='Disease',
    agg = 1, agg.na.rm=T
) {
    ## for each disease, split reports
    ## return data.frame of sample, inferred distribution with events/covariates 
    ret <- ddply( reprates, 'Disease', function(.rep) {
        ## pull out disease
        .dis <- unique(.rep$Disease)
        .cases <- cases[[.dis]]
        if (agg > 1) {
            .ep = endpoints(.cases, on='weeks', k=agg)
            ## na.rm only if some non-NA obs
            .cases = period.apply(.cases, .ep, function(.dat) apply(.dat, 2, my.na.sum, na.rm=agg.na.rm))
        }
        ## for each city, grab mean and median cases
        ## keeping zeros leaves median basically unchanged
        ## discarding zeros greatly raises mean
        in.ret <- adply(.cases, 2, function(.city.cases) {
            ## first prep objects
            ## get length and #NAs of raw data
            N <- length(.city.cases)
            NAs <- sum(is.na(.city.cases))
            Nobs <- N - NAs
            ##
            .dat <- na.omit(.city.cases)
            ## the remainder of processing is on non-na
            prop0 <- sum(.dat==0)/N
            propNA <- NAs/N
            n.persist <- sum(.dat>0)
            n.zero <- sum(.dat==0)
            #.lcases <- log10(.city.cases[.city.cases>0])
            ## mean of log cases, with log(0)=0
            #.lmean <- log10(.dat[.dat>0])
            #.lmean <- log10(.dat)
            #.lmean[is.infinite(.lmean)] <- 0
            #.lmean <- mean(.lmean)
            ## then prepare return obj
            city.df <- data.frame(
                N, NAs, Nobs, propNA, prop0, n.persist, n.zero,
                ## estimate in logspace, back convert
                #lmean.cases = 10^.lmean,
                mean.cases = mean(.dat),
                sum.cases = sum(.dat),
                ## the following works fine, model in Fig S3 not quite as good
                #mean.cases = mean(.dat), 
                ## transform doesnt matter for median
                median.cases = median(.dat)
                ## 
                ## tested l-estimator, can't interpret scale 
                #llest.cases = 10^mk.lest(log10(.dat+1))-1,
                #lest.cases = mk.lest(.dat)
            )
            city.df
        })
        ## combine results
        in.ret <- rename(in.ret, c(X1='placename'))
        ## pull out relevant demographics, merge in
        .rep.sub <- subset(.rep, 
            select=c(placename, pop.rates, reprate.rates, reprate.use)
        )
        ## summaries with demographics
        in.ret <- merge(in.ret, .rep.sub)
        return(in.ret)
    })

    ## keep only pops with some apparent extinct
    ## remove diagnostics, cast inferred vars into cols
    ret <- subset(ret, 
        prop0 > 0   #& which == 'infer' 
    )

    ## calculate predictors using demographics
    ret <- within(ret, {
        ## final measure, logspace
        #mean <- log10(mean.cases)
        mean.cases.est <- mean.cases/reprate.use
        sum.cases.est <- sum.cases/reprate.use
        measure <- mean.cases
        quant.cases <- trans.x(measure)
        ## quantile cases with full reporting, e.g. "true"
        quant.cases.full <- trans.x(measure / reprate.use)
        ## effective pop = pop * reprate
        eff.pop <- pop.rates * reprate.use
        ## non-extinct time
        persist <- 1-prop0
    })
    ret
}

    ##??
    ## NOTE - most rates estimates fall outside 1 SE of census method
    ## and negative bias
    ## ss(with(fit.df, abs(reprate.rates - reprate.ipums)<reprate.ipums.se))
