## depends on:
# source('run.init.R')

## data from the paper:
## Conserved patterns of incomplete reporting in pre-vaccine era childhood diseases. Proc. R. Soc. B, 2014.
## See run.lcasedata.R in repo IPUMS
## (was reprate.cache/cache.demog.RData)
## contains all.casereps (filtered for "good" cities)
load('cache.reprate.RData')

## cleanup - change wc to pertussis
.tmp <- mk.rename.disease(all.casereps)
.disease <- mk.names(names(.tmp))
## pull out simplified object
city.casereps <- lapply(.disease, function(.dis) {
    ## pull out city xts for this disease
    ret <- .tmp[[.dis]]$City
    ## pull out time limits (in run.params.R)
    ## subset xts
    .lim <- time.limits[[.dis]]
    ret <- ret[ .lim, ]
    ret
})

## modified from run.lcasedata.R in repo ipums
## for each area/year
## find min/max time range, return as data.frame
city.sample.range <- ldply(names(city.casereps), function(.dis){
    .dat <- city.casereps[[.dis]]
    .index <- index(.dat)
    ret <- data.frame(
        Disease=.dis,
        min=format(min(.index)), max=format(max(.index)), N=length(.index)
    )
    ## this disease
    ret
})

## get case report totals + summary (for reprates)
## grab data and rename
## pass in names, build final dataframe in loop center
city.cases.totals <- ldply(.disease, function(.dis) {
    ## first get timeseries for cities
    .xts <- city.casereps[[.dis]]
    ## for each place/col
    ldply( colnames(.xts), function(.place){
        ## pull out the data
        .dat <- .xts[,.place]
        ## return object
        data.frame(
            Disease=.dis,
            placename=.place,
            ## sum cases
            cases=sum(.dat, na.rm=T),
            ## proportion zeros
            prop0=sum(.dat==0, na.rm=T)/nrow(.dat),
            ## prop and number NAs
            propNA=sum(is.na(.dat))/nrow(.dat),
            NAs=sum(is.na(.dat))
        )
    })
})
