## Copied from prev-scaling repo, reprate branch
## depends on ldemog from full.RData 
## and cache.demog.RData 
#source('run.load.ipums.R')

get.year <- function(.date) {
    ## convenience function to get numeric year
    ## from date (input as a factor)
    as.numeric(format(as.Date(as.character(.date)), format = "%Y")) 
}

mk.S <- function(.rates){
    ## surviving (integer) births per year, add to .rates
    .rates <- within(.rates, {
        S <- (birth*pop)*(1-infant)
        S<- round(S)
    })
    .rates
}

## get reporting rates for each disease, place
mk.rates <- function(.case.totals, .range, .rates, .pop.year=1930) {
    ## combine in sample range and case reports
    .case.totals <- merge(.case.totals, .range)
    ## for each, subset relevant years
    .outret <- ddply(.case.totals, .(Disease, placename),
    function(.df){
        ## get years we have cases for as seq
        .years <- get.year(.df$min):get.year(.df$max)
        ## get per cap rates for this place
        .S <- merge(.rates, .df)
        ## rates just for case-years
        .S <- subset(.S, year %in% .years)
        ## add a bunch of stuff to return df
        .inret <- within(.df, {
            ## add in fixed population for graphing / analysis
            pop <- pop.rates <- subset(.S, year==.pop.year)$pop
            ## nyears we have cases for - nyears we have rates for
            ## should be zero
            missing.years <- length(.years) - nrow(subset(.S, !is.na(S)))
            ## total S for this disease / place
            S.rates <- sum(.S$S)
            ## add reporting rate back to df
            reprate <- reprate.rates <- cases/S.rates
        })
        .inret
    })
    .outret
}


library(data.table)
#' Compute S from bootstrap draws of 
#' (city) pop, (state) birth, and (us) infant mortality 
#' from avail per capita rates (trimmed to  casereport timerange )
#' Divide cases by bootstrapped S -> reprate
#' data.table allows reasonable runtime
mk.boot.reprate <- function(.sample.range, .case.totals, 
    .place.pops, .infant.rates, .birth.rates, .nboot=1e3
) {
    ## for each disease
    ret <- ddply(.sample.range, 'Disease', function(.range){
        ## trim case totals to this disease
        .totals <- subset(.case.totals, Disease==.range$Disease, 
            select=c(placename, Disease, cases)
        )
        .totals <- as.data.table(droplevels(.totals))
        ## get places
        .places <- .totals$placename
        .nplace <- length(.places)
        ## Construct range of years to trim rates
        .range <- with(.range, data.frame(
            Disease, year=get.year(.range$min):get.year(.range$max)
        ))
        ## just get data col, subset to data time range 
        .infant.rates <- na.omit(merge( .range,
            subset(.infant.rates, select=c(year, infant))
        ))$infant
        .birth.rates <- na.omit(merge(.range, 
                subset(.birth.rates, select=c(year, birth))
        ))$birth
        .place.pops <- na.omit(merge(.range, 
                subset(.place.pops, select=c(placename, year, pop))
        ))
        ## bootstrap draws.  For each draw,
        ## sample on birth and infant mortality rate for full time period
        .draws <- data.table(
            placename = .places,
            index = rep(1:.nboot, each=.nplace),
            birth = sample(.birth.rates, .nboot*.nplace, replace=T),
            infant = sample(.infant.rates, .nboot*.nplace, replace=T)
        )
        ## use merge to replace prior values of birth/infant w/draws
        .place.pops <- as.data.table(.place.pops)
        .draws <- merge(.place.pops, .draws, by=c('placename'), allow.cartesian=T)
        ## compute yearly S, return
        .draws <- mk.S(.draws)
        .draws <- .draws[, by=c('index', 'placename'), j=list(S.boot = sum(S))]
        ## add in case report totals, get reprate
        .draws <- merge(.draws, .totals, by='placename')
        .draws[, boot.reprate := cases /S.boot]
        ## cleanup
        .draws <- subset(.draws, select=c(placename, Disease, index, boot.reprate))
        .draws
    })
    ret
}
