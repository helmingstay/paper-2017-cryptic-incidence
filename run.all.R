## load libraries 
source('run.init.R')
## definitions (called by run.init.R)
#source('run.params.R')
## and helper functions (uses some defns)
## (called by run.init.R)
#source('mk.helpers.R')
source('mk.newdist.R')
#source('mk.predict.R')


## US - Meas vs EW
## read in data, trim to complete years,
## pull out complete years and sum case reports 
## pushed up to ipums repo: source('run.lcasedata.R')

## read in data, minor processing
source('run.read.cases.R')
source('run.read.demog.R')
## compute reporting rates from demographics and case report sums
source('run.rates-reprate.R')

## get mean cases, join demographics,
## bootstrapping
## for both ipums and percap rates
source('run.fit.R')


.do.fresh <- FALSE
if (.do.fresh || !(exists('bootmod'))) {
    source('run.bootmod.R')
} else {
    warning("bootmod exists, run.bootmod.R not run.")
}

## aggregate by variable number of weeks
## includes rsqs for all separate models
source('run.agg.R')

#############
## plotting
#############
source('run.plot.prep.R')
source('plot.ccs.R')
source('plot.sample_range.tab.R')
source('plot.levelplots.R')
source('plot.cryptic_byreprate.R')
## see caption
#source('plot.persist_inferred.R')
#source('plot.compare_ipums.R')
#source('plot.persist_quantcases.R')
#source('plot.persist_compare.R')
source('plot.agg.R')
source('plot.bootmod.R')
## prep legends at the end
source('plot.legs.R')

## close devices
for (i in 1:length(dev.list())) {dev.off()}
