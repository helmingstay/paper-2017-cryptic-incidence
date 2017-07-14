source('mk.rates-reprate.R')
## data prepared / subsetted in run.read.data.R

## get yearly S from birth and infant mortality
city.rates <- mk.S(city.rates)

## reporting rates from per capita data
city.reprates <- mk.rates(city.cases.totals, city.sample.range, city.rates)
## merge in IPUMS reprates, only available for a subset of cities
## keep all, many NAs for IPUMS estimates 
#city.reprates <- merge(city.reprates, .ipums.city.reprates, all=T)
city.reprates <- droplevels(city.reprates)

#nboot.reprate.rates <- 5e3
boot.reprate.rates <- mk.boot.reprate(
    city.sample.range, city.cases.totals, 
    .place.pops=city.rates, .birth.rates=state.birth, 
    .infant.rates=us.infant, .nboot=.nboot
)
## ipums.se from cache, 
## see run.read.data.R
#city.reprates <- merge(city.reprates, city.boot.ipums.se, all=T)
