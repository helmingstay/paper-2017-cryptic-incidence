## assumes source('run.read.cases.R')
load('cache.demog.RData')
## see also repo ipums and repo helen
#load('reprate.cache/cache.demog.RData')
#load('reprate.cache/cache.city.boot.ipums.se.RData')

## ldemog from full.RData
## city demographic data
## birth rates are from yearly state per capita
## infant rates are yearly US
city.rates <- subset(ldemog$city.rates, 
    select=c(placename, year, pop, birth, death, infant)
)

## IPUMS reprates, only available for a subset of cities
#.ipums.city.reprates <- subset(all.demog, area=='City')
## pull original per capita rate estimates
state.birth <- ldemog$state.rates.cast
state.birth <- rename(state.birth, c(Births = 'birth'))
us.infant <- ldemog$US.yr

#.ipums.city.reprates$Disease <- mk.relevel.disease( .ipums.city.reprates$Disease)
## from cache/cache.city.boot.ipums.se.RData
#city.boot.ipums.se$Disease <- mk.relevel.disease( city.boot.ipums.se$Disease)
