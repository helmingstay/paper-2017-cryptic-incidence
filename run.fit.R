## see run.all.R for dependencies / call sequence
## 
## compute ipums predictions for comparison
## just changing names...
## use parametric bootstrap here (which is confusing)
#new.city.reprates <- within(city.reprates, {
    #reprate.use <- reprate.ipums
    #se.use <- reprate.ipums.se 
#})
## predict persist at full reporting, cryptic persist
## for each disease, get predictions
#.fit.df <- mk.newdist(new.city.reprates, city.casereps)
## strip out missing places
#.fit.df <- droplevels(na.omit(.fit.df))
#l.ipums <- list(
    #fit.df = .fit.df#,
    #predict.df = mk.predict(.fit.df, form_base)
#)

## as above with per capita reprates 
## just changing names
city.reprates <- within(city.reprates, {
    reprate.use <- reprate.rates 
})
## build statistical model, 
## predict persist at full reporting, cryptic persist
## 
## use non-parametric boostrapping for census,
## instead use draws from mk.boot.reprate
fit.df <- mk.newdist(city.reprates, city.casereps)
fit.df <- data.table(fit.df, 
    key=c('Disease','placename')
)
## delete ipums column
#fit.df$reprate.ipums <- NULL
#predict.df <- mk.predict(fit.df, form_base, .boot.rates=boot.reprate.rates)
