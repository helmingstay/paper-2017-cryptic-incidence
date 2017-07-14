require(plyr)
require(lubridate)

## from run.rates-reprate.R
.tmpdf <- ddply(city.sample.range, c('Disease'), function(.df) {
    ## what we have estimates for
    ## left join - by this disease
    .data <- join(.df, agg.pred.1)
    .data <- droplevels(.data)
    ## get number of locations for each level
    .df$L <- nrow(.data)
    .df
})
## clean up for final table
.tmpdf <- within(.tmpdf, {
    max <- year(max)
    min <- year(min)
})
## grab reporting rates -- mean and sd
##my.report <- function(x) sprintf('%0.2f [%1.2f]', mean(x), sd(x)/mean(x))
my.report <- function(x) {
    #if (any(x<=0)) stop("x==0 in function my.report, geom mean")
    #.mean <- exp(mean(log(x)))
    #.sd <- exp(sd(log(x)))
    .mean <- mean(x)
    .quants <- quantile(x, probs=c(0.25, 0.75))
    .cv <- sd(x)/mean(x)
    ret <- sprintf('%0.2f [%0.2f-%0.2f, %1.2f]', .mean, .quants[1], .quants[2], .cv)
    ret
}
.tmp.persist <- ddply(agg.pred.1, c('Disease'), function(.df) { 
    with(.df, 
        data.frame(
            Reporting =my.report(reprate.use),
            Observed =my.report(obs.fit),
            Predicted =my.report(est.fit),
            Cryptic =my.report(crypt.fit)
        )
    )
})

## merge in reporting rates
.tmpdf <- merge(.tmpdf, .tmp.persist)
.tmpdf$range <- with(.tmpdf, sprintf('%s - %s', min, max))
## reorder
.tmpdf <- subset(.tmpdf, 
    select=c(Disease, L, N, range, Reporting, Observed, Predicted, Cryptic)
)
.tmpdf <- rename(.tmpdf, c(
    N = "Total Weeks",
    L = "Total Cities (with $P_{obs} <1$)",
    range = "Date Range",
    Reporting = "Reporting Probability ($r$)",
    Observed = "Observed Presence ($P_{obs}$)",
    Predicted = "Estimated Presence ($P_{est}$)",
    Cryptic = "Cryptic Presence ($P_c$)"
))
.tab.sample_range <- t(.tmpdf)
#.tmpdf <- .tmpdf[, 
    #c('Disease', 'Area', 'L', 'N', 'Start', 'End', 'Mean', 'CV')
#]

## output to file
#print(.tab.sample_range, file='figs/sample_range.tex', size=.size, table.placement='h!', include.rownames=T, include.colnames=F, sanitize.text.function=I )
## output for knitr
