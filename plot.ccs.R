library(latticeExtra)
## compute "empirical ccs" for a range of alpha
## CCS $:= min(Population) | P > alpha$)
.dat.plot <- subset(agg.pred.1, 
    select=c(Disease, pop.rates, obs.fit, est.fit)
)
.tmpplot <- melt(.dat.plot, measure.vars=c('obs.fit', 'est.fit'))
.alphas = data.frame(alpha=c(seq(0.5, 0.95, by=0.01)))
.ccs <- ddply(.tmpplot, c('Disease', 'variable'), function(.df) {
    ddply(.alphas, 'alpha', function(.aa) {
        persists <- subset(.df, value >.aa$alpha)
        data.frame(pop = min(persists$pop.rates))
    })
})

## only report pops greater than min pop
.minpop <- min(.dat.plot$pop.rates)
.ccs <- subset(.ccs, pop>.minpop)


## clean up labels
#trellis.par.set(theme.2col)
.tmptheme <- theme.2col
.tmptheme$superpose.line$lty <- c(2,1)
.tmptheme$superpose.line$lwd <- 2.2
.ccs$variable <- factor(.ccs$variable, 
    levels=c('obs.fit', 'est.fit'), 
    labels=c('Observed', 'Estimated')
)

.plot.ccs <- xyplot(pop ~ alpha | Disease, groups=variable, .ccs, 
    type=c('l','g'), 
    scales=list(y=list(relation='free', log=T, abbreviate=T)), 
    yscale.components = yscale.components.log,
    as.table=T, layout=c(1,2), 
    par.settings=.tmptheme,
    auto.key=list(x=0, y=0.9, corner=c(0,1), points=F, lines=T),
    ylab=expression(CCS[alpha]~(Population)), xlab=expression(alpha~(`for`~P > alpha))
)

.plot.ccs <- (
    ggplot(.ccs, 
        aes(x=alpha, y=pop, color=variable, linetype=variable, group=variable), 
        log='y'
    )
    + facet_wrap(~Disease, ncol=1, scales='free_y')
    + geom_line(size=1.3)
    +ylab(expression(CCS[alpha]~(Population)))
    +xlab(expression(alpha~(`for`~P > alpha)))
    + theme_bw()
    + theme(
        legend.position=c(0.02, 0.98),
        legend.justification=c(0,1)
    )
    + theme.leg.back
    + scale_color_manual(NULL, values=my2cols)
    + scale_linetype_discrete(NULL)
)

## Min observable 
.ccs.wc <- subset(.ccs, grepl('Pertuss', Disease) & variable == 'Estimated')
.ccs.wc <- subset(.ccs.wc, alpha == min(alpha))
.ccs.wc$min <- .ccs.wc$alpha - 0.01 
.ccs.wc$per <- .ccs.wc$min *100


## get pop predicted/observed for each disease at 95%
## for use in text
## rounded to 1e4 
.ccs.pop <- round(dcast(
    subset(.ccs, alpha==0.95), ... ~ Disease + variable, value.var='pop'
)/1e3, digits=-1)
