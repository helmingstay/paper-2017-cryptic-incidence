## see run.agg.R 

## report for in-text \Sexpr 
mods.rsq.1 <- dcast(
    agg.rsq[Weeks=='1', j=list(Disease, rsq)], 
    . ~Disease, value.var='rsq'
)
## report, #weeks
.max.wks = dcast.data.table(
    agg.rsq[Weeks=='16',j=.(Disease,max.obs)], 
    . ~ Disease 
)
## get rid of dot
.max.wks[,`.`:=NULL]
## maximum proportion of min wks time
.max.prop = as.list(
    sprintf('%0.3f', (.max.wks-1)/.max.wks)
)
names(.max.prop) = names(.max.wks)


## prep table
## table of pseudo-rsq by aggregate week and disease
## sort
setkey(agg.rsq, Disease, Weeks) 
agg.rsq.tab <- agg.rsq[,j=list(Disease, Weeks, ncity, max.obs, rsq)]
agg.rsq.tab <- rename(agg.rsq.tab, c(
    Weeks="Window Length $W$ (weeks)",
    ncity="City \\#",
    max.obs="Window \\#",
    rsq="Pseudo-$R^2$"
))

## descending order of nweeks 
.agg.guide <- guide_legend(reverse = TRUE)
## legend text
.agg.name <- 'W'
## observed
## facet by disease, color by week
.plot.agg <- (
    ggplot(agg.pred, 
        aes(x=get(form_vars$obs), y=obs.y.trans, col=Weeks, shape=Weeks)
    )
    + theme_bw()
    + geom_point(size=1.5)
    + geom_line(aes(y=obs.fit.link, group=Weeks), alpha=0.4)
    + facet_grid(Disease ~ ., scales='free_y')
    + scale_x_log10('Monitored Population (1930)')
    #+ scale_color_brewer(name=.agg.name, palette='Spectral')
    + scale_color_discrete(name=.agg.name, guide=.agg.guide)
    + scale_shape_discrete(name=.agg.name, guide=.agg.guide, solid=F)
    #+ scale_shape_manual(name=.agg.name, values=c(1:3,5))
    #+ scale_y_continuous(name='Predicted Persistence')
    + scale_y_continuous(name=expression(P[Obs](W)~(cloglog)))
    + theme(
        legend.position=c(0.99, 0.01),
        #legend.position='right'
        legend.justification=c(1,0)
    )
    + theme.leg.back
) 

## legend text
.dis.name <- 'Disease'
## predicted
## facet by week, color by disease
.plot.agg.dis <- (
    ggplot(agg.pred, 
        aes(x=get(form_vars$est), y=est.fit.link, col=Disease, shape=Weeks)
    )
    + theme_bw()
    + geom_point(size=1.5, alpha=0.8)
    ## cryptic = diff...
    #+ geom_linerange(aes(ymin=obs.y.trans, ymax=est.fit.link))
    + geom_line(aes(y=est.fit.link, group=group, linetype=Disease), alpha=0.4)
    #+ facet_grid(weeks ~ ., scales='free_y')
    + scale_x_log10('Population (1930)')
    #+ scale_color_brewer(name=.dis.name, palette='Spectral')
    + scale_color_manual(name=.dis.name, values=my3cols)
    + scale_shape_discrete(name=.agg.name, guide=.agg.guide, solid=F)
    + guides(shape='none')
    #+ scale_shape_manual(name=.dis.name, values=c(1:3,5))
    + scale_y_continuous(name=expression(P[Est](W)~(cloglog)))
    + theme(
        legend.position=c(0.99, 0.01),
        #legend.position=c(1.01,-0.04),
        legend.justification=c(1,0),
        legend.box='horizontal',
        legend.box.just='bottom'
    )
    + theme.leg.back
) 
