## melt observation cols
.persist.cols=c('Observed','Estimated','Cryptic')
bootmod.m <- melt(bootmod, 
    measure.vars=.persist.cols
)

## cleanup
## set negatives to zero
bootmod.m <- bootmod.m[value < 0, value := 0 ]
## remove col
bootmod.m[, Nobs := NULL]
    
## melt population cols
.pop.cols <- c( 'pop.rates', 'eff.pop')
.pop.labs <- c('Full', 'Monitored')
bootmod.m <- melt(bootmod.m, 
    measure.vars=.pop.cols,
    variable.name='pop.var',
    value.name='pop.val'
)

## remove boostrap eff.pop CI from all but observed
bootmod.m[
    (
        (probs != 0.5)
        & grepl('Observed', variable) 
    ),
    pop.val := NA
]
## prep for obsmod (complement of above)
## variable x: cast by pop.val / new.eff.pop 
## rather than value
obsmod <- bootmod.m[ 
    (
        grepl('Observed',variable) 
        & grepl('eff',pop.var)
    ),
]

## for observed, add back "real" data from fit.df
## and plot osmod separately
bootmod.m <- subset(bootmod.m,
    !grepl('Observed',variable),
    select= -new.eff.pop
) 

## replace observations
.obs.df <- subset(fit.df,
    select=c(Disease, placename, persist, pop.rates)
)
.obs.df <- .obs.df[, `:=`(
    probs=0.5,
    variable='Observed', value=persist, 
    pop.var='pop.rates', pop.val=pop.rates, 
    ## cleanup
    persist=NULL, pop.rates=NULL
)]
    
bootmod.m <- rbind(bootmod.m, .obs.df)

## helper:
mk.fix.pop.var <- function(fac) {
    factor(fac, levels=.pop.cols, labels=.pop.labs)
}

## relevel for plotting
bootmod.m[, pop.var := mk.fix.pop.var(pop.var)]
obsmod[, pop.var := mk.fix.pop.var(pop.var)]

## wide by quantile probs
bootmod.w <- dcast(bootmod.m, ... ~ probs, 
    value.var=c('value')
)
## omit pop.value
obsmod <- dcast(obsmod, 
    Disease + placename + variable + value + pop.var ~ probs, 
    value.var=c('new.eff.pop')
)

## helpers
mk.spaces <- function(x) sub(' ','~', x)
mk.strip <- function(.dt) {
    ret <- within(.dt, {
        strip.persist <- factor(variable, 
            levels=.persist.cols,
            labels=paste(.persist.cols, sep='~~',
                c('(phantom(.)*P[obs]*phantom(.))', '(phantom(.)*P[est]*phantom(.) )', '(phantom(.)*P[c]*phantom(.))')
            )
        )
        strip.pop <- mk.spaces( pop.var)
    })
}

## nice strip text
bootmod.w <- data.table(mk.strip(bootmod.w))
obsmod <- data.table(mk.strip(obsmod))

## panel labels
## position
.pos=list(top=rep(0.99,2), bot=rep(0.055,2))
## design matrix w/nice labels matching data
.txt.cols <- bootmod.w
## deep copy 
.txt.df <- obsmod[ Disease=='Pertussis',]
## prep for rbind
.txt.df[, `:=`(pop.val = `0.5`, value=NULL) ]
.txt.df <- rbind(.txt.df, bootmod.w)
##
.nudge.mon <- 0.9
.bg <- "#EEEEEE20"
## the actual text, positions
.txt.df = .txt.df[,
    by=.(strip.persist, strip.pop),
    j=list(min=min(pop.val), max=max(pop.val))
][, `:=`(
    txt = LETTERS[c(2,1,3:6)],
    ## smaller eff pop = right edge
    Disease = 'Pertussis',
    yy = with(.pos, c(top, bot, top)),
    xx = c(min*c(.nudge.mon,1.2,1.2,.nudge.mon,1.2,.nudge.mon)) #, max[5:6]/1.5)
)]

## grab factor levels for panel B
.mod.line <- subset(obsmod,
    pop.var == 'Monitored' & variable == 'Observed'
)
## add fitted model points
.mod.line <- .mod.line[, by='Disease', j={
    ret <- copy(.SD)
    .mod <- lmod[[Disease]][[1]]
    ret$eff.pop <- ret[["0.5"]]
    ret$yy <- predict(.mod, newdata=ret, type='response')
    ret
}]

plot.bootmod <- (
    ggplot(bootmod.w, aes(
        y=`0.5`, 
        color=Disease, shape=Disease
    ))
    + facet_grid(strip.persist ~ strip.pop, 
        #space='free_x', 
        scales='free_x',
        labeller=label_parsed
    )
    ## model line in panel B, plot first
    + geom_line(data=.mod.line, 
        aes(y=yy, x=`0.5`, group=Disease), 
        color='black',
        linetype=2, size=0.6, alpha=0.7,
        inherit.aes=F
    )
    + geom_point(aes(x=pop.val), alpha=0.8, size=1)
    + geom_linerange(
        aes(x=pop.val, ymin=`0.025`, ymax=`0.975`),
        alpha=0.4, size=0.3, na.rm=T
    )
    ## x-range: model
    + geom_point(data=obsmod, 
        aes(x=`0.5`, y=value),
        alpha=0.8, size=1
    )
    + geom_segment(data=obsmod,
        aes(x=`0.025`, xend=`0.975`, y=value, yend=value),
        alpha=0.4, size=0.3, na.rm=F
    )
    ## panel labels
    + geom_label(data=.txt.df, 
        aes(y=yy, x=xx, label=txt),
        size=6, color='black', 
        show.legend=F, fill=.bg
    )
    + theme_bw()
    + scale_shape_discrete('Disease', solid=F)
    + scale_x_log10('Population (Full vs. Monitored, 1930)')
    + scale_y_continuous('Presence (1/wk)', limits=c(-0.01, 1.04))
    + scale_color_manual(values=my3cols)
    + theme(
        legend.position=c(0.998,0.32),
        legend.justification=c(1,1)
    ) 
    + theme.leg.back
)
