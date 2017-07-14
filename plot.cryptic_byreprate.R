## False extinction ("apparent extinction" sense ecolets) - 
## by reporting rate and population
## Fig 3c

.dat.plot <- subset(agg.pred, agg==1)
.plot.crypt <- (
    ggplot(.dat.plot,
        aes(x=reprate.rates, y=crypt.fit, size=pop.rates, color=Disease, shape=Disease )
    )
    ## loess fit, exclude from legend
    + geom_smooth(alpha=0.25, show.legend=F)
    + geom_point(alpha=0.70)
    + .theme_ggplot
    + theme.leg.back
    + theme(
        legend.position=c(0.07, 0.18), 
        legend.box.just='left'
    )
    + scale_size(name='Population', trans='log10', range=c(1.5,6))
    + scale_color_brewer(palette='Set1')
    + scale_x_continuous(name='Reporting probability', trans='log10') 
    + scale_y_continuous(name='Cryptic Presence (Estimated - Observed, 1/wk)')
)


## subplot of loess residuals
## first grab residuals
.tmpdat <- ddply(.dat.plot, 'Disease', function(.df){
    .mod <- loess(crypt.fit ~ log10(reprate.rates), .df)
    .df$resid <- resid(.mod)
    .df
})
## plot, no legend, use same symbology as above
.plot.crypt.sub <- ggplot(.tmpdat, aes(x=pop.rates, y=resid, color=Disease, shape=Disease) ) +
    geom_hline(yintercept=0, alpha=0.25, size=0.5) +
    geom_point(alpha=0.7) + 
    #geom_smooth(method='loess', alpha=0.2) + 
    .theme_ggplot + 
    theme(plot.margin = unit(c(0,0,0,0), "cm")) +
    theme(
        legend.position='none', 
        plot.background = element_rect(fill = "transparent",colour = NA)
    ) +
    scale_color_brewer(palette='Set1') +
    scale_y_continuous(name='Excess Cryptic Presence\n(Loess Resid. | Reporting)') +
    scale_x_continuous(name='Population', trans='log10')

