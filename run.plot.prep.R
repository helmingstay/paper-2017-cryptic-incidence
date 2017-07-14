postscript(fonts='Times')
.fig.fontsize <- 9

.theme_ggplot <- theme_bw(base_size=.fig.fontsize+2, base_family='Times')
## prepare plotting
## red/blue for most
my3cols <- brewer.pal(3, 'Set1')
## alternate, blue/green
my2cols <- my3cols[2:3]
## lattice themes
theme.base <- list(
    fontsize=list(text=.fig.fontsize),
    grid.pars = list(fontfamily = "Times"),
    ## grab ggplot strip colors
    strip.background = list(col=theme_bw()$strip.background$fill)
)
theme.3col <- within( theme.base, {
    superpose.symbol <- list(col=my3cols, fill=my3cols, pch=1:2, cex=c(1,0.75))
    superpose.line <- list(col=my3cols)
})
##
theme.2col <- within(theme.base, {
    superpose.symbol <- list(col=my2cols, fill=my2cols, pch=1:2, cex=c(1,0.75))
    superpose.line <- list(col=my2cols)
})

theme.leg.back <- theme(legend.background=element_rect(fill='#CCCCCC50'))
