.levelplots <- list()
.sumplots <- list()
.levelplots.captions <- list()

.dat <- city.casereps
## make a levelplot of each timeseries
for (.dis in names(.dat)) {
    ## pull out list of xts
    this.dat <- .dat[[.dis]]
    .sum <- apply(this.dat, 1, sum, na.rm=T)
    .sum <- data.frame(date=index(this.dat), sum=.sum)
    ## ggp
    .pp <- (
        ggplot(.sum, aes(x=date, y=sum)) 
        + geom_line()
        + theme_bw()
        + theme(
            plot.margin=unit(c(1,0,0,0),units='lines')
        )
        + scale_y_sqrt()
        + scale_x_date(expand=rep(0, 2))
        + ylab('Case reports\n(weekly sum)')
        + xlab('')
    )
    .sumplots[[.dis]] <- .pp

    ## plot variance scaled...
    for (ii in 1:ncol(this.dat)) {
        this.dat[,ii] <- this.dat[,ii]/sd(this.dat[,ii], na.rm=T)
    }
    ## prep xlab
    tmpy=head(endpoints(this.dat, on='years'), -1)[-1];
    ## plot every other place
    tmpy = tmpy[as.logical((1:length(tmpy))%%2)]
    tmplab = .indexyear(this.dat)[tmpy]+1901
    ## number of color ramp division
    .ncolor <- 50
    ## subset of cities for plotting
    .pcols <- seq(from=1, to=ncol(this.dat), by=3)
    tmpplot <- this.dat[,.pcols]
    .plot <- levelplot(
        coredata(tmpplot), aspect='fill',
        scales=list(
            y=list(cex=0.85, rot=0), 
            x=list(at=tmpy, labels=tmplab, cex=1.2)
        ), par.settings=theme.base,
        ## make sure zero is black. NA is white, all else colorramp
        col.regions=c('#000000',
            colorRampPalette(
                c('blue', 'turquoise4', 'red', 'magenta'),
                space='Lab'
            )(.ncolor)
        ),
        ## again, only 0 should be black.  remove galveston -- stupid high
        at=c(0, 
            seq(1e-9, max(tmpplot[,!grepl('Galveston, TX', colnames(tmpplot))], na.rm=T), length.out=.ncolor-1)
        ),
        xlab='', #
        ylab=''#,
    )
    ## generate caption, store in list 
    ## need to fix & in England
    .levelplots.captions[[.dis]] <- sprintf("\\textbf{%s. A}. Total weekly case reports (all cities, sqrt-transformed). \\textbf{B}. Weekly case reports (black = 0; white = missing). One city per row, ordered by population size, showing every third city. To facilitate comparison between cities, each city's observations are scaled by that city's temporal variance.", .dis)
    ## store plots in a list
    .levelplots[[.dis]] <- .plot
}

