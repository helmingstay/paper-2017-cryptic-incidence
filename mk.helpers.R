## logistic functions
odds=function(x) (x/(1-x))
logit=function(x) log(x/(1-x))
logistic=function(x) exp(x)/exp(x+1)
logit2=function(x) log2(x/(1-x))
logistic2=function(x) (2^x)/(2^x+1)
cloglog = function(x) log(-log(1-x))
cloglog.inv = function(x) 1-exp(-exp(x))

## put this in run.init?
#the_family <- binomial()
#$Measles
#[1] 3209.725
#$Pertussis
#[1] 2395.041

#$Measles
#[1] 2592.149
#$Pertussis
#[1] 2069.98

#the_family <- binomial(link='probit')
#$Measles
#[1] 3017.572
#$Pertussis
#[1] 2194.29



## actually used 
untrans.x <- exp
trans.x <- log
#untrans.x <- function(x) x
#trans.x <- function(x) x
#### y
#trans.y <- cloglog 
#untrans.y <- cloglog.inv
untrans.y <- the_family$linkinv
trans.y <-  the_family$linkfun
## fit model to untransformed x?

## L-estimator
mk.lest <- function(x, probs=c(0.1, 0.25, 0.5, 0.75, 0.9)) {
    mean(quantile(x, probs=probs))
}

## helper functions
## transform in and out of model-space
## testing different transforms
## see run.predict.R
#do.trans <- function(x) log10(logit2(x))
#undo.trans <- function(x) logistic2(10^x)
#do.trans <- function(x) log10(qnorm(x, mean=10))
#undo.trans <- function(x) pnorm(10^x, mean=10)
## AIC 
# $Measles [1] -508.7253
# $`Whooping cough` [1] -507.0707
#do.trans <- function(x) logit2(x)
#undo.trans <- function(x) logistic2(x)
do.trans <- function(x) log10(atanh(x))
undo.trans <- function(x) tanh(10^x)
## AIC of above
#> AIC(.mods.list[[1]])
#[1] -181.4297
#> AIC(.mods.list[[2]])
#[1] -173.6313
## testing just atanh
#do.trans <- function(x) atanh(x)
#undo.trans <- function(x) tanh(x)

##  sample parametric reprate bootstraps draws into a matrix 
##  using columns of means/sds from data.frame
mk.boots <- function(.nboot, .nplaces, .df,
    unif.factor=2,
    mean.col, sd.col, ret.col, include.cols='placename',
    do.min=F
){
    ## prepare return df
    .ret <- .df[,include.cols]
    ## index of bootstrap samples
    ## this makes .ret the correct dimensions -- loooong
    .ret <- cbind(.ret, index = rep(1:.nboot, each=.nplaces))
    ## the actual draws
    .mean <- .df[[mean.col]]
    .sd=.df[[sd.col]]
    .dat <- .df[[mean.col]]
    #.low <- exp(log(.dat)*unif.factor)
    #.hi <- exp(log(.dat)/unif.factor)
    #.draws <- runif(.nboot*.nplaces, .low, .hi)
    .draws <- rnorm(.nboot*.nplaces, .mean, .sd)
    ## place in named column
    .ret[[ret.col]]  <- .draws
    ## prevent negative bootstraps??
    #if (do.min) {
        #.ret[[ret.col]] <- min(1e-6, .ret[[ret.col]] )
    #}
    .ret
}

## panel for confidence intervals based on SE in provided data.frame
## need to untransform and retransform
mk.panel.ci <- function(x,y,.dat, err.col='l.trans.pe', .alpha=0.05, .inverse=F, ...){
    ## pull out these rows from dataframe in panel function call
    #.dat <- predict.df[subscripts,]
    ## by default, plot CI
    .gpars <- list(...)
    if (length(err.col) !=2) { stop('err.col must have length 2')
    } else {
        ## confidence intervals are already in the right form
        ## hi/low order doesn't matter...
        .hi <- .dat[[err.col[1] ]]
        .low <- .dat[[err.col[2] ]]
    }
    if (.inverse) {
        ## horizontal
        lsegments(.hi, y, .low, y, col=.gpars$col.symbol, lwd=1.5, alpha=0.3)
    } else {
        ## vertical
        lsegments(x, .hi, x, .low, col=.gpars$col.symbol, lwd=1.5, alpha=0.3)
    }
}


mk.prop.dev <- function(x, .as.string=T) {
    ## takes a glm, returns prop reduction diviance 
    ## see zheng 2000
    D <- 1 - x$deviance/x$null.deviance
    if (.as.string) {
        D <- sprintf('%2.3f', D)
    }
    return(D)
}

## helper function
## rotate x by n 
mk.rotate <- function(x, n) {
    c(tail(x, -n), head(x,n))
}

## convenience name cleanup
mk.relevel.disease <- function(x) {
    revalue(x, c(`Whooping cough`='Pertussis'))
}
##
mk.rename.disease <- function(x) {
    rename(x, c(`Whooping cough`='Pertussis'))
}

## helper, mainly for lapply and ilk
mk.names <- function(x) {
    names(x) <- x
    x
}
