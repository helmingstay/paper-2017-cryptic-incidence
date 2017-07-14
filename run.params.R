## main Definitions 
the_family <- binomial(link='cloglog')
#the_family <- binomial()

## ?? where?
## bootstrap draws of prediction
.nboot <- 1e4

agg.weeks = c(1,2,4, 8, 16)

## xts time range by disease
time.limits <- list(Measles='::2000', Pertussis='::1943')
## remarkably resistant, tho slope decreases with +agg
#agg.weeks = c(1,2,4, 8, 16, 32)

##################
## the model
##################
## model aggregated by week 
## (tests different formulations)
## worst
#form_vars <- list(obs='sum.cases', est='sum.cases.est')
##better
#form_vars <- list(obs='mean.cases', est='mean.cases.est')
## collapse the following onto one curve
#agg.pred <- within(agg.pred, eff.pop <- eff.pop*agg)
## best
form_vars <- list(obs='eff.pop', est='pop.rates')
## the final formulas
## main logistic glm model
form_base <- formula(sprintf(
    'cbind(n.persist, n.zero) ~ log(%s)', form_vars$obs
))
## aggregated by week
form_agg <- formula(sprintf(
    'cbind(n.persist, n.zero) ~ log(%s)*Weeks', form_vars$obs
    # 'persist ~ log(%s)*Weeks', form_vars$obs
))
