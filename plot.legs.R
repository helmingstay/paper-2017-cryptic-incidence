## DEPRECATED
##### legends (should be captions)
## organize figure legends into list
.legs <- list()
## prep plots, captions

.legs$persist_inferred= 'Observed ($P_o$, blue circles) and estimated ($\\widetilde{P}$, green triangles) persistence versus mean case reports ($m_C$).  We define observed persistence as the long-time, per-week probability of non-zero case reports; ${m}_C$ is a proxy for observed incidence. For each disease, a linear model (Figure \\ref{fig:persist_quantcases}) was used to estimate persistence from incidence by assuming 100\\% reporting. In general, the difference between observed and estimated persistence decreases with increasing incidence.  95\\% bootstrap prediction intervals are shown.  See Figure \\ref{fig:persist_compare} for details.'


## Supplemental
.legs$compare_ipums="Difference in measures of persistence by demographic data source.  X axis: results based on per capita birth and infant mortality rates (used in main text).  Y axis:  results based on census microdata.  The result sets generally agree, while the per capita method yields smaller reporting probabilities and higher persistence estimates, particularly for pertussis. Black dashed line: one-to-one line; blue solid line: linear regression. Approximate 95\\% bootstrap confidence intervals (CI) are included as cross-hairs. With few exceptions, each CI includes the other method's estimate, as shown by intersections with the one-to-one line. See \\citep{gunning2014conserved} for a detailed comparison of methods."

.legs$persist_quantcases <- sprintf('Scaling of observed persistence [ cloglog($P_o$) ]
 with mean case reports [ log(${m}_C$) ]. GLM fits are also shown. Model pseudo-$R^2$ for linear models: Measles~=~%s; Pertussis~=~%s.', mods.rsq.1$Measles,
mods.rsq.1$Pertussis) 

.legs$persist_compare <- "Observed (estimated) persistence versus observed (estimated) incidence.  The meaning of the x-axis changes between groups: observed persistence is plotted against mean case reports ($m_C$), while estimated persistence is plotted against mean cases at full reporting ($\\widetilde{\\mu}_I$). Correction for incomplete reporting transforms case reports (observed incidence) to cases (estimated incidence).  The linear model for each disease is then used to extrapolate an estimate of persistence for each location. The overall motion of a location's point is rightwards (correction for incomplete reporting) and upwards (new model prediction).  Thus uncertainty in estimated incidence includes uncertainty in both reporting probability and linear model predictions."

## prep for label
