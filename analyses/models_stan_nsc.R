### 14 October 2019 - Cat
## Now with real data...


### Main Question:
# This is really what I want to show - 
# that concentrations vary seasonally A LOT in shallower increments, and only a little in deeper increments.
## And this varies between diffuse and ring-porous wood anatomies 

## Main Issues with data:
# Right skew of data for total and total sugar concentration
## zero-inflated starch concentration


## housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

library(ggplot2)
library(dplyr)
library(tidyr)
library(brms)
library(rstanarm)

## Load the data
setwd("~/Documents/git/nscradiocarbon/analyses/")

### Ring-porous first:
ring <- read.csv("input/ring.csv")
diff <- read.csv("input/diff.csv")

ring$season <- ifelse(ring$season=="spring", "aspring", ring$season)
ring$season <- ifelse(ring$season=="summer", "bsummer", ring$season)
ring$season <- ifelse(ring$season=="autumn", "cautumn", ring$season)

diff$season <- ifelse(diff$season=="spring", "aspring", diff$season)
diff$season <- ifelse(diff$season=="summer", "bsummer", diff$season)
diff$season <- ifelse(diff$season=="autumn", "cautumn", diff$season)

ring.total <- ring[(ring$method=="total"),]
diff.total <- diff[(diff$method=="total"),]

ring.sugar <- ring[(ring$method=="sugar"),]
diff.sugar <- diff[(diff$method=="sugar"),]

ring.starch <- ring[(ring$method=="starch"),]
diff.starch <- diff[(diff$method=="starch"),]


if(FALSE){
ringtot.mod <- brm(conc ~ season + (season | increment), data=ring.total, 
                          control=list(max_treedepth = 15,adapt_delta = 0.99))

save(ringtot.mod, file="stan/ringtotalconc_randincr.Rdata")

difftot.mod <- brm(conc ~ season + (season | increment), data=diff.total,
                         control=list(max_treedepth = 15,adapt_delta = 0.99))

save(difftot.mod, file="stan/difftotalconc_randincr.Rdata")
}

ringsug.mod <- brm(conc ~ season + (season | increment), data=ring.sugar, 
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(ringsug.mod, file="stan/ringsugarconc_randincr.Rdata")

diffsug.mod <- brm(conc ~ season + (season | increment), data=diff.sugar,
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(diffsug.mod, file="stan/diffsugarconc_randincr.Rdata")

ringstar.mod <- brm(conc ~ season + (season | increment), data=ring.starch, 
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(ringstar.mod, file="stan/ringstarchconc_randincr.Rdata")

diffstar.mod <- brm(conc ~ season + (season | increment), data=diff.starch,
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(diffstar.mod, file="stan/diffstarchconc_randincr.Rdata")



if(FALSE){
alltot <- full_join(ring.total, diff.total)
alltot$season <- ifelse(alltot$season=="spring", "aspring", alltot$season)
alltot$season <- ifelse(alltot$season=="summer", "bsummer", alltot$season)
alltot$season <- ifelse(alltot$season=="autumn", "cautumn", alltot$season)


allseas.mod <- brm(conc ~ increment*wood + (increment*wood|season), data=alltot,
                         control=list(max_treedepth = 15,adapt_delta = 0.99))

save(allseas.mod, file="stan/allwood_seasrand.Rdata")

#alltot$ring <- ifelse(alltot$wood=="ring", 1, 0)
allincr.mod <- brm(conc ~ season*wood + (season*wood | increment), data=alltot,
                         control=list(max_treedepth = 15,adapt_delta = 0.99))

save(allincr.mod, file="stan/allwood_incrrand.Rdata")


#loo1 <- loo(tot.arm.inc)
#loo2 <- loo(tot.arm.student)

#compare_models(loo1, loo2)

launch_shinystan(ringtot.mod)
}


