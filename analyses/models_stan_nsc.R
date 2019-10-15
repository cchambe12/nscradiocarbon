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

ring.total <- ring[(ring$method=="total"),]
diff.total <- diff[(diff$method=="total"),]

ring.sugar <- ring[(ring$method=="sugar"),]
diff.sugar <- diff[(diff$method=="sugar"),]

ring.starch <- ring[(ring$method=="starch"),]
diff.starch <- diff[(diff$method=="starch"),]

## Better understand the data:
range(ring.total$conc, na.rm=TRUE) ## 6.9 - 152.3
mean(ring.total$conc, na.rm = TRUE) # 25
sd(ring.total$conc, na.rm = TRUE) ## 19


#tot.arm.inc <- stan_glmer(conc ~ increment + (increment | season), data=ring.total,
 #                         control=list(max_treedepth = 15,adapt_delta = 0.99))

if(FALSE){
set.seed(12345)
ringtot.mod <- stan_glmer(conc ~ increment + (increment | season), data=ring.total,
                            prior_intercept = student_t(21,10,0.1), 
                          control=list(max_treedepth = 15,adapt_delta = 0.99))



difftot.mod <- stan_glmer(conc ~ increment + (increment | season), data=diff.total,
                         prior_intercept = student_t(21,10,0.1), 
                         control=list(max_treedepth = 15,adapt_delta = 0.99))

}

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



