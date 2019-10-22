### 14 October 2019 - Cat
## Now with season as the random effect and increment as fixed effect


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

#### Total
ringseas.tot <- brm(conc ~ increment + (increment | season), data=ring.total, 
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(ringseas.tot, file="stan/ringtotal_seas.Rdata")

diffseas.tot <- brm(conc ~ increment + (increment | season), data=diff.total,
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(diffseas.tot, file="stan/difftotal_seas.Rdata")

#### Sugar
ringseas.sug <- brm(conc ~ increment + (increment | season), data=ring.sugar, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99))

save(ringseas.sug, file="stan/ringsugar_seas.Rdata")

diffseas.sug <- brm(conc ~ increment + (increment | season), data=diff.sugar,
                    control=list(max_treedepth = 15,adapt_delta = 0.99))

save(diffseas.sug, file="stan/diffsugar_seas.Rdata")

#### Starch
ringseas.star <- brm(conc ~ increment + (increment | season), data=ring.starch, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99))

save(ringseas.star, file="stan/ringstarch_seas.Rdata")

diffseas.star <- brm(conc ~ increment + (increment | season), data=diff.starch,
                    control=list(max_treedepth = 15,adapt_delta = 0.99))

save(diffseas.star, file="stan/diffstarch_seas.Rdata")



