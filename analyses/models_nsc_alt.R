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
#ring <- read.csv("input/ring.csv")
#diff <- read.csv("input/diff.csv")

all <- read.csv("input/CC_data.csv")

### Now I will attempt to impute data that is missing for 8-pith
missingdata <- data.frame(species=rep(c(rep("QURU", each=3), rep("ACRU", each=3), rep("BEPA", each=3), rep("FRAM", each=3)), times=9*3), 
                          method=rep(c(rep("total", each=12), rep("sugar", each=12), rep("starch", each=12)), times=9), 
                          month=rep(c("February", "March", "April", "May", "June", "August", "September", "October", "November"), each=3*4*3),
                          increment=rep("8-pith", each=4*3*9*3),
                          conc=rep(NA, each=4*3*9*3))

spring <- c("March", "April", "May")
summer <- c("June", "July", "August")
autumn <- c("September", "October", "November")
winter <- c("January", "February")

missingdata$season <- NA
missingdata$season <- ifelse(missingdata$month%in%spring, "spring", missingdata$season)
missingdata$season <- ifelse(missingdata$month%in%summer, "summer", missingdata$season)
missingdata$season <- ifelse(missingdata$month%in%autumn, "autumn", missingdata$season)
missingdata$season <- ifelse(missingdata$month%in%winter, "winter", missingdata$season)

ring <- c("QURU", "FRAM")
missingdata$wood <- ifelse(missingdata$species%in%ring, "ring", "diff")


all <- dplyr::select(all, species, month, increment, conc, season, method, wood)

all <- rbind(all, missingdata)



ring <- all[(all$wood=="ring"),]
diff <- all[(all$wood=="diff"),]

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

set.seed(1234)
random.imp <- function (a){ #a=ring.starch$conc
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE) 
  return (imputed)
}

ringtotimp <- ring.total
ringtotimp$conc[(difftotimp$increment=="8-pith")] <- random.imp(ringtotimp$conc[(ringtotimp$increment=="8-pith")])
difftotimp <- diff.total
difftotimp$conc[(difftotimp$increment=="8-pith")] <- random.imp(difftotimp$conc[(ringtotimp$increment=="8-pith")])

ringsugimp <- ring.sugar
ringsugimp$conc[(ringsugimp$increment=="8-pith")] <- random.imp(ringsugimp$conc[(ringtotimp$increment=="8-pith")])
diffsugimp <- diff.sugar
diffsugimp$conc[(diffsugimp$increment=="8-pith")] <- random.imp(diffsugimp$conc[(ringtotimp$increment=="8-pith")])

ringstarimp <- ring.starch
ringstarimp$conc[(ringstarimp$increment=="8-pith")] <- random.imp(ringstarimp$conc[(ringtotimp$increment=="8-pith")])
diffstarimp <- diff.starch
diffstarimp$conc[(diffstarimp$increment=="8-pith")] <- random.imp(diffstarimp$conc[(ringtotimp$increment=="8-pith")])

#### Total
ringseas.tot <- brm(conc ~ increment + (increment | season), data=ringtotimp, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(ringseas.tot, file="stan/ringtotal_seas_imp.Rdata")

diffseas.tot <- brm(conc ~ increment + (increment | season), data=difftotimp, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(diffseas.tot, file="stan/difftotal_seas_imp.Rdata")

#### Sugar
ringseas.sug <- brm(conc ~ increment + (increment | season), data=ringsugimp, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(ringseas.sug, file="stan/ringsugar_seas_imp.Rdata")

diffseas.sug <- brm(conc ~ increment + (increment | season), data=diffsugimp, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(diffseas.sug, file="stan/diffsugar_seas_imp.Rdata")

#### Starch
ringseas.star <- brm(conc ~ increment + (increment | season), data=ringstarimp, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(ringseas.star, file="stan/ringstarch_seas_imp.Rdata")

diffseas.star <- brm(conc ~ increment + (increment | season), data=diffstarimp, 
                     control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                     prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                       prior(normal(0,10), class = "b"))

save(diffseas.star, file="stan/diffstarch_seas.Rdata")



##### Alternative to calculating total now...
ringstarimp_alt <- ringstarimp
ringstarimp_alt$starchconc <- ringstarimp_alt$conc
ringstarimp_alt$conc <- NULL
ringstarimp_alt$method <- "total"
ringsugimp_alt <- ringsugimp
ringsugimp_alt$sugarconc <- ringsugimp_alt$conc
ringsugimp_alt$conc <- NULL
ringsugimp_alt$method <- "total"

ring.pith <- cbind(ringstarimp_alt, ringsugimp_alt$sugarconc)
ring.pith <- ring.pith[(ring.pith$increment=="8-pith"),]
goodmonths <- c("January", "July")
ring.pith <- ring.pith[!(ring.pith$month%in%goodmonths),]

ring.pith$conc <- ring.pith$starchconc + ring.pith$`ringsugimp_alt$sugarconc`
ring.pith$starchconc <- NULL
ring.pith$`ringsugimp_alt$sugarconc` <- NULL
ring.pith <- subset(ring.pith, select=c("species", "month", "increment", "conc", "season", "method", "wood"))

ring.total_comb <- ring.total[!is.na(ring.total$conc),]
ring.total_comb <- rbind(ring.total_comb, ring.pith)

#### Total
ringseas.tot.comb <- brm(conc ~ increment + (increment | season), data=ring.total_comb, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(ringseas.tot.comb, file="stan/ringtotal_seas_imp_comb.Rdata")

diffseas.tot <- brm(conc ~ increment + (increment | season), data=diff.total_comb, 
                    control=list(max_treedepth = 15,adapt_delta = 0.99), iter=2000, warmup=1500,
                    prior = prior(normal(0, 30), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                      prior(normal(0,10), class = "b"))

save(diffseas.tot, file="stan/difftotal_seas_imp_comb.Rdata")

