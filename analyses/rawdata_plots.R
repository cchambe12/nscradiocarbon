### 5 November 2019 - Cat
## Let's plot some raw data and compare it to the model to 
# make sure the model is doing what it's supposed to


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


my.pal <- rep(c("indianred3", "darkseagreen4", "darkorange2", "dodgerblue3"), 4)

ring.total$incr <- as.numeric(as.factor(ring.total$increment))
ringtot <- ggplot(ring.total, aes(x=incr, y=conc, col=as.factor(season))) + geom_jitter(alpha=0.5, width=0.25) +
  geom_smooth(method="lm", fill=NA, alpha=0.8) + xlab("Increment") + ylab("Total Concentration (mg/g)") +
  scale_color_manual(name="Season", values=c("indianred4","darkseagreen4",
                              "darkorange3","dodgerblue3"),
                     labels=c("aspring"="Spring",
                              "bsummer"="Summer",
                              "cautumn"="Autumn",
                              "winter"="Winter")) + xlim(breaks=c("0-1", "1-2", "2-3", "3-4",
                                                                  "4-8", "8-pith"))

rt.lm_vals = lm(ring.total$conc~ring.total$increment*ring.total$season)

quartz()
ringtot




