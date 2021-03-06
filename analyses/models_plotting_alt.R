#### Now for plotting the models...
# Started 15 October 2019 by Cat
## with season as random effect


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
library(RColorBrewer)
library(broom)

## Load the data
setwd("~/Documents/git/nscradiocarbon/analyses/")

### Ring-porous first:
#ring <- read.csv("input/ring.csv")
#diff <- read.csv("input/diff.csv")

all <- read.csv("input/CC_data.csv")

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



load("stan/ringtotal_seas_imp.Rdata")
load("stan/difftotal_seas_imp.Rdata")
load("stan/ringsugar_seas_imp.Rdata")
load("stan/diffsugar_seas_imp.Rdata")
load("stan/ringstarch_seas_imp.Rdata")
load("stan/diffstarch_seas_imp.Rdata")


figpath <- "figures"
figpathmore <- "difftot_seas_imp" ### change based on model

xlab <- "Model estimate of total concentration (mg/g)"

df <- difftotimp
season <- unique(df$season)

modelhere <- diffseas.tot

source("source/exp_muplot_alt.R")
cols <- "black"
my.pal <- rep(c("indianred1", "darkseagreen4", "darkorange2", "dodgerblue3"), 4)
#my.pch <- rep(2:5)

alphahere = 0.6

intercept <- coef(modelhere, prob=c(0.25, 0.75))$season[, c(1, 3:4), 1] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(season)){
  new.names[i]<-paste("intercept", "[", i, "]", sep="")
}
intercept$parameter<-new.names

onetwo <- coef(modelhere, prob=c(0.25, 0.75))$season[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(season)){
  new.names[i]<-paste("increment1M2", "[", i, "]", sep="")
}
onetwo$parameter<-new.names
mod.ranef <- full_join(intercept, onetwo)

twothree <- coef(modelhere, prob=c(0.25, 0.75))$season[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(season)){
  new.names[i]<-paste("increment2M3", "[", i, "]", sep="")
}
twothree$parameter<-new.names
mod.ranef<-full_join(mod.ranef, twothree)

threefour <- coef(modelhere, prob=c(0.25, 0.75))$season[, c(1, 3:4), 4] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(season)){
  new.names[i]<-paste("increment3M4", "[", i, "]", sep="")
}
threefour$parameter<-new.names
mod.ranef <- full_join(mod.ranef, threefour)

foureight <- coef(modelhere, prob=c(0.25, 0.75))$season[, c(1, 3:4), 5] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(season)){
  new.names[i]<-paste("increment4M8", "[", i, "]", sep="")
}
foureight$parameter<-new.names
mod.ranef <- full_join(mod.ranef, foureight)

pith <- coef(modelhere, prob=c(0.25, 0.75))$season[, c(1, 3:4), 6] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(season)){
  new.names[i]<-paste("increment8Mpith", "[", i, "]", sep="")
}
pith$parameter<-new.names
mod.ranef <- full_join(mod.ranef, pith)

ints <- c("intercept[1]", "intercept[2]", "intercept[3]", "intercept[4]")
mod.ranef$int.mean <- rep(mod.ranef$mean[(mod.ranef$parameter%in%ints)])
mod.ranef$int.25 <- rep(mod.ranef$`25%`[mod.ranef$parameter%in%ints])
mod.ranef$int.75 <- rep(mod.ranef$`75%`[mod.ranef$parameter%in%ints])

mod.ranef$mean <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$mean + mod.ranef$int.mean, mod.ranef$mean)
mod.ranef$`25%` <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$`25%` + mod.ranef$int.25, mod.ranef$`25%`)
mod.ranef$`75%` <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$`75%` + mod.ranef$int.75, mod.ranef$`75%`)

modoutput <- tidy(modelhere, prob=c(0.5))

muplotfx(modelhere, "", 8, 8, c(0,6), c(-10, 70) , 72, 4.5)



##### Now let's plot the posteriors against the raw data
#load("stan/ringtotal_seas.Rdata")
#load("stan/difftotal_seas.Rdata")
#load("stan/ringsugar_seas.Rdata")
#load("stan/diffsugar_seas.Rdata")
#load("stan/ringstarch_seas.Rdata")
#load("stan/diffstarch_seas.Rdata")

pp <- as.data.frame(predict(diffseas.star, probs = c(0.25, 0.75)))
pp <- pp[c(1:513),]
yraw <- diff.starch$conc
pp.raw <- cbind(pp, yraw)

pred <- ggplot(pp.raw, aes(y=Estimate, x=yraw)) + geom_point() +
     ylab("Predicted starch concentration (mg/g)") + xlab("Observed starch concentration (mg/g)") +
     coord_cartesian(xlim = c(0, 150), ylim = c(0, 150))

pdf("~/Documents/git/nscradiocarbon/analyses/figures/diffstarch_predvraw.pdf", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=5,
    height=5)
grid.arrange(pred)
dev.off()

svg("~/Documents/git/nscradiocarbon/analyses/figures/diffstarch_predvraw.svg", width=5, height=5)
grid.arrange(pred)
dev.off()



#points(pp.raw$Q75 ~ ring.total$conc, col="blue", pch = 16)
#points(pp.raw$Estimate ~ ring.total$conc, col="black", pch = 16)
#abline(lm(pp.raw$Estimate ~ ring.total$conc), col="red")

