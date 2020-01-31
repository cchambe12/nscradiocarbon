#### Now for plotting the models...
# Started 15 October 2019 by Cat


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



#load("stan/ringtotalconc_randincr.Rdata")
#load("stan/difftotalconc_randincr.Rdata")
#load("stan/ringsugarconc_randincr.Rdata")
#load("stan/diffsugarconc_randincr.Rdata")
load("stan/ringstarchconc_randincr.Rdata")
#load("stan/diffstarchconc_randincr.Rdata")


figpath <- "figures"
figpathmore <- "ringstarch_brms" ### change based on model

source("exp_muplot_brms.R")
cols <- adjustcolor("indianred3", alpha.f = 0.3) 
my.pal <- rep(brewer.pal(n = 8, name = "Dark2"), 5)

alphahere = 0.4
xlab <- "Model estimate of starch concentration"

df <- ring.starch
incr <- unique(df$increment)

modelhere <- ringstar.mod



intercept <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 1] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("intercept", "[", i, "]", sep="")
}
intercept$parameter<-new.names

summer <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 2] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select(mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("seasonbsummer", "[", i, "]", sep="")
}
summer$parameter<-new.names
mod.ranef <- full_join(intercept, summer)

fall <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 3] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("seasoncautumn", "[", i, "]", sep="")
}
fall$parameter<-new.names
mod.ranef<-full_join(mod.ranef, fall)

winter <- coef(modelhere, prob=c(0.25, 0.75))$incr[, c(1, 3:4), 4] %>%
  as.data.frame() %>%
  round(digits = 2) %>% 
  rename(mean = Estimate) %>%
  rename(`25%` = Q25) %>%
  rename(`75%` = Q75) %>%
  dplyr::select( mean, `25%`, `75%`) 
new.names<-NULL
for(i in 1:length(incr)){
  new.names[i]<-paste("winter", "[", i, "]", sep="")
}
winter$parameter<-new.names
mod.ranef <- full_join(mod.ranef, winter)

ints <- c("intercept[1]", "intercept[2]", "intercept[3]", "intercept[4]", "intercept[5]", "intercept[6]")
mod.ranef$int.mean <- rep(mod.ranef$mean[(mod.ranef$parameter%in%ints)])
mod.ranef$int.25 <- rep(mod.ranef$`25%`[mod.ranef$parameter%in%ints])
mod.ranef$int.75 <- rep(mod.ranef$`75%`[mod.ranef$parameter%in%ints])

mod.ranef$mean <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$mean + mod.ranef$int.mean, mod.ranef$mean)
mod.ranef$`25%` <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$`25%` + mod.ranef$int.25, mod.ranef$`25%`)
mod.ranef$`75%` <- ifelse(!(mod.ranef$parameter%in%ints), mod.ranef$`75%` + mod.ranef$int.75, mod.ranef$`75%`)

modoutput <- tidy(modelhere, prob=c(0.5))

muplotfx(modelhere, "", 8, 8, c(0,4), c(-10, 70) , 72, 3.5)



