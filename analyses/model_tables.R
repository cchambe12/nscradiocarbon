### Now let's make all of the tables

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

#load("stan/ringtotal_seas.Rdata")
#load("stan/difftotal_seas.Rdata")
#load("stan/ringsugar_seas.Rdata")
#load("stan/diffsugar_seas.Rdata")
#load("stan/ringstarch_seas.Rdata")
#load("stan/diffstarch_seas.Rdata")

modelhere <- diffseas.star

fit50 <- as.data.frame(summary(modelhere, prob=0.5)$fixed)
fit50 <- dplyr::select(fit50, -Eff.Sample, -Rhat)
fit98 <- as.data.frame(summary(modelhere, prob=0.975)$fixed)
fit98 <- subset(fit98, select=c("l-97.5% CI", "u-97.5% CI"))

fit <- cbind(fit50, fit98)

#add column names to all sub tables
colnames(fit)<-c("mean", "sd", "25%", "75%", "2.5%", "97.5%")

row.names(fit)<-c("$\\mu_{\\alpha}$", "$\\mu_{increment 1-2}$", 
                   "$\\mu_{increment 2-3}$", "$\\mu_{increment 3-4}$","$\\mu_{increment 4-8}$",
                   "$\\mu_{increment 8-pith}$") 


write.csv(fit,"output/diffstarch_tab.csv", row.names = TRUE)
