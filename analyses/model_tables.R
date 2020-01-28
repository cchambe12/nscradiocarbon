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
#load("stan/radioring_mod.Rdata")
load("stan/radiodiff_mod.Rdata")

modelhere <- raddiff.mod

if(FALSE){
fit50 <- as.data.frame(summary(modelhere, prob=0.5)$fixed)
fit50 <- dplyr::select(fit50, -Eff.Sample, -Rhat)
fit98 <- as.data.frame(summary(modelhere, prob=0.975)$fixed)
fit98 <- subset(fit98, select=c("l-97.5% CI", "u-97.5% CI"))

fit <- cbind(fit50, fit98)

colnames(fit) <- c("mean", "sd", "25%", "50%", "2.5%", "97.5%")

write.csv(fit, file="output/ringradiomodel.csv", row.names=TRUE)
}

all50 <- broom::tidy(modelhere, prob=c(0.5))
all975 <- broom::tidy(modelhere, prob=c(0.975))

all50 <- all50[c(1:6,29:52),]
all975 <- all975[c(1:6,29:52),]

colnames(all50) <- c("term", "mean", "sd", "25%", "75%")
colnames(all975) <- c("term", "mean", "sd", "2.5%", "97.5%")

all <- full_join(all50, all975)

fit <- all[,-1]
rownames(fit) <- c("$\\mu_{\\alpha}$", "$\\mu_{increment 1-2}$", 
                   "$\\mu_{increment 2-3}$", "$\\mu_{increment 3-4}$","$\\mu_{increment 4-8}$",
                   "$\\mu_{increment 8-pith}$", 
                   "$\\mu_{\\alphaSpring}$", "$\\mu_{\\alphaSummer}$", "$\\mu_{\\alphaAutumn}$",
                   "$\\mu_{\\alphaWinter}$",
                   "$\\mu_{\\increment 1-2Spring}$", "$\\mu_{\\increment 1-2Summer}$", "$\\mu_{\\increment 1-2Autumn}$",
                   "$\\mu_{\\increment 1-2Winter}$",
                   "$\\mu_{\\increment 2-3Spring}$", "$\\mu_{\\increment 2-3Summer}$", "$\\mu_{\\increment 2-3Autumn}$",
                   "$\\mu_{\\increment 2-3Winter}$",
                   "$\\mu_{\\increment 3-4Spring}$", "$\\mu_{\\increment 3-4Summer}$", "$\\mu_{\\increment 3-4Autumn}$",
                   "$\\mu_{\\increment 3-4Winter}$",
                   "$\\mu_{\\increment 4-8Spring}$", "$\\mu_{\\increment 4-8Summer}$", "$\\mu_{\\increment 4-8Autumn}$",
                   "$\\mu_{\\increment 4-8Winter}$",
                   "$\\mu_{\\increment 8-pithSpring}$", "$\\mu_{\\increment 8-pithSummer}$", "$\\mu_{\\increment 8-pithAutumn}$",
                   "$\\mu_{\\increment 8-pithWinter}$") 



write.csv(fit,"output/diffstarch_tab.csv", row.names = TRUE)
