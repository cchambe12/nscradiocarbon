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
library(gridExtra)
library(dplyr)
library(tidyr)
library(brms)
library(rstanarm)
library(RColorBrewer)
library(broom)

## Load the data
setwd("~/Documents/git/nscradiocarbon/analyses/")

### Ring-porous first:
all <- read.csv("input/CC_data.csv")

ring <- all[(all$wood=="ring"),]
diff <- all[(all$wood=="diff"),]

ring.total <- ring[(ring$method=="total"),]
diff.total <- diff[(diff$method=="total"),]

ring.sugar <- ring[(ring$method=="sugar"),]
diff.sugar <- diff[(diff$method=="sugar"),]

ring.starch <- ring[(ring$method=="starch"),]
diff.starch <- diff[(diff$method=="starch"),]

#### Now we want to make a raw data plot that mirrors the muplot 
df <- diff.starch

source("source/rawdataprep.R")

cols <- "black"
my.pal <- rep(c("indianred1", "darkseagreen4", "darkorange2", "dodgerblue3"), 4)

rawplot<-ggplot(rawprep, aes(x=lower, xend=upper, y=jvar, yend=jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=jvar, col=group, size=group, alpha=group)) +
  geom_segment(arrow = arrow(length = unit(0.00, "npc")), aes(col=group, alpha=group)) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(rawprep$term[rawprep$group=="atotal"])), labels=rev(c("0-1", "1-2", "2-3", "3-4", "4-8", "8-pith"))) +
  xlab("Starch concentration (mg/g)") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(3,3,1,1), "lines")) +
  scale_color_manual(values=c(cols, my.pal), labels=sort(unique(rawprep$group))) +
  scale_size_manual(values=c(3,2,2,2,2), labels=sort(unique(rawprep$group))) +
  scale_alpha_manual(values=c(1,0.6,0.6,0.6,0.6), labels=sort(unique(rawprep$group))) +
  coord_cartesian(xlim=c(0,80))


png("figures/diffstarch_raw.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=7,
    height=6, units="in", res = 350 )
grid.arrange(rawplot)
dev.off()

svg("figures/diffstarch_raw.svg")
grid.arrange(rawplot)
dev.off()



####### Let's do some simple anovas and linear mixed models
library(lme4)
library(arm)
library(car)
  

all <- read.csv("input/CC_data.csv")

ring <- all[(all$wood=="ring"),]
diff <- all[(all$wood=="diff"),]

ring.sugar <- ring[(ring$method=="sugar"),]
diff.sugar <- diff[(diff$method=="sugar"),]
  
### First Diffuse
diff.sugar.01.mod<- aov(conc ~ season, data=diff.sugar[(diff.sugar$increment=="0-1"),])
summary(diff.sugar.01.mod)

diff.sugar.12.mod<- aov(conc ~ season, data=diff.sugar[(diff.sugar$increment=="1-2"),])
summary(diff.sugar.12.mod)

diff.sugar.23.mod<- aov(conc ~ season, data=diff.sugar[(diff.sugar$increment=="2-3"),])
summary(diff.sugar.23.mod)

diff.sugar.34.mod<- aov(conc ~ season, data=diff.sugar[(diff.sugar$increment=="3-4"),])
summary(diff.sugar.34.mod)

diff.sugar.48.mod<- aov(conc ~ season, data=diff.sugar[(diff.sugar$increment=="4-8"),])
summary(diff.sugar.48.mod)

diff.sugar.8p.mod<- aov(conc ~ season, data=diff.sugar[(diff.sugar$increment=="8-pith"),])
summary(diff.sugar.8p.mod)


### Now Ring
ring.sugar.01.mod<- aov(conc ~ season, data=ring.sugar[(ring.sugar$increment=="0-1"),])
summary(ring.sugar.01.mod)

ring.sugar.12.mod<- aov(conc ~ season, data=ring.sugar[(ring.sugar$increment=="1-2"),])
summary(ring.sugar.12.mod)

ring.sugar.23.mod<- aov(conc ~ season, data=ring.sugar[(ring.sugar$increment=="2-3"),])
summary(ring.sugar.23.mod)

ring.sugar.34.mod<- aov(conc ~ season, data=ring.sugar[(ring.sugar$increment=="3-4"),])
summary(ring.sugar.34.mod)

ring.sugar.48.mod<- aov(conc ~ season, data=ring.sugar[(ring.sugar$increment=="4-8"),])
summary(ring.sugar.48.mod)

ring.sugar.8p.mod<- aov(conc ~ season, data=ring.sugar[(ring.sugar$increment=="8-pith"),])
summary(ring.sugar.8p.mod)


if(FALSE){
#### Extra anovas
ringtotmod <- lm(conc ~ increment*season, data=ring.total)
display(ringtotmod)
Anova(ringtotmod)

difftotmod <- lm(conc ~ increment*season, data=diff.total)
display(difftotmod)
Anova(difftotmod)

ringsugmod <- lm(conc ~ increment*season, data=ring.sugar)
display(ringsugmod)
Anova(ringsugmod)

diffsugmod <- lm(conc ~ increment*season, data=diff.sugar)
display(diffsugmod)
Anova(diffsugmod)

ringstarmod <- lm(conc ~ increment*season, data=ring.starch)
display(ringstarmod)
Anova(ringstarmod)

diffstarmod <- lm(conc ~ increment*season, data=diff.starch)
display(diffstarmod)
Anova(diffstarmod)
}

