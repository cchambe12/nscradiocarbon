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
ring <- read.csv("input/ring.csv")
diff <- read.csv("input/diff.csv")

ring.total <- ring[(ring$method=="total"),]
diff.total <- diff[(diff$method=="total"),]

ring.sugar <- ring[(ring$method=="sugar"),]
diff.sugar <- diff[(diff$method=="sugar"),]

ring.starch <- ring[(ring$method=="starch"),]
diff.starch <- diff[(diff$method=="starch"),]

#### Now we want to make a raw data plot that mirrors the muplot 
df <- diff.total

source("source/rawdataprep.R")

cols <- "black"
my.pal <- rep(c("indianred1", "darkseagreen4", "darkorange2", "dodgerblue3"), 4)

rawplot<-ggplot(rawprep, aes(x=lower, xend=upper, y=jvar, yend=jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estimate, y=jvar, col=group, size=group, alpha=group)) +
  geom_segment(arrow = arrow(length = unit(0.00, "npc")), aes(col=group, alpha=group)) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(rawprep$term[rawprep$group=="atotal"])), labels=rev(c("0-1", "1-2", "2-3", "3-4", "4-8", "8-pith"))) +
  xlab("Total concentration (mg/g)") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(3,3,1,1), "lines")) +
  scale_color_manual(values=c(cols, my.pal), labels=sort(unique(rawprep$group))) +
  scale_size_manual(values=c(3,2,2,2,2), labels=sort(unique(rawprep$group))) +
  scale_alpha_manual(values=c(1,0.6,0.6,0.6,0.6), labels=sort(unique(rawprep$group))) +
  coord_cartesian(xlim=c(0,60))


png("figures/difftotal_raw.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=7,
    height=6, units="in", res = 350 )
grid.arrange(rawplot)
dev.off()

svg("figures/difftotal_raw.svg")
grid.arrange(rawplot)
dev.off()


if(FALSE){
####### Let's do some simple anovas and linear mixed models
library(lme4)
library(arm)
library(car)

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

