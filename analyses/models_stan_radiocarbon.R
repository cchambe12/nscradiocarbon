### 14 October 2019 - Cat
## Radiocarbon data

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

radio <- read.csv("input/radiocarbon.csv")

rad.diff <- radio[(radio$wood=="diff"),]
rad.ring <- radio[(radio$wood=="ring"),]


raddiff.mod <- brm(age ~ increment, data=rad.diff,
               control=list(max_treedepth = 15,adapt_delta = 0.99))

save(raddiff.mod, file="stan/radiodiff_mod.Rdata")

radring.mod <- brm(age ~ increment, data=rad.ring,
                   control=list(max_treedepth = 15,adapt_delta = 0.99))

save(radring.mod, file="stan/radioring_mod.Rdata")


######## Now for some plotting... #####
#setwd("~/Documents/git/nscradiocarbon")
#load("orig_full.Rdata")

if(FALSE){
  raddiff<-as.data.frame(tidy(raddiff.mod, prob=0.9))
  names(raddiff)<-c("term", "estimate", "error", "10%", "90%")
  raddiff50<-as.data.frame(tidy(raddiff.mod, prob=0.5))
  names(raddiff50)<-c("term", "estimate", "error", "25%", "75%")
  raddiff <- full_join(raddiff, raddiff50)
  raddiff98<-as.data.frame(tidy(raddiff.mod, prob=0.98))
  names(raddiff98)<-c("term", "estimate", "error", "2%", "98%")
  raddiff <- full_join(raddiff, raddiff98)
  raddiff <- subset(raddiff, select=c("term", "estimate", "2%", "10%", "25%", "75%", "90%", "98%"))
  write.csv(raddiff, file="output/raddiffuse.csv", row.names=FALSE)
}
#raddiff <- read.csv("output/raddiffuse.csv", header=TRUE)

if(FALSE){
  radring<-as.data.frame(tidy(radring.mod, prob=0.9))
  names(radring)<-c("term", "estimate", "error", "10%", "90%")
  radring50<-as.data.frame(tidy(radring.mod, prob=0.5))
  names(radring50)<-c("term", "estimate", "error", "25%", "75%")
  radring <- full_join(radring, radring50)
  radring98<-as.data.frame(tidy(radring.mod, prob=0.98))
  names(radring98)<-c("term", "estimate", "error", "2%", "98%")
  radring <- full_join(radring, radring98)
  radring <- subset(radring, select=c("term", "estimate", "2%", "10%", "25%", "75%", "90%", "98%"))
  write.csv(radring, file="output/radringuse.csv", row.names=FALSE)
}
#radring <- read.csv("output/radringuse.csv", header=TRUE)

### Now to make the plots
modoutput <- radring50 #modelhere

modoutput$term <- ifelse(modoutput$term=="b_Intercept", "b_incrementbranch", modoutput$term)
modoutput<-modoutput[1:10,]
modoutput$term<-gsub(".*b_increment","",modoutput$term)

modoutput$intercept <- modoutput$estimate[modoutput$term=="branch"]
modoutput$int.25 <- modoutput$`25%`[modoutput$term=="branch"]
modoutput$int.75 <- modoutput$`75%`[modoutput$term=="branch"]

modoutput$estclean <- NA
modoutput$estclean <- ifelse(modoutput$term!="branch",
                             modoutput$estimate + modoutput$intercept, modoutput$estimate)
modoutput$clean.25 <- ifelse(modoutput$term!="branch",
                             modoutput$`25%` + modoutput$int.25, modoutput$`25%`)
modoutput$clean.75 <- ifelse(modoutput$term!="branch",
                             modoutput$`75%` + modoutput$int.75, modoutput$`75%`)

modoutput$Jvar<-seq(1, 10, by=1)
modoutput$Jvar <- rev(modoutput$Jvar)



estimates<-c("Branch", "Fineroot", "Root 1", "Root 2", "Root 3", "Root 4",
             "Shoot 1", "Shoot 2","Shoot 3", "Shoot 4")
estimates<-rev(estimates)

radcarb<-ggplot(modoutput, aes(x=clean.25, xend=clean.75, y=Jvar, yend=Jvar)) +
  geom_vline(xintercept=0, linetype="dotted") + geom_point(aes(x=estclean, y=Jvar)) +
  geom_segment(arrow = arrow(length = unit(0.00, "npc"))) +
  guides(size=FALSE) +
  scale_y_discrete(limits = sort(unique(modoutput$term)), labels=estimates) +
  xlab("Model estimate of change in age (ring porous)") + ylab("") + theme_linedraw() +
  theme(legend.text=element_text(size=7), legend.title = element_text(size=9), 
        legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans"), legend.position = "none",
        legend.text.align = 0,
        plot.margin = unit(c(3,3,1,1), "lines")) +  
  coord_cartesian(xlim=c(-5, 20), ylim=c(1,10), clip = 'off') + 
  annotate("segment", x = 0.05, xend = 21.2, y = 10.75, yend = 10.75, colour = "black", size=0.2, arrow=arrow(length=unit(0.20,"cm"))) +
  annotate("segment", x = -0.05, xend = -6.2, y = 10.75, yend = 10.75, colour = "black", size=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 10, y = 11, colour = "black", size=3, label="Older") + 
  annotate("text", x = -2.5, y = 11, colour = "black", size=3, label="Younger") 
  
quartz()
radcarb


png("figures/radiocarbon_ring50.png", ### makes it a nice png and saves it so it doesn't take forever to load as a pdf!
    width=7,
    height=6, units="in", res = 350 )
grid.arrange(radcarb)
dev.off()

