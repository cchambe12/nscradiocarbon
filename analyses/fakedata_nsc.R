### 14 October 2019 - Cat
## Now with real data...


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
library(lme4)

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


### Okay, now let's make some fake data using help Rethinking, Gelman, OSPREE and Geoff
#  1) Let's make the observations much higher than the actual data to build a good model.
nseas = 4 # number of seasons
ninc = 6 # numbers of increments 
nobs = 100 # number of observations per increment per season

#test <- list(seas.env = rep(seq(1, 4, by=1), each=ninc*nobs),
 #                inc.env = rep(seq(1, 6, by=1), times=nseas*nobs))

seas.env = as.numeric(as.factor(rep(c("aspring", "bsummer", "cautumn", "dwinter"), each=ninc*nobs)))

intercept <- ifelse(seas.env==1, 0, seas.env)
summer <- ifelse(seas.env==2, 1, seas.env)
fall <- ifelse(seas.env==3, 2, seas.env)
winter <- ifelse(seas.env==4, 3, seas.env)

sample_a <- list(summer, fall, winter)

model.parameters <- list(intercept = 55,
                         summer.coef = -3,
                         fall.coef = -1,
                         winter.coef = -2)

#  2) Now, we will make varying intercepts
env.samples <- sapply(sample_a, FUN = function(x){
  sample(x, size = nobs * ninc, replace = TRUE)})
#foo <- sapply(test, FUN = function(x){
 # sample(x, size = nseas * nobs * ninc, replace = TRUE)})
mm <- model.matrix(~env.samples)
#mm <- mm[,-2]

#  4) We need to make a random intercept model for each species
parameters.temp <- matrix(unlist(model.parameters), ncol = length(model.parameters), nrow = nseas * nobs * ninc, byrow = TRUE)
# Which parameters are random?
random.regex <- grep(pattern = paste(c("intercept", "summer.coef", "fall.coef", "winter.coef"), collapse = "|"), x = names(model.parameters))
# Generate random parameters (by species)
for(i in 1:length(random.regex)){
  parameters.temp[, i] <- sapply(1:ninc, FUN = function(x){
    rep(rnorm(n = 1, mean = model.parameters[[random.regex[i]]], sd = 1), nobs*nseas)})}
# Calculate response
response <- sapply(1:nrow(env.samples), FUN = function(x){
  rnorm(n = 1, mean = mm[x, ] %*% parameters.temp[x, ], sd = 1)})

fakedata_nsc <- cbind(data.frame(increment = as.vector(sapply(1:ninc, FUN = function(x) rep(x, nobs*nseas))),
                                 season = env.samples[,1],
                                 conc = response))

write.csv(fakedata_nsc, file="output/fakedata_nsc.csv", row.names = FALSE)


#### Quick model check:

fakemod <- lmer(conc ~ season + (season|increment), data=fakedata_nsc)

get_prior(conc ~ season + (season|increment), data=ring.total)

priorpreds_vague <- brm(chains=1,iter=100,
                       sample_prior = 'only',
                       data=ring.total,
                       prior = prior(normal(0, 18), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                         prior(normal(0,1), class = "b"),
                       formula = conc ~ season + (season|increment))

priorpreds_wip <- brm(chains=1,iter=100,
                       sample_prior = 'only',
                       data=ring.total,
                       prior = prior(normal(0, 18), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                         prior(normal(0,10), class = "b"),
                       formula = conc ~ season + (season|increment))


#fakebrm <- brm(conc ~ season + (season|increment), data=ring.total,
 #                  control=list(max_treedepth = 15, adapt_delta = 0.99),
  #                 prior = prior(normal(0, 50), class = "Intercept") + prior(normal(0, 10), class = "sd") +
   #                  prior(normal(0,10), class = "b"))


fakebrm_wip <- brm(conc ~ season + (season|increment), data=ring.total,
               control=list(max_treedepth = 15, adapt_delta = 0.99),
               prior = prior(normal(0, 50), class = "Intercept") + prior(normal(0, 30), class = "sd") +
                 prior(normal(0,30), class = "b"))


fakebrm_vague <- brm(conc ~ season + (season|increment), data=ring.total,
                   control=list(max_treedepth = 15, adapt_delta = 0.99),
                   prior = prior(normal(0, 18), class = "Intercept") + prior(normal(0, 10), class = "sd") +
                     prior(normal(0,10), class = "b"))


fakebrm_informative <- brm(conc ~ season + (season|increment), data=ring.total,
                     control=list(max_treedepth = 15, adapt_delta = 0.99),
                     prior = prior(normal(20, 5), class = "Intercept") + prior(normal(0, 5), class = "sd") +
                       prior(normal(0,5), class = "b"))







