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


rad.mod <- brm(age ~ increment*wood, data=radio,
               control=list(max_treedepth = 15,adapt_delta = 0.99))


