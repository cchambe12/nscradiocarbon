##### Source file for building dfs with raw data for muplots


rawprep <- data.frame(term=c("total01", "spring01", "summer01", "autumn01", "winter01",
                             "total12", "spring12", "summer12", "autumn12", "winter12",
                             "total23", "spring23", "summer23", "autumn23", "winter23",
                             "total34", "spring34", "summer34", "autumn34", "winter34",
                             "total48", "spring48", "summer48", "autumn48", "winter48",
                             "total8", "spring8", "summer8", "autumn8", "winter8"), estimate=NA,
                      lower=NA, upper=NA, se=NA, group=rep(c("atotal", "bspring", "csummer", "dautumn", "ewinter"), 6))

rawprep$estimate <- ifelse(rawprep$term=="total01", mean(df$conc[df$increment=="0-1"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="total12", mean(df$conc[df$increment=="1-2"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="total23", mean(df$conc[df$increment=="2-3"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="total34", mean(df$conc[df$increment=="3-4"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="total48", mean(df$conc[df$increment=="4-8"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="total8", mean(df$conc[df$increment=="8-pith"], na.rm=TRUE), rawprep$estimate)

rawprep$se <- (ifelse(rawprep$term=="total01", sd(df$conc[df$increment=="0-1"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="0-1"])), rawprep$se))
rawprep$se <- ifelse(rawprep$term=="total12", sd(df$conc[df$increment=="1-2"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="1-2"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="total23", sd(df$conc[df$increment=="2-3"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="2-3"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="total34", sd(df$conc[df$increment=="3-4"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="3-4"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="total48", sd(df$conc[df$increment=="4-8"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="4-8"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="total8", sd(df$conc[df$increment=="8-pith"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="8-pith"])), rawprep$se)

rawprep$estimate <- ifelse(rawprep$term=="spring01", mean(df$conc[df$increment=="0-1" & df$season=="spring"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="spring12", mean(df$conc[df$increment=="1-2" & df$season=="spring"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="spring23", mean(df$conc[df$increment=="2-3" & df$season=="spring"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="spring34", mean(df$conc[df$increment=="3-4" & df$season=="spring"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="spring48", mean(df$conc[df$increment=="4-8" & df$season=="spring"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="spring8", mean(df$conc[df$increment=="8-pith" & df$season=="spring"], na.rm=TRUE), rawprep$estimate)

rawprep$se <- ifelse(rawprep$term=="spring01", sd(df$conc[df$increment=="0-1" & df$season=="spring"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="0-1" & df$season=="spring"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="spring12", sd(df$conc[df$increment=="1-2" & df$season=="spring"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="1-2" & df$season=="spring"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="spring23", sd(df$conc[df$increment=="2-3" & df$season=="spring"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="2-3" & df$season=="spring"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="spring34", sd(df$conc[df$increment=="3-4" & df$season=="spring"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="3-4" & df$season=="spring"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="spring48", sd(df$conc[df$increment=="4-8" & df$season=="spring"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="4-8" & df$season=="spring"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="spring8", sd(df$conc[df$increment=="8-pith" & df$season=="spring"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="8-pith" & df$season=="spring"])), rawprep$se)

rawprep$estimate <- ifelse(rawprep$term=="summer01", mean(df$conc[df$increment=="0-1" & df$season=="summer"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="summer12", mean(df$conc[df$increment=="1-2" & df$season=="summer"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="summer23", mean(df$conc[df$increment=="2-3" & df$season=="summer"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="summer34", mean(df$conc[df$increment=="3-4" & df$season=="summer"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="summer48", mean(df$conc[df$increment=="4-8" & df$season=="summer"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="summer8", mean(df$conc[df$increment=="8-pith" & df$season=="summer"], na.rm=TRUE), rawprep$estimate)

rawprep$se <- ifelse(rawprep$term=="summer01", sd(df$conc[df$increment=="0-1" & df$season=="summer"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="0-1" & df$season=="summer"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="summer12", sd(df$conc[df$increment=="1-2" & df$season=="summer"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="1-2" & df$season=="summer"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="summer23", sd(df$conc[df$increment=="2-3" & df$season=="summer"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="2-3" & df$season=="summer"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="summer34", sd(df$conc[df$increment=="3-4" & df$season=="summer"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="3-4" & df$season=="summer"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="summer48", sd(df$conc[df$increment=="4-8" & df$season=="summer"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="4-8" & df$season=="summer"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="summer8", sd(df$conc[df$increment=="8-pith" & df$season=="summer"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="8-pith" & df$season=="summer"])), rawprep$se)

rawprep$estimate <- ifelse(rawprep$term=="autumn01", mean(df$conc[df$increment=="0-1" & df$season=="autumn"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="autumn12", mean(df$conc[df$increment=="1-2" & df$season=="autumn"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="autumn23", mean(df$conc[df$increment=="2-3" & df$season=="autumn"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="autumn34", mean(df$conc[df$increment=="3-4" & df$season=="autumn"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="autumn48", mean(df$conc[df$increment=="4-8" & df$season=="autumn"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="autumn8", mean(df$conc[df$increment=="8-pith" & df$season=="autumn"], na.rm=TRUE), rawprep$estimate)

rawprep$se <- ifelse(rawprep$term=="autumn01", sd(df$conc[df$increment=="0-1" & df$season=="autumn"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="0-1" & df$season=="autumn"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="autumn12", sd(df$conc[df$increment=="1-2" & df$season=="autumn"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="1-2" & df$season=="autumn"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="autumn23", sd(df$conc[df$increment=="2-3" & df$season=="autumn"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="2-3" & df$season=="autumn"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="autumn34", sd(df$conc[df$increment=="3-4" & df$season=="autumn"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="3-4" & df$season=="autumn"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="autumn48", sd(df$conc[df$increment=="4-8" & df$season=="autumn"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="4-8" & df$season=="autumn"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="autumn8", sd(df$conc[df$increment=="8-pith" & df$season=="autumn"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="8-pith" & df$season=="autumn"])), rawprep$se)

rawprep$estimate <- ifelse(rawprep$term=="winter01", mean(df$conc[df$increment=="0-1" & df$season=="winter"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="winter12", mean(df$conc[df$increment=="1-2" & df$season=="winter"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="winter23", mean(df$conc[df$increment=="2-3" & df$season=="winter"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="winter34", mean(df$conc[df$increment=="3-4" & df$season=="winter"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="winter48", mean(df$conc[df$increment=="4-8" & df$season=="winter"], na.rm=TRUE), rawprep$estimate)
rawprep$estimate <- ifelse(rawprep$term=="winter8", mean(df$conc[df$increment=="8-pith" & df$season=="winter"], na.rm=TRUE), rawprep$estimate)

rawprep$se <- ifelse(rawprep$term=="winter01", sd(df$conc[df$increment=="0-1" & df$season=="winter"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="0-1" & df$season=="winter"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="winter12", sd(df$conc[df$increment=="1-2" & df$season=="winter"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="1-2" & df$season=="winter"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="winter23", sd(df$conc[df$increment=="2-3" & df$season=="winter"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="2-3" & df$season=="winter"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="winter34", sd(df$conc[df$increment=="3-4" & df$season=="winter"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="3-4" & df$season=="winter"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="winter48", sd(df$conc[df$increment=="4-8" & df$season=="winter"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="4-8" & df$season=="winter"])), rawprep$se)
rawprep$se <- ifelse(rawprep$term=="winter8", sd(df$conc[df$increment=="8-pith" & df$season=="winter"], na.rm=TRUE)/sqrt(length(df$conc[df$increment=="8-pith" & df$season=="winter"])), rawprep$se)

rawprep$upper <- rawprep$estimate + rawprep$se
rawprep$lower <- rawprep$estimate - rawprep$se

rawprep$jvar <- NA
rawprep$jvar <- ifelse(rawprep$term=="total01", 6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="spring01", 5.8, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="summer01", 5.7, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="autumn01", 5.6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="winter01", 5.5, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="total12", 5, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="spring12", 4.8, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="summer12", 4.7, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="autumn12", 4.6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="winter12", 4.5, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="total23", 4, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="spring23", 3.8, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="summer23", 3.7, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="autumn23", 3.6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="winter23", 3.5, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="total34", 3, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="spring34", 2.8, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="summer34", 2.7, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="autumn34", 2.6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="winter34", 2.5, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="total48", 2, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="spring48", 1.8, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="summer48", 1.7, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="autumn48", 1.6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="winter48", 1.5, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="total8", 1, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="spring8", 0.8, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="summer8", 0.7, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="autumn8", 0.6, rawprep$jvar)
rawprep$jvar <- ifelse(rawprep$term=="winter8", 0.5, rawprep$jvar)