## Started 8 October 2019 ##
## By Cat - based off chillfreeze brms plots ##

# with COLORS for each incr #

muplotfx <- function(modelhere, nameforfig, width, height, ylim, xlim, leg1, leg2){
  seasnum <- unique(df$season)
  svg(file.path(figpath, paste("", nameforfig, figpathmore, ".svg", sep="")),
      width = width, height = height)
  par(xpd=FALSE)
  par(mar=c(5,7,3,10))
  plot(x=NULL,y=NULL, xlim=xlim, yaxt='n', ylim=ylim,
       xlab=xlab, ylab="", main=nameforfig)
  axis(2, at=1:6, labels=rev(c("0-1", "1-2", "2-3", "3-4", "4-8", "8-pith")), las=1)
  abline(v=0, lty=2, col="darkgrey")
  rownameshere <- c("b_Intercept", "b_increment1M2", "b_increment2M3", "b_increment3M4", "b_increment4M8",
                    "b_increment8Mpith")
  ppeffects <- c("b_Intercept", "b_increment1M2", "b_increment2M3", "b_increment3M4", "b_increment4M8",
                 "b_increment8Mpith")
  for(i in 1:6){#i=3
    pos.y<-(6:1)[i]
    if(i!=1){
    pos.x<-modoutput[(modoutput$term==rownameshere[i]),"estimate"] + 
      modoutput[(modoutput$term=="b_Intercept"), "estimate"]
    lines(modoutput[(modoutput$term==rownameshere[i]),c("lower","upper")]+
                   modoutput[(modoutput$term=="b_Intercept"),c("lower","upper")],rep(pos.y,2),col="black")
    } else{
      pos.x<- modoutput[(modoutput$term==rownameshere[i]),"estimate"]
      lines(modoutput[(modoutput$term==rownameshere[i]),c("lower","upper")],rep(pos.y,2),col="black")
    }
    points(pos.x,pos.y,cex=1.5,pch=19,col="black")
    for(seassi in 1:length(seasnum)){#incrsi=4
      pos.sps.i<-which(grepl(paste("[",seassi,"]",sep=""),mod.ranef$parameter,fixed=TRUE))
      jitt<-runif(1,0.05,0.4)
      pos.y.sps.i<-pos.y-jitt
      pos.x.sps.i<-mod.ranef[pos.sps.i[i],"mean"]
      lines(mod.ranef[pos.sps.i[i],c("25%","75%")],rep(pos.y.sps.i,2),
            col=alpha(my.pal[seassi], alphahere))
      points(pos.x.sps.i,pos.y.sps.i,cex=0.8, col=alpha(my.pal[seassi], alphahere), pch=my.pch[seassi])
      
      
    }
  }
  par(xpd=TRUE) # so I can plot legend outside
  legend(leg1, leg2, #sort(unique(gsub("_", " ", df$season)))
         col=alpha(my.pal[1:length(seasnum)], alphahere),
         cex=1, bty="n", pch=my.pch[1:length(seasnum)],
         legend=c("Spring", "Summer", "Autumn", "Winter"))
  dev.off()
  
}
