#' gearPlotHH.wgdr Door spread vs. Wingspread
#' Plots Door Spread vs. Wing Spread behaviour if there are DoorSpread and WingSpread values 
#' @param Survey: Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @param  c.int: the confidenc interval to be used in the confint function
#' @return Produces a set of 4 or 2 graphs: DoorSpread vs. WingSpread, WingSpread vs. Depth, DoorSpread vs. Depth, Vertical Opening vs Depth
#, it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples gearPlotHH.wgdo("SWC-IBTS",c(2014:2016),1)
#' @examples gearPlotHH.wgdo("SWC-IBTS",c(2013:2016),4)
#' @examples gearPlotHH.wgdo("ROCKALL",c(2013:2016),3)
#' @examples gearPlotHH.wgdo("NIGFS",c(2005:2016),1)
#' @examples gearPlotHH.wgdo("NIGFS",c(2006:2007,2009:2016),4)
#' @examples gearPlotHH.wgdo("IE-IGFS",c(2011:2016),4)
#' @examples gearPlotHH.wgdo("SP-PORC",c(2003:2016),3)
#' @examples gearPlotHH.wgdo("FR-CGFS",c(1998:2016),4)
#' @examples gearPlotHH.wgdo("EVHOE",c(1997:2015),4)
#' @examples gearPlotHH.wgdo("SP-NORTH",c(2014:2016),4)
#' @examples gearPlotHH.wgdo("SP-ARSA",c(2014:2016),1)
#' @examples gearPlotHH.wgdo("SP-ARSA",c(2014:2016),4)
#' @export
gearPlotHH.wgdo<-function(Survey,years,quarter,c.int=.8) {
   require(icesDatras)                                         
   dumb<-getHHdata.ts(Survey,years,quarter)
   #   HH.SWCQ1<-subset(HH.SWCQ1,Year>2013)
   #attach(dumb)
   dumb$sweeplngt<-factor(dumb$SweepLngt)
   if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))==0) {stop("No records with WingSpread>0")}
  if(length(subset(dumb,DoorSpread>c(-9)))>0){
         wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))
         dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(-9)))
         if (length(levels(dumb$sweeplngt))==1) {
            lm.WingVsDoor<-lm(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9))
            outlierTest(lm.WingVsDoor,data=dumb)
            plot(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(dspr[1]-20,dspr[2]+20),ylim=c(wspr[1]-10,wspr[2]+10),xlab="Door Spread (m)",ylab="Wing Spread (m)",pch=21,col="grey")
         points(WingSpread~DoorSpread,dumb,subset=c(HaulVal=="V" & Year==years[length(years)]),pch=21,bg="steelblue2")
         title(main=paste0("Door spread vs. Wing spread in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
         mtext(dumb$Ship[1],line=.4,cex=.7,adj=0)
         abline(lm.WingVsDoor,col=2,lty=2)
         legend("bottomright",legend=substitute(paste(WS == a + b %*% DS),list(a=round(coef(lm.WingVsDoor)[1],2),b=(round(coef(lm.WingVsDoor)[2],2)))),bty="n",text.font=2,inset=.2)
         legend("bottomright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor)$adj.r.squared,2))),inset=c(.25,.15),cex=.9,bty="n")
         dumbo<-bquote("WS"== a + b %*% DS)
         mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         }
         if (length(levels(dumb$sweeplngt))==2) {
         lm.WingVsDoor.short<-lm(WingSpread~DoorSpread,dumb,
            subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & sweeplngt==levels(sweeplngt)[1])
         lm.WingVsDoor.long<-lm(WingSpread~DoorSpread,dumb,
            subset=HaulVal=="V" & WingSpread > c(-9) & DoorSpread> c(-9) & sweeplngt==levels(sweeplngt)[2])
         plot(WingSpread~DoorSpread,dumb,subset=HaulVal=="V" & Year!=years[length(years)],xlim=c(dspr[1]-20,dspr[2]+20),ylim=c(wspr[1]-10,wspr[2]+10),xlab="Door Spread (m)",ylab="Wing Spread (m)",pch=21,col="grey")
         points(WingSpread~DoorSpread,dumb,
            subset=c(HaulVal=="V" & sweeplngt==levels(sweeplngt)[1]),pch=21,col="steelblue2")
         points(WingSpread~DoorSpread,dumb,
            subset=c(HaulVal=="V" & sweeplngt==levels(sweeplngt)[2]),pch=21,col="steelblue")
         points(WingSpread~DoorSpread,dumb,
            subset=c(HaulVal=="V" & Year==years[length(years)]& sweeplngt==levels(sweeplngt)[1]),pch=21,bg="steelblue2")
         points(WingSpread~DoorSpread,dumb,
            subset=c(HaulVal=="V" & Year==years[length(years)] & sweeplngt==levels(sweeplngt)[2]),pch=21,bg="steelblue")
         abline(lm.WingVsDoor.short,col="steelblue2",lty=2,lwd=2)
         abline(lm.WingVsDoor.long,col="steelblue",lty=2,lwd=2)
         legend("bottomright",legend=substitute(paste(WSshort == a + b %*% DSshort),list(a=round(coef(lm.WingVsDoor.short)[1],2),b=(round(coef(lm.WingVsDoor.short)[2],2)))),bty="n",text.font=2,inset=.1)
         legend("bottomright",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor.short)$adj.r.squared,2))),inset=c(.15,.05),cex=.9,bty="n")
         legend("topleft",legend=substitute(paste(WSlong == a + b %*% DSlong),list(a=round(coef(lm.WingVsDoor.long)[1],2),b=(round(coef(lm.WingVsDoor.long)[2],2)))),bty="n",text.font=2,inset=.1)
         legend("topleft",legend=substitute(paste(r^2 ==resq),list(resq=round(summary(lm.WingVsDoor.long)$adj.r.squared,2))),inset=c(.18,.15),cex=.9,bty="n")
         }
         } else {stop("No records with DoorSpread>0")}
         }