#' Function gearPlotsHH to plot gear parameters and behaviour obtaining plots and models with nls R function
#' Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are DoorSpread and WingSpread values in HH records produces four graphs, if only DoorSpread values are available produces only two graphs
#' @param Survey: Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in yellow, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @param  c.int: the confidenc interval to be used in the confint function
#' @return Produces a set of 4 or 2 graphs: DoorSpread vs. WingSpread, WingSpread vs. Depth, DoorSpread vs. Depth, Vertical Opening vs Depth
#, it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples gearPlotsHH.3("SWC-IBTS",c(2014:2016),1,.8)
#' @examples gearPlotsHH.3("SWC-IBTS",c(2013:2016),4)
#' @examples gearPlotsHH.3("ROCKALL",c(2013:2016),3)
#' @examples gearPlotsHH.3("NIGFS",c(2005:2016),1)
#' @examples gearPlotsHH.3("NIGFS",c(2006:2007,2009:2016),4)
#' @examples gearPlotsHH.3("IE-IGFS",c(2011:2016),4)
#' @examples gearPlotsHH.3("SP-PORC",c(2003:2016),3)
#' @examples gearPlotsHH.3("FR-CGFS",c(1998:2016),4)
#' @examples gearPlotsHH.3("EVHOE",c(1997:2015),4)
#' @examples gearPlotsHH.3("SP-NORTH",c(2014:2016),4)
#' @examples gearPlotsHH.3("SP-ARSA",c(2014:2016),1)
#' @examples gearPlotsHH.3("SP-ARSA",c(2014:2016),4)
#' @export
gearPlotsHH.3<-function(Survey,years,quarter,c.int=.8) {
   require(icesDatras)                                         
   dumb<-getHHdata.ts(Survey,years,quarter)
   #   HH.SWCQ1<-subset(HH.SWCQ1,Year>2013)
   #attach(dumb)
   dumb$sweeplngt<-factor(dumb$SweepLngt)
   if (length(subset(dumb$DoorSpread,dumb$DoorSpread> c(-9)))>0){
      dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(-9)))
      dpthA<-range(dumb$Depth,na.rm=T)
      dp<-seq(dpthA[1],dpthA[2]+20,length=650)
      plot(DoorSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,dspr[2]+20),pch=21,col=grey(.5),ylab="Door spread (m)",xlab="Depth (m)",subset=DoorSpread!=c(-9)& Year!=years[length(years)])
      title(main=paste0("Door Spread vs. Depth in ",Survey[1],".Q",quarter," survey"),line=2.5)
      if (length(levels(dumb$sweeplngt))==1) {
         DoorSpread.log<-nls(DoorSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
         dspr<-range(subset(DoorSpread,DoorSpread>c(-9)))
         points(DoorSpread~Depth,dumb,subset=Year==years[length(years)],pch=21,bg="yellow")
         mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
         a1<-round(coef(DoorSpread.log)[1],2)
         b1<-round(coef(DoorSpread.log)[2],2)
         lines(dp,a1+b1*log(dp),col=1,lwd=2)
         a1low<-confint(DoorSpread.log,level=c.int)[1,1]
         b1low<-confint(DoorSpread.log,level=c.int)[2,1]
         lines(dp,a1low+b1low*log(dp),col=2,lty=2,lwd=2)
         a1Upr<-confint(DoorSpread.log,level=c.int)[1,2]
         b1Upr<-confint(DoorSpread.log,level=c.int)[2,2]
         lines(dp,a1Upr+b1Upr*log(dp),col=2,lty=2,lwd=2)
         legend("bottomright",legend=substitute(DS == a1 + b1 %*% log(depth),list(a1=round(coef(DoorSpread.log)[1],2),b1=(round(coef(DoorSpread.log)[2],2)))),bty="n",text.font=2,inset=.2)
#         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
         dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
         mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         summary(DoorSpread.log)
         }
         if (length(levels(dumb$sweeplngt))==2) {
            dumbshort<-subset(dumb,SweepLngt==levels(dumb$sweeplngt)[1])
            dumblong<-subset(dumb,SweepLngt==levels(dumb$sweeplngt)[2])
            DoorSpreadst.log<-nls(DoorSpread~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
            DoorSpreadlg.log<-nls(DoorSpread~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=HaulVal=="V" & DoorSpread> c(-9))
            dspr<-range(subset(dumbshort$DoorSpread,dumbshort$DoorSpread>c(-9)))
#            plot(DoorSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,dspr[2]+20),pch=21,col=grey(.5),ylab="Door spread (m)",xlab="Depth (m)",subset=DoorSpread!=c(-9)& Year!=years[length(years)])
            points(DoorSpread~Depth,dumbshort,subset=HaulVal=="V",pch=21,col="yellow")
            points(DoorSpread~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg="yellow")
            title(main=paste0("Door Spread vs. Depth in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            a1st<-round(coef(DoorSpreadst.log)[1],2)
            b1st<-round(coef(DoorSpreadst.log)[2],2)
            lines(dpst,a1st+b1st*log(dpst),col="orange",lwd=2)
            a1lowst<-confint(DoorSpreadst.log,level=c.int)[1,1]
            b1lowst<-confint(DoorSpreadst.log,level=c.int)[2,1]
            lines(dpst,a1lowst+b1lowst*log(dpst),col="orange",lty=2,lwd=2)
            a1Uprst<-confint(DoorSpreadst.log,level=c.int)[1,2]
            b1Uprst<-confint(DoorSpreadst.log,level=c.int)[2,2]
            lines(dpst,a1Uprst+b1Uprst*log(dpst),col=2,lty=2,lwd=2)
            legend("bottomleft",legend=substitute(DSshort == a1st + b1st %*% log(depth),list(a1st=round(coef(DoorSpreadst.log)[1],2),b1st=(round(coef(DoorSpreadst.log)[2],2)))),bty="n",text.font=2,inset=.1)
            points(DoorSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,col="red")
            points(DoorSpread~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg="red")
            a1lg<-round(coef(DoorSpreadlg.log)[1],2)
            b1lg<-round(coef(DoorSpreadlg.log)[2],2)
            lines(dplg,a1lg+b1lg*log(dplg),col="red",lwd=2)
            a1lowlg<-confint(DoorSpreadlg.log,level=c.int)[1,1]
            b1lowlg<-confint(DoorSpreadlg.log,level=c.int)[2,1]
            lines(dplg,a1lowlg+b1lowlg*log(dplg),col="red",lty=2,lwd=2)
            a1Uprlg<-confint(DoorSpreadlg.log,level=c.int)[1,2]
            b1Uprlg<-confint(DoorSpreadlg.log,level=c.int)[2,2]
            lines(dplg,a1Uprlg+b1Uprlg*log(dplg),col="red",lty=2,lwd=2)
            legend("right",legend=substitute(DSlong == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(DoorSpreadlg.log)[1],2),b1lg=(round(coef(DoorSpreadlg.log)[2],2)))),bty="n",text.font=2,inset=.2)
#         text("bottomleft",paste0(c(years[1],"-",years[length(years)])),inset=c(0,.1))
         dumbo<-bquote("Door Spread"== a + b %*% log("Depth"))
         mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
         summary(DoorSpreadst.log)
         summary(DoorSpreadlg.log)
         }
         }
         }