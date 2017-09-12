#' Function gearPlotsHH.wgdp to plot Wing Spread vs. Depth behaviour obtaining plots and models with nls R function
#' 
#' Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are DoorSpread and WingSpread values in HH records produces four graphs, if only DoorSpread values are available produces only two graphs
#' @param Survey: Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @param  c.int: the confidenc interval to be used in the confint function
#' @return Produces a set of 4 or 2 graphs: DoorSpread vs. WingSpread, WingSpread vs. Depth, DoorSpread vs. Depth, Vertical Opening vs Depth
#, it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples gearPlotsHH.wgdp("SWC-IBTS",c(2014:2016),1,.2)
#' @examples gearPlotsHH.wgdp("SWC-IBTS",c(2013:2016),4,.2)
#' @examples gearPlotsHH.wgdp("ROCKALL",c(2013:2016),3,.8)
#' @examples gearPlotsHH.wgdp("NIGFS",c(2005:2016),1,.2)
#' @examples gearPlotsHH.wgdp("NIGFS",c(2006:2007,2009:2016),4,.8)
#' @examples gearPlotsHH.wgdp("IE-IGFS",c(2005:2016),4,.8)
#' @examples gearPlotsHH.wgdp("SP-PORC",c(2003:2016),3,.5)
#' @examples gearPlotsHH.wgdp("FR-CGFS",c(1998:2016),4,.8)
#' @examples gearPlotsHH.wgdp("EVHOE",c(1997:2015),4,.9)
#' @examples gearPlotsHH.wgdp("SP-NORTH",c(2014:2016),4,.3)
#' @examples gearPlotsHH.wgdp("SP-ARSA",c(2014:2016),1.2)
#' @examples gearPlotsHH.wgdp("SP-ARSA",c(2014:2016),4,.2)
#' @export
gearPlotsHH.wgdp<-function(Survey,years,quarter,c.int=.8) {
   require(icesDatras)                                         
   dumb<-getHHdata.ts(Survey,years,quarter)
   #   HH.SWCQ1<-subset(HH.SWCQ1,Year>2013)
   #attach(dumb)
   dumb$sweeplngt<-factor(dumb$SweepLngt)
   if (length(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))>0){
         wspr<-range(subset(dumb$WingSpread,dumb$WingSpread> c(-9)))
         dspr<-range(subset(dumb$DoorSpread,dumb$DoorSpread>c(-9)))
         dpthA<-range(dumb$Depth,na.rm=T)
         plot(WingSpread~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,wspr[2]+10),subset=WingSpread!=c(-9) & Year!=years[length(years)],pch=21,col=grey(.5),ylab="Wing spread (m)",xlab="Depth (m)")
         if (length(levels(dumb$sweeplngt))==1) {
            dpthA<-range(dumb$Depth,na.rm=T)
            dp<-seq(dpthA[1],dpthA[2]+20,length=650)
            WingSpread.log<-nls(WingSpread~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
            points(WingSpread~Depth,dumb,subset=Year==years[length(years)],pch=21,bg="steelblue2")
            title(paste0("Wing Spread vs. Depth in ",Survey[1],".Q",quarter," survey"),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            a1<-round(coef(WingSpread.log)[1],2)
            b1<-round(coef(WingSpread.log)[2],2)
            lines(dp,a1+b1*log(dp),col=1,lwd=2)
            a1low<-confint(WingSpread.log,level=c.int)[1,1]
            b1low<-confint(WingSpread.log,level=c.int)[2,1]
            lines(dp,a1low+b1low*log(dp),col="steelblue",lty=2,lwd=1)
            a1Upr<-confint(WingSpread.log,level=c.int)[1,2]
            b1Upr<-confint(WingSpread.log,level=c.int)[2,2]
            lines(dp,a1Upr+b1Upr*log(dp),col="steelblue",lty=2,lwd=1)
            legend("bottomright",legend=substitute(WS == a1 + b1 %*% log(depth),list(a1=round(coef(WingSpread.log)[1],2),b1=(round(coef(WingSpread.log)[2],2)))),bty="n",text.font=2,inset=.2)
            dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            print(summary(WingSpread.log))
         }
         if (length(levels(dumb$sweeplngt))==2) {
            dumbshort<-subset(dumb,SweepLngt==levels(dumb$sweeplngt)[1])
            dumblong<-subset(dumb,SweepLngt==levels(dumb$sweeplngt)[2])
            dpthAst<-range(dumbshort$Depth,na.rm=T)
            dpthAlg<-range(dumblong$Depth,na.rm=T)
            dpst<-seq(dpthAst[1],dpthAst[2]+20,length=650)
            dplg<-seq(dpthAlg[1],dpthAlg[2]+20,length=650)
            WingSpreadst.log<-nls(WingSpread~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
            WingSpreadlg.log<-nls(WingSpread~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=WingSpread!=c(-9))
            points(WingSpread~Depth,dumbshort,subset=HaulVal=="V",pch=21,col="steelblue2")
            points(WingSpread~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg="steelblue2")
            points(WingSpread~Depth,dumblong,subset=HaulVal=="V",pch=21,col="steelblue")
            points(WingSpread~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg="steelblue")
            title(paste0("Wing Spread vs. Depth in ",dumb$Survey[1],".Q",quarter," survey"),line=2.5)
            mtext(dumb$Ship[1],line=.4,cex=.8,adj=0)
            a1st<-round(coef(WingSpreadst.log)[1],2)
            b1st<-round(coef(WingSpreadst.log)[2],2)
            lines(dpst,a1st+b1st*log(dpst),col="steelblue2",lwd=2)
            a1lowst<-confint(WingSpreadst.log,level=c.int)[1,1]
            b1lowst<-confint(WingSpreadst.log,level=c.int)[2,1]
            lines(dpst,a1lowst+b1lowst*log(dpst),col="steelblue2",lty=2,lwd=2)
            a1Uprst<-confint(WingSpreadst.log,level=c.int)[1,2]
            b1Uprst<-confint(WingSpreadst.log,level=c.int)[2,2]
            lines(dpst,a1Uprst+b1Uprst*log(dpst),col="steelblue2",lty=2,lwd=2)
            legend("bottomleft",legend=substitute(WSshort == a1st + b1st %*% log(depth),list(a1st=round(coef(WingSpreadst.log)[1],2),b1st=(round(coef(WingSpreadst.log)[2],2)))),bty="n",text.font=2,inset=.2)
            a1lg<-round(coef(WingSpreadlg.log)[1],2)
            b1lg<-round(coef(WingSpreadlg.log)[2],2)
            lines(dplg,a1lg+b1lg*log(dplg),col="steelblue",lwd=2)
            a1lowlg<-confint(WingSpreadlg.log,level=c.int)[1,1]
            b1lowlg<-confint(WingSpreadlg.log,level=c.int)[2,1]
            lines(dplg,a1lowlg+b1lowlg*log(dplg),col="steelblue",lty=2,lwd=2)
            a1Uprlg<-confint(WingSpreadlg.log,level=c.int)[1,2]
            b1Uprlg<-confint(WingSpreadlg.log,level=c.int)[2,2]
            lines(dplg,a1Uprlg+b1Uprlg*log(dplg),col="steelblue",lty=2,lwd=2)
            legend("topright",legend=substitute(WSlong == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(WingSpreadlg.log)[1],2),b1lg=(round(coef(WingSpreadlg.log)[2],2)))),bty="n",text.font=2,inset=.2)
            dumbo<-bquote("Wing Spread"== a + b %*% log("Depth"))
            mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            print(summary(WingSpreadst.log))
            print(summary(WingSpreadlg.log))
         txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
         text(0,0, txt, font=2, cex=1,pos=4)
         }
      }
      }