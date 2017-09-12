#' Function gearPlotsHH.nodp to plot net opening vs. depth 
#'  
#' Prduces Net Vertical opening vs. Depth plot and a model with nls R function. Data are taken directly from DATRAS getting all the data from DATRAS using function getHHdata from library(icesDatras)
#' it only produces plots for surveys with HH files uploaded in DATRAS
#' If there are different two different sweeps lengths produces a model for each sweep length
#' @param Survey: Surveys available in DATRAS: i.e. SWC-IBTS, ROCKALL, NIGFS, IE-IGFS, SP-PORC, FR-CGFS, EVHOE, SP-NORTH, PT-IBTS and SP-ARSA
#' @param years: years to be downloaded and used, had to be available in DATRAS. The time series will be ploted in grey dots, last year in steelblue2, it depends on the order of years, not the actual chronological year.
#' @param  quarter: the quarter of the survey to be ploted
#' @param  c.int: the confidenc interval to be used in the confint function
#' @return Produces a set of 4 or 2 graphs: DoorSpread vs. WingSpread, WingSpread vs. Depth, DoorSpread vs. Depth, Netopening Opening vs Depth
#, it also includes information on the ship, the time series used (bottom fourth graph), the models and parameters estimated.
#' @examples gearPlotsHH.nodp("SWC-IBTS",c(2014:2016),1,.07,.5)
#' @examples gearPlotsHH.nodp("SWC-IBTS",c(2013:2016),4)
#' @examples gearPlotsHH.nodp("ROCKALL",c(2013:2016),3)
#' @examples gearPlotsHH.nodp("NIGFS",c(2005:2016),1)
#' @examples gearPlotsHH.nodp("NIGFS",c(2006:2007,2009:2016),4)
#' @examples gearPlotsHH.nodp("IE-IGFS",c(2011:2016),4,.8)
#' @examples gearPlotsHH.nodp("SP-PORC",c(2003:2016),3)
#' @examples gearPlotsHH.nodp("FR-CGFS",c(1998:2016),4)
#' @examples gearPlotsHH.nodp("EVHOE",c(1997:2015),4)
#' @examples gearPlotsHH.nodp("SP-NORTH",c(2014:2016),4)
#' @examples gearPlotsHH.nodp("SP-ARSA",c(2014:2016),1)
#' @examples gearPlotsHH.nodp("SP-ARSA",c(2014:2016),4)
#' @export
gearPlotsHH.nodp<-function(Survey,years,quarter,c.inta=.8,c.intb=.3) {
   require(icesDatras)                                         
   dumb<-getHHdata.ts(Survey,years,quarter)
   #   HH.SWCQ1<-subset(HH.SWCQ1,Year>2013)
   #attach(dumb)
   dumb$sweeplngt<-factor(dumb$SweepLngt)
   if (length(subset(dumb$Netopening,dumb$Netopening> c(-9)))>0){
      dpthA<-range(dumb$Depth,na.rm=T)
      vrt<-range(subset(dumb$Netopening,dumb$Netopening> c(-9)))
      plot(Netopening~Depth,dumb,xlim=c(0,dpthA[2]+20),ylim=c(0,vrt[2]+2),pch=21,col="steelblue",
         ylab="Vertical opening (m)",xlab="Depth (m)",subset=Year!=years[length(years)] & Netopening> c(-9))
       if (length(levels(dumb$sweeplngt))==1) {
           dp<-seq(dpthA[1],dpthA[2]+20,length=650)
           Netopening.log<-nls(Netopening~a1+b1*log(Depth),dumb,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))
           points(Netopening~Depth,dumb,subset=Year==years[length(years)],pch=21,bg="steelblue",col="steelblue",lwd=2)   
           title(main=paste0("Vertical opening vs. Depth in ",Survey,".Q",quarter," survey"),line=2.5)
           mtext(dumb$Ship[length(dumb$Ship)],line=.4,cex=.9,adj=0)
           a1<-round(coef(Netopening.log)[1],2)
           b1<-round(coef(Netopening.log)[2],2)
           lines(dp,a1+b1*log(dp),col=1,lwd=2)
           a1low<-confint(Netopening.log,level=c.inta)[1,1]
           b1low<-confint(Netopening.log,level=c.inta)[2,1]
           lines(dp,a1low+b1low*log(dp),col="black",lty=2,lwd=1)
           a1Upr<-confint(Netopening.log,level=c.inta)[1,2]
           b1Upr<-confint(Netopening.log,level=c.inta)[2,2]
           lines(dp,a1Upr+b1Upr*log(dp),col="black",lty=2,lwd=1)
           legend("topright",legend=substitute(NetOpening == a1 + b1 %*% log(depth),list(a1=round(coef(Netopening.log)[1],2),b1=(round(coef(Netopening.log)[2],2)))),bty="n",text.font=2,inset=.05)
           dumbo<-bquote("Netopening opening"== a + b %*% log("Depth"))
           mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
           summary(Netopening.log)
           }
       if (length(levels(dumb$sweeplngt))==2) {
           dumbshort<-subset(dumb,SweepLngt==levels(dumb$sweeplngt)[1])
           dumblong<-subset(dumb,SweepLngt==levels(dumb$sweeplngt)[2])
           dpthAst<-range(dumbshort$Depth,na.rm=T)
           dpthAlg<-range(dumblong$Depth,na.rm=T)
           dpst<-seq(dpthAst[1],dpthAst[2]+20,length=650)
           dplg<-seq(dpthAlg[1],dpthAlg[2]+20,length=650)
           Netopeningst.log<-nls(Netopening~a1+b1*log(Depth),dumbshort,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))
           Netopeninglg.log<-nls(Netopening~a1+b1*log(Depth),dumblong,start=c(a1=.1,b1=1),subset=HaulVal=="V" & Netopening> c(-9))
           vrtst<-range(subset(dumbshort$Netopening,dumbshort$Netopening> c(-9)))
           vrtlg<-range(subset(dumblong$Netopening,dumblong$Netopening> c(-9)))
           points(Netopening~Depth,dumbshort,subset=HaulVal=="V",pch=21,col="steelblue2")   
           points(Netopening~Depth,dumbshort,subset=Year==years[length(years)],pch=21,bg="steelblue2",col="steelblue2",lwd=2)   
           points(Netopening~Depth,dumblong,subset=HaulVal=="V",pch=21,col="steelblue")   
           points(Netopening~Depth,dumblong,subset=Year==years[length(years)],pch=21,bg="steelblue",col="steelblue",lwd=2)   
           title(main=paste0("Vertical opening vs. Depth in ",Survey,".Q",quarter," survey"),line=2.5)
           mtext(dumb$Ship[length(dumb$Ship)],line=.4,cex=.9,adj=0)
           a1st<-round(coef(Netopeningst.log)[1],2)
           b1st<-round(coef(Netopeningst.log)[2],2)
           lines(dpst,a1st+b1st*log(dpst),col="black",lwd=2)
           a1lowst<-confint(Netopeningst.log,level=c.inta)[1,1]
           b1lowst<-confint(Netopeningst.log,level=c.inta)[2,1]
           lines(dpst,a1lowst+b1lowst*log(dpst),col="black",lty=2,lwd=1)
           a1Uprst<-confint(Netopeningst.log,level=c.inta)[1,2]
           b1Uprst<-confint(Netopeningst.log,level=c.inta)[2,2]
           lines(dpst,a1Uprst+b1Uprst*log(dpst),col="black",lty=2,lwd=1)
           legend("topleft",legend=substitute(SortNop == a1st + b1st %*% log(depth),list(a1st=round(coef(Netopeningst.log)[1],2),b1st=(round(coef(Netopeningst.log)[2],2)))),bty="n",text.font=2,inset=.05)
           a1lg<-round(coef(Netopeninglg.log)[1],2)
           b1lg<-round(coef(Netopeninglg.log)[2],2)
           lines(dplg,a1lg+b1lg*log(dplg),col="steelblue",lwd=2)
           a1lowlg<-confint(Netopeninglg.log,level=c.intb)[1,1]
           b1lowlg<-confint(Netopeninglg.log,level=c.intb)[2,1]
           lines(dplg,a1lowlg+b1lowlg*log(dplg),col="steelblue",lty=2,lwd=2)
           a1Uprlg<-confint(Netopeninglg.log,level=c.intb)[1,2]
           b1Uprlg<-confint(Netopeninglg.log,level=c.intb)[2,2]
           lines(dplg,a1Uprlg+b1Uprlg*log(dplg),col="steelblue",lty=2,lwd=2)
           legend("bottomright",legend=substitute(LongNop == a1lg + b1lg %*% log(depth),list(a1lg=round(coef(Netopeninglg.log)[1],2),b1lg=(round(coef(Netopeninglg.log)[2],2)))),bty="n",text.font=2,inset=.1)
           summary(Netopeningst.log)
           summary(Netopeninglg.log)
           }
           dumbo<-bquote("Netopening opening"== a + b %*% log("Depth"))
           mtext(dumbo,line=.4,side=3,cex=.8,font=2,adj=1)
            
         txt<-paste0("Years: ",paste0(c(years[1],"-",years[length(years)]),collapse=" "))
         text(0,0, txt, font=2, cex=1,pos=4)
      }
      }
            