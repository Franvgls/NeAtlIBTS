## ------------------------------------------------------------------------
library(NeAtlIBTS.R)


## ----Warp-Depth, fig.width=10.1,fig.height=5.1,include=TRUE,results='hide'----
par(mfrow=c(1,2))
gearPlotHH.wrpdp("SWC-IBTS",c(2011:2016),1,col1="darkblue")
gearPlotHH.wrpdp("SWC-IBTS",c(2011:2016),1,col1="darkblue",pF=F)

## ----Get data,include=TRUE,results='hide'--------------------------------
ieigfs<-getHHdata.ts(Survey="IE-IGFS",years=c(2011:2016),quarter=4)

## ----DoorSpread, fig.width=10.2,fig.height=5.1,include=TRUE,results='hide',fig.align='left',echo=FALSE,message=FALSE----
par(mfrow=c(1,2))
gearPlotHH.dodp(ieigfs,years=c(2011:2016),quarter=4,getICES = FALSE)
gearPlotHH.dodp(ieigfs,c(2011:2016),4,pF=F,getICES=F)

## ----WingSpread, fig.width=10,fig.height=4.1,fig.align='left',include=TRUE,results='hide',echo=FALSE,message=FALSE----
par(mfrow=c(1,2))
gearPlotHH.wgdp("EVHOE",c(2011:2016),4,.3)
gearPlotHH.wgdp("EVHOE",c(2011:2016),4,.4,pF=F)

## ----NetOpening,echo=FALSE,fig.align='center', fig.width=10, fig.height=4.1,include=TRUE,results='hide',message=FALSE----
par(mfrow=c(1,2))
gearPlotHH.nodp("SP-PORC",c(2011:2016),3,.3)
gearPlotHH.nodp("SP-PORC",c(2011:2016),3,.4,pF=F)

## ----DoorSpread vs WingSpread, fig.width=10, fig.height=4.1,include=TRUE,results='hide'----
par(mfrow=c(1,2))
gearPlotHH.wgdo("SP-ARSA",c(2014:2016),1,col1="darkblue",col2="steelblue2")
gearPlotHH.wgdo("SP-ARSA",c(2014:2016),1,col1="darkblue",col2="steelblue2",pF=F)

