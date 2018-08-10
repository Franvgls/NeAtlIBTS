#' Function IBTSNeAtl_map plots the map with all the surveys **STILL ON DEVELOPMENT**
#' 
#' Produces a map from the shapefiles that define the IBTSNeAtl Surveys from Scotland to Cádiz, Still needs to include the shapefiles within the package, or right references.
#' @param nl = 61.5 northernmost limit of the map
#' @param sl = 35 Southernmost limit of the map
#' @param leg = TRUE if TRUE includes the legend with the colors of the surveys
#' @param cex.leg = .7 Size of the legend
#' @param dens = 30 density of the shading lines for all the surveys
#' @param ICESdiv = TRUE if TRUE plots the IBTS divisions behind the shapefiles
#' @param ICESrect = FALSE if TRUE plots the lines of the ICES statistic rectangles
#' @param bathy = TRUE if TRUE plots the isobaths under the behind the shapefiles
#' @param out = format of the output, can be "def" default device, "pdf", "tiff" or "png"
#' @param nfile = name for the output file 
#' @param shpdir = path to the folder with the shapefiles
#' @param load = T or F to load all the shapes files.
#' @examples IBTSNeAtl_map(out="def",dens=0,nl=45,leg=F,ICESrect = T);text(stat_y~stat_x,Area,labels=ICESNAME,cex=.8,font=4);text(stat_y~stat_x,Area,labels=Area,cex=.6,pos=1,col=2) 
#' @export
IBTSNeAtl_map<-function(nl=60.5,sl=36,xlims=c(-18,3),leg=TRUE,cex.leg=.7,dens=30,ICESdiv=TRUE,ICESrect=FALSE,bathy=TRUE,out="def",nfile="NeAtlIBTS_map",lwdl=.1,shpdir="c:/GitHubRs/shapes/") {
  library(mapdata)
  library(maptools)
  library(maps)
  library(rgdal)
  largo=(nl-sl)*10
  if (xlims[2] < 0) {
    ancho<- diff(rev(abs(xlims)))*10
  } else ancho<- diff(xlims)*10
  ices.div<-readOGR(paste0(shpdir,"ices_div.dbf"),"ices_div",verbose = F)
  bath100<-readOGR(paste0(shpdir,"100m.dbf"),"100m",verbose = F)
  bathy.geb<-readOGR(paste0(shpdir,"bathy_geb.dbf"),"bathy_geb",verbose = F)
  SWC_Q1<-readOGR(paste0(shpdir,"SWC_Q1.dbf"),"SWC_Q1",verbose = F)
  SWC_Q1_w84<-spTransform(SWC_Q1,CRS("+proj=longlat +datum=WGS84"))
  SWC_Q3<-readOGR(paste0(shpdir,"SWC_Q3.dbf"),"SWC_Q3",verbose = F)
  SWC_Q3_w84<-spTransform(SWC_Q3,CRS("+proj=longlat +datum=WGS84"))
  IGFS<-readOGR(paste0(shpdir,"IGFS.dbf"),"IGFS",verbose = F)
  IGFS_w84<-spTransform(IGFS,CRS("+proj=longlat +datum=WGS84")) 
  NIGFS<-readOGR(paste0(shpdir,"NI_IBTS.dbf"),"NI_IBTS",verbose = F)
  NIGFS_w84<-spTransform(NIGFS,CRS("+proj=longlat +datum=WGS84")) 
  CGFS<-readOGR(paste0(shpdir,"CGFS_stratum.dbf"),"CGFS_stratum",verbose = F)
  Porc<-readOGR(paste0(shpdir,"Porcupine.dbf"),"Porcupine",verbose = F)
  Porc_w84<-spTransform(Porc,CRS("+proj=longlat +datum=WGS84")) 
  EVHOE<-readOGR(paste0(shpdir,"EVHOE.dbf"),"EVHOE",verbose = F)
  EVHOE_w84<-spTransform(EVHOE,CRS("+proj=longlat +datum=WGS84"))
  Sp_North_w84<-rgdal::readOGR(paste0(shpdir,"Sp_North.WGS84.dbf"),verbose = F) #"Sp_North",
  #Sp_North_w84<-spTransform(Sp_North,CRS("+proj=longlat +datum=WGS84")) 
  #Sp_North<-sf::st_read(paste0(shpdir,"Sp_North.dbf"))
  Sp_Cadiz<-readOGR(paste0(shpdir,"Sp_Cadiz.dbf"),verbose = F) #"Sp_Cadiz"
  Sp_Cadiz_w84<-spTransform(Sp_Cadiz,CRS("+proj=longlat +datum=WGS84"))
  PT_IBTS<-readOGR(paste0(shpdir,"PT_IBTS_2015.dbf"),"PT_IBTS_2015",verbose = F)
  switch(out,
         "pdf" = pdf(file = paste0(nfile,".pdf")),
         "tiff" = tiff(filename=paste0(nfile,".tiff"),width=660*ancho/largo,height=800*largo/ancho),
         "png" = png(filename=paste0(nfile,".png"),bg="transparent",type="cairo",width=round(800*ancho/largo),height=round(800*largo/ancho)))
  par(mar=c(3.5,2,2,2)+0.1)
  #  windows()
  map(database = "worldHires", xlim = xlims, ylim = c(sl,nl),type="n")
  if (bathy) {
    plot(bath100,add=T,col=gray(.85),lwd=.1)
    plot(bathy.geb[bathy.geb$DEPTH!=100,],add=T,col=gray(.85),lwd=.1)
  }
  grid(col=gray(.8),lwd=.5)
  if (ICESdiv) plot(ices.div,add=T,col=NA,border="burlywood")
  if (xlims[2] > 0) {
    degs = seq(xlims[1],-1,ifelse(abs(diff(xlims))>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    degs = seq(3,xlims[2],ifelse(abs(diff(xlims))>1,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    degs = c(0)
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
  } else {
    degs = seq(xlims[1],xlims[2],ifelse(ancho>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
  }
  degs = seq(sl,nl,ifelse(abs(diff(c(sl,nl)))>10,5,2))
  alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
  axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  if (ICESrect) {
    abline(h=seq(30,65,by=.5),col=gray(.3),lwd=.2)
    abline(v=seq(-44,68),col=gray(.3),lwd=.2)
  }
  rug(seq(c(sl+.5),c(nl+.5),by=1),.005,side=2,lwd=lwdl,quiet=TRUE)
  rug(seq(c(xlims[1]+.5),c(xlims[2]+.5),by=1),.005,side=1,lwd=lwdl,quiet=TRUE)
  rug(seq(c(xlims[1]+.5),c(xlims[2]+.5),by=1),.005,side=3,lwd=lwdl,quiet=TRUE)
  rug(seq(c(sl+.5),c(nl+.5),by=1),.005,side=4,lwd=lwdl,quiet=TRUE)
  map(SWC_Q1_w84,SWC_Q1_w84$Name,add=T,col=gray(.4),lwd=.1,fill=T,dens=0,angle=0)
  map(SWC_Q3_w84,SWC_Q3_w84$Name,add=T,col=gray(.4),lwd=.1,fill=T,dens=0,angle=45)
  map(NIGFS_w84,NIGFS_w84$NAME,add=T,col=gray(.4),lwd=.1,fill=T,dens=0,angle=90)
  plot(SWC_Q1_w84,add=T,col=gray(.4),lwd=.01,dens=dens,angle=0)
  plot(SWC_Q3_w84,add=T,col=2,lwd=.01,dens=dens,angle=45)
  plot(NIGFS_w84,add=T,col=3,lwd=.01,dens=dens,angle=45)
  plot(IGFS_w84,add=T,col=4,lwd=.01,dens=dens,angle=135)
  plot(Porc_w84,add=T,col=5,lwd=.01,dens=dens,angle=180)
  plot(CGFS,add=T,col=6,lwd=.1,dens=dens,angle=225)
  plot(EVHOE_w84,add=T,col=gray(.4),lwd=.1,dens=dens,angle=270)
  plot(Sp_North_w84,add=T,col=2,lwd=.1,dens=dens,angle=315)
  plot(PT_IBTS,add=T,col=3,lwd=.1,dens=dens,angle=0)
  plot(Sp_Cadiz_w84,add=T,col=4,lwd=.1,dens=dens,angle=45)
  map(database = "worldHires",xlim = xlims, ylim = c(sl,nl),fill=T,col="gray",add=T,bg="blue")
  box()
  if (leg) legend("bottomleft",c("UK-SCOSWCGFS","UK-SCOROC","UK-NIGFS","IE-IGFS","SP-PORC","FR-CGFS",
                                 "FR-EVHOE","SP-NORTH","PT-PGFS","SP-GCGFS"),fill=c(gray(.4),2:6),
                  cex=cex.leg,inset=c(.03,.2),title="Surveys",bg="white",text.col="black",
                  dens=dens,angle=seq(0,315,by=45))
  if (out!="def") dev.off()
}

