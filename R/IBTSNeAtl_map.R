#' Function IBTSNeAtl_map plots the map with all the surveys **STILL ON DEVELOPMENT**
#' 
#' Produces a map from the shapefiles that define the IBTSNeAtl Surveys from Scotland to Cádiz, Still needs to include the shapefiles within the package, or right references.
#' @param nl = 61.5 northernmost limit of the map
#' @param sl = 35 Southernmost limit of the map
#' @param xlims = longitudinal limits of the map by default 18W to 3E (-18,3)
#' @param leg = TRUE if TRUE includes the legend with the colors of the surveys
#' @param cex.leg = .7 Size of the legend
#' @param dens = 30 density of the shading lines for all the surveys
#' @param lwdl = .1 width of the rugs on the axes of the maps
#' @param ICESdiv = TRUE if TRUE plots the IBTS divisions behind the shapefiles
#' @param bathy = TRUE if TRUE plots the isobaths under the behind the shapefiles
#' @param out = format of the output, can be "def" default device, "pdf", "tiff" or "png"
#' @param nfile = name for the output file 
#' @param shpdir = path to the folder with the shapefiles
#' @examples IBTSNeAtl_map(out="def",leg=T,dens=100)
#' @examples IBTSNeAtl_map(out="def",leg=F,dens=0)
#' @export
IBTSNeAtl_map<-function(nl=60.5,sl=36,xlims=c(-18,3),leg=TRUE,cex.leg=.7,dens=30,lwdl=.1,ICESdiv=TRUE,bathy=TRUE,out="tiff",nfile="NeAtlIBTS_map",shpdir="c:/users/francisco.velasco.ST/documents/fvg/campañas/IBTS/shapes/") {
  library(mapdata)
  library(maptools)
  library(maps)
  library(rgdal)
  largo=(nl-sl)*10
  if (xlims[2] < 0) {
    ancho<- diff(rev(abs(xlims)))*10
      } else ancho<- diff(xlims)*10
  if (ICESdiv & !exists("ices.div")) ices.div<-rgdal::readOGR(paste0(shpdir,"ices_div.dbf"),"ices_div",verbose=F)
  if (bathy & !exists("bath100")) {
    bath100<-rgdal::readOGR(paste0(shpdir,"100m.dbf"),"100m",verbose=F)
    bathy.geb<-rgdal::readOGR(paste0(shpdir,"bathy_geb.dbf"),"bathy_geb",verbose=F)
  }
  if (!exists("SWC_Q1")) {
    SWC_Q1<-rgdal::readOGR(paste0(shpdir,"SWC_Q1.dbf"),"SWC_Q1",verbose=F)
    SWC_Q1_w84<-rgdal::spTransform(SWC_Q1,rgdal::CRS("+proj=longlat +datum=WGS84"),verbose=F)}
  if (!exists("SWC_Q3")) {
    SWC_Q3<-rgdal::readOGR(paste0(shpdir,"SWC_Q3.dbf"),"SWC_Q3",verbose=F)
    SWC_Q3_w84<-rgdal::spTransform(SWC_Q3,rgdal::CRS("+proj=longlat +datum=WGS84"),verbose=F)}
  if (!exists("IGFS")) {
    IGFS<-rgdal::readOGR(paste0(shpdir,"IGFS.dbf"),"IGFS",verbose=F)
    IGFS_w84<-rgdal::spTransform(IGFS,CRS("+proj=longlat +datum=WGS84")) }
  if (!exists("NIGFS")) {
    NIGFS<-rgdal::readOGR(paste0(shpdir,"NI_IBTS.dbf"),"NI_IBTS",verbose=F)
    NIGFS_w84<-rgdal::spTransform(NIGFS,CRS("+proj=longlat +datum=WGS84")) }
  if (!exists("CGFS")) CGFS<-rgdal::readOGR(paste0(shpdir,"CGFS_stratum.dbf"),"CGFS_stratum",verbose=F)
  if (!exists("Porc")) {
    Porc<-rgdal::readOGR(paste0(shpdir,"Porcupine.dbf"),"Porcupine",verbose=F)
    Porc_w84<-rgdal::spTransform(Porc,CRS("+proj=longlat +datum=WGS84")) 
  }
  if (!exists("EVHOE")) {
    EVHOE<-rgdal::readOGR(paste0(shpdir,"EVHOE.dbf"),"EVHOE",verbose=F)
    EVHOE_w84<-rgdal::spTransform(EVHOE,CRS("+proj=longlat +datum=WGS84"))
    }
  if (!exists("Sp_North")) {
    Sp_North<-rgdal::readOGR(paste0(shpdir,"Sp_North.shp"),verbose=F)
    Sp_North_w84<-rgdal::spTransform(Sp_North,CRS("+proj=longlat +datum=WGS84")) 
    }
  if (!exists("Sp_Cadiz")) {
    Sp_Cadiz<-rgdal::readOGR(paste0(shpdir,"Sp_Cadiz.shp"),verbose=F)
    Sp_Cadiz_w84<-rgdal::spTransform(Sp_Cadiz,CRS("+proj=longlat +datum=WGS84"))
    }
  if (!exists("PT_IBTS")) PT_IBTS<-rgdal::readOGR(paste0(shpdir,"PT_IBTS_2015.dbf"),"PT_IBTS_2015",verbose=F)
  switch(out,
    "pdf" = grDevices::pdf(file = paste0(nfile,".pdf")),
    "tiff" = grDevices::tiff(filename=paste0(nfile,".tiff"),width=660*ancho/largo,height=800*largo/ancho),
     "png" = grDevices::png(filename=paste0(nfile,".png"),bg="transparent",type="cairo",width=round(800*ancho/largo),height=round(800*largo/ancho)))
  graphics::par(mar=c(3.5,2,2,2)+0.1)
#  windows()
  maps::map(database = "worldHires", xlim = xlims, ylim = c(sl,nl),type="n")
  if (bathy) {
    graphics::plot(bath100,add=T,col=grDevices::gray(.85),lwd=.1)
    graphics::plot(bathy.geb[bathy.geb$DEPTH!=100,],add=T,col=grDevices::gray(.85),lwd=.1)
    }
  graphics::grid(col=grDevices::gray(.8),lwd=.5)
  if (ICESdiv) graphics::plot(ices.div,add=T,col=NA,border="burlywood")
  if (xlims[2] > 0) {
    degs = seq(xlims[1],-1,ifelse(abs(diff(xlims))>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    graphics::axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    graphics::axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    degs = seq(3,xlims[2],ifelse(abs(diff(xlims))>1,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
    graphics::axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    graphics::axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    degs = c(0)
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
    graphics::axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    graphics::axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
  } else {
    degs = seq(xlims[1],xlims[2],ifelse(ancho>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    graphics::axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    graphics::axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),mgp=c(1,.2,0))
    }
  degs = seq(sl,nl,ifelse(abs(diff(c(sl,nl)))>10,5,2))
  alt = sapply(degs,function(x) bquote(.(x)*degree ~ N))
  graphics::axis(2, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  graphics::axis(4, at=degs, lab=do.call(expression,alt),font.axis=2,cex.axis=.8,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  graphics::rug(seq(c(sl+.5),c(nl+.5),by=1),.005,side=2,lwd=lwdl,quiet=TRUE)
  graphics::rug(seq(c(xlims[1]+.5),c(xlims[2]+.5),by=1),.005,side=1,lwd=lwdl,quiet=TRUE)
  graphics::rug(seq(c(xlims[1]+.5),c(xlims[2]+.5),by=1),.005,side=3,lwd=lwdl,quiet=TRUE)
  graphics::rug(seq(c(sl+.5),c(nl+.5),by=1),.005,side=4,lwd=lwdl,quiet=TRUE)
  graphics::box()
  maps::map(SWC_Q1_w84,SWC_Q1_w84$Name,add=T,col=grDevices::gray(.4),lwd=.1,fill=T,dens=dens,angle=0)
  maps::map(SWC_Q3_w84,SWC_Q3_w84$Name,add=T,col=ifelse(dens==0,1,2),lwd=.1,fill=T,dens=dens,angle=45)
  maps::map(NIGFS_w84,NIGFS_w84$NAME,add=T,col=ifelse(dens==0,1,3),lwd=.1,fill=T,dens=dens,angle=90)
  graphics::plot(IGFS_w84,add=T,col=4,lwd=.01,dens=dens,angle=135)
  graphics::plot(Porc_w84,add=T,col=5,lwd=.01,dens=dens,angle=180)
  graphics::plot(CGFS,add=T,col=6,lwd=.1,dens=dens,angle=225)
  graphics::plot(EVHOE_w84,add=T,col=grDevices::gray(.4),lwd=.1,dens=dens,angle=270)
  graphics::plot(Sp_North_w84,add=T,col=2,lwd=.1,dens=dens,angle=315)
  graphics::plot(PT_IBTS,add=T,col=3,lwd=.1,dens=dens,angle=0)
  graphics::plot(Sp_Cadiz_w84,add=T,col=4,lwd=.1,dens=dens,angle=45)
  maps::map(database = "worldHires",xlim = xlims, ylim = c(sl,nl),fill=T,col="gray",add=T,bg="blue")
  graphics::box()
  if (leg) graphics::legend("bottomleft",c("UK-SCOSWCGFS","UK-SCOROC","UK-NIGFS","IE-IGFS","SP-PORC","FR-CGFS",
                                 "FR-EVHOE","SP-NORTH","PT-PGFS","SP-GCGFS"),fill=c(grDevices::gray(.4),2:6),
                  cex=cex.leg,inset=c(.03,.2),title="Surveys",bg="white",text.col="black",
                  dens=dens,angle=seq(0,315,by=45))
  if (out!="def") grDevices::dev.off()
}
