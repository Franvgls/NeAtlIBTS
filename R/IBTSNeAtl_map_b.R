#' Function IBTSNeAtl_map plots the map with all the surveys **STILL ON DEVELOPMENT**
#' 
#' #' @description
#  Produces a map from the shapefiles that define the IBTSNeAtl Surveys from Scotland 
#' to Cádiz, Still needs to include the shapefiles within the package, or right references. 
#'
#' @details
#' The default limits for the IBTS including the North Sea or not, but the script could 
#' be used to get maps all arround the world playing with nl,sl and xlims
#'
#' @param nl = 61.5 northernmost limit of the map
#' @param sl = 35 Southernmost limit of the map
#' @param leg = TRUE if TRUE includes the legend with the colors of the surveys
#' @param legpos defines where the legend is placed
#' @param cex.leg = .7 Size of the legend
#' @param dens = 30 density of the shading lines for all the surveys
#' @param ICESdiv = TRUE if TRUE plots the IBTS divisions behind the shapefiles
#' @param ICESrect = FALSE if TRUE plots the lines of the ICES statistic rectangles
#' @param ICESlab = FALSE if TRUE plots labs por ICES rectangles
#' @param ICESlabcex tamaño del ICESlab en cex, .5 por defecto subirlo si se quiere más grande
#' @param NS = FALSE if TRUE includes the ICES rectangles only for the North Sea area
#' @param bathy = TRUE if TRUE plots the isobaths under the behind the shapefiles
#' @param bw = True plots the map with land in grey, if F in light brown (burlywood3)
#' @param bords = TRUE plots the borders o the countries, FALSE leaves dashed lines
#' @param axlab= .8 decides the size of the axis numbers
#' @param shpdir = path to the folder with the shapefiles
#' @param load = T or F to load all the shapes files.
#' @param places = if T adds towns in the maps
#' @param minpop = adds the limit of population of cities (places) to plot.
#' @param graf si F el gráfico sale en la pantalla, si nombre fichero va a fichero en el directorio de trabajo del Rstudio ver getwd()
#' @param xpng width archivo png si graf es el nombre del fichero
#' @param ypng height archivo png si graf es el nombre del fichero
#' @param ppng points png archivo si graf es el nombre del fichero
#' @examples IBTSNeAtl_map(dens=0,nl=45,leg=F,load=TRUE,ICESrect = T,graf="MapIBTS");text(stat_y~stat_x,Area,labels=ICESNAME,cex=.8,font=4);text(stat_y~stat_x,Area,labels=Area,cex=.6,pos=1,col=2) 
#' @export
IBTSNeAtl_map_b<-function(nl=60.5,sl=36.0,xlims=c(-18,3),leg=TRUE,legpos=c("bottomright"),cex.leg=.7,dens=30,
                        load=TRUE,ICESdiv=TRUE,ICESrect=FALSE,ICESlab=F,ICESlabcex=.8,NS=FALSE,bathy=TRUE,
                        bw=FALSE,axlab=.8,bords=TRUE,lwdl=.1,shpdir="c:/GitHubRs/shapes/",places=FALSE,minpop=200000,
                        graf=FALSE,xpng=1200,ypng=800,ppng=15) {
  library(mapdata)
  library(maptools)
  library(maps,gpclibPermit())
  if (all(c(sl,nl)<0) | all(c(sl,nl)>0)) {
    largo=rev(abs(nl-sl))*1
  } else largo=(nl-sl)
  if (xlims[2] < 0) {
    ancho<- diff(rev(abs(xlims)))*1
  } else ancho<- diff(xlims)*1
  ices.div<-rgdal::readOGR(paste0(shpdir,"ices_div.dbf"),"ices_div",verbose = F)
  bath100<-rgdal::readOGR(paste0(shpdir,"100m.dbf"),"100m",verbose = F)
  bathy.geb<-rgdal::readOGR(paste0(shpdir,"bathy_geb.dbf"),"bathy_geb",verbose = F)
  ices.areasWG84<-sf::st_read(paste0(shpdir,"ices_areasWGS84.shp"),stringAsFactors = FALSE)
  if (load) {
  SWC_Q1<-rgdal::readOGR(paste0(shpdir,"SWC_Q1.dbf"),"SWC_Q1",verbose = F)
  SWC_Q1_w84<-sp::spTransform(SWC_Q1,CRS("+proj=longlat +datum=WGS84"))
  SCOROC<-rgdal::readOGR(paste0(shpdir,"SCOROC.dbf"),"SCOROC",verbose = F)
  SCOROC<-sp::spTransform(SCOROC,CRS("+proj=longlat +datum=WGS84"))
  SCOROC<-rgdal::readOGR(paste0(shpdir,"SCOROC.dbf"),"SCOROC",verbose = F)
  IGFS<-rgdal::readOGR(paste0(shpdir,"IGFS.dbf"),"IGFS",verbose = F)
  IGFS_w84<-sp::spTransform(IGFS,CRS("+proj=longlat +datum=WGS84")) 
  NIGFS<-rgdal::readOGR(paste0(shpdir,"NI_IBTS.dbf"),"NI_IBTS",verbose = F)
  NIGFS_w84<-sp::spTransform(NIGFS,CRS("+proj=longlat +datum=WGS84")) 
  CGFS<-rgdal::readOGR(paste0(shpdir,"CGFS_stratum.dbf"),"CGFS_stratum",verbose = F)
  WCGFS<-rgdal::readOGR(paste0(shpdir,"CGFS_Western_Channel_stratification-2023.dbf"),"CGFS_Western_Channel_stratification-2023",verbose = F)
  Porc<-rgdal::readOGR(paste0(shpdir,"Porcupine.dbf"),"Porcupine",verbose = F)
  Porc_w84<-sp::spTransform(Porc,CRS("+proj=longlat +datum=WGS84")) 
  EVHOE<-rgdal::readOGR(paste0(shpdir,"EVHOE.dbf"),"EVHOE",verbose = F)
  EVHOE_w84<-sp::spTransform(EVHOE,CRS("+proj=longlat +datum=WGS84"))
  Sp_North_w84<-rgdal::readOGR(paste0(shpdir,"Sp_North.WGS84.dbf"),verbose = F) #"Sp_North",
  #Sp_North_w84<-sp::spTransform(Sp_North,CRS("+proj=longlat +datum=WGS84")) 
  #Sp_North<-sf::st_read(paste0(shpdir,"Sp_North.dbf"))
  PT_IBTS<-rgdal::readOGR(paste0(shpdir,"PT_IBTS_2015.dbf"),"PT_IBTS_2015",verbose = F)
  Sp_Cadiz<-rgdal::readOGR(paste0(shpdir,"Sp_Cadiz.dbf"),verbose = F) #"Sp_Cadiz"
  Sp_Cadiz_w84<-sp::spTransform(Sp_Cadiz,CRS("+proj=longlat +datum=WGS84"))
  }
  if (!is.logical(graf)) png(filename=paste0(graf,".png"),width = xpng,height = ypng, pointsize = ppng)
  par(mar=c(3.5,2,2,2)+0.1)
  maps::map(database = "worldHires", xlim = xlims, ylim = c(sl,nl),type="n")
  if (!bw) rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],par("usr")[1],col=ifelse(bw,"white","lightblue1"))
  if (bathy) {
    sp::plot(bath100,add=T,col=ifelse(bw,gray(.85),gray(.50)),lwd=.1)
    sp::plot(bathy.geb[bathy.geb$DEPTH!=100,],add=T,col=ifelse(bw,gray(.85),gray(.50)),lwd=.1)
  }
  # if (grid) grid(col=gray(.8),lwd=.5)
  if (ICESdiv) sp::plot(ices.div,add=T,col=NA,border="burlywood")
  if (all(xlims < 0)) {
    degs = seq(round(xlims[1],0),round(xlims[2],0),ifelse(ancho>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    }
  if (all(xlims > 0)) {
    degs = seq(round(xlims[1],0),round(xlims[2],0),ifelse(ancho>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
  }
  if (xlims[1]<0 & xlims[2]>0) {
    degs = seq(round(xlims[1],0),-1,ifelse(abs(diff(xlims))>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ W))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    degs = seq(2,round(xlims[2],0),ifelse(abs(diff(xlims))>1,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ E))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    degs = c(0)
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
    axis(1, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
    axis(3, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),mgp=c(1,.4,0))
  }
  if (all(c(sl,nl) < 0)) {
    degs = seq(round(sl,0),round(nl,0),ifelse(largo>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ S))
    axis(2, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    axis(4, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  if (all(c(sl,nl) > 0)) {
    degs = seq(round(sl,0),round(nl,0),ifelse(largo>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ N))
    axis(2, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    axis(4, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  if (!all(c(sl,nl) >0)) {
    degs = seq(round(sl,0),-5,ifelse(largo>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ S))
    axis(2, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    axis(4, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    degs = seq(5,nl,ifelse(largo>10,4,1))
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ N))
    axis(2, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    axis(4, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    degs = c(0)
    alg = sapply(degs,function(x) bquote(.(abs(x))*degree ~ ""))
    axis(2, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
    axis(4, at=degs, lab=do.call(expression,alg),font.axis=2,cex.axis=axlab,tick=T,tck=c(-.01),las=2,mgp=c(1,.5,0))
  }
  if (NS){
    rect(-4,55.5,9.5,60.2,col="tomato1")
    rect(-2,50.0,9.5,60.2,col="tomato1")
    for (lat in seq(55,66,by=.5)) {segments(x0=c(-4),y0=lat,x1=12,y1=lat,col=gray(.85),lwd=.01) }
    for (lat in seq(49.5,55,by=.5)) {segments(x0=c(-2),y0=lat,x1=12,y1=lat,col=gray(.85),lwd=.01) }
    for (long in seq(c(-4),12,by=1)) {segments(x0=long,y0=55,x1=long,y1=65,col=gray(.85),lwd=.01) }
    for (long in seq(c(-2),12,by=1)) {segments(x0=long,y0=50,x1=long,y1=55,col=gray(.85),lwd=.01) }
    }
  if (ICESlab) text(c(stat_y+.22)~stat_x,Area,label=ICESNAME,cex=ICESlabcex,font=2)
  if (ICESrect) {
    abline(h=seq(30,65,by=.5),v=seq(-44,68),col=gray(.8),lwd=.2)
    }
  rug(seq(c(round(sl,0)+.5),c(round(nl,0)+.5),by=1),.005,side=2,lwd=lwdl,quiet=TRUE)
  rug(seq(c(round(xlims[1],0)+.5),c(round(xlims[2],0)+.5),by=1),.005,side=1,lwd=lwdl,quiet=TRUE)
  rug(seq(c(round(xlims[1],0)+.5),c(round(xlims[2],0)+.5),by=1),.005,side=3,lwd=lwdl,quiet=TRUE)
  rug(seq(c(round(sl,0)+.5),c(round(nl,0)+.5),by=1),.005,side=4,lwd=lwdl,quiet=TRUE)
  if (load){
    maps::map(SWC_Q1_w84,SWC_Q1_w84$Name,add=T,col="yellow",lwd=.1,fill=T,dens=dens,angle=0)
    sp::plot(SWC_Q1_w84,add=T,col="yellow",lwd=.01,dens=dens,angle=0)
    maps::map(SCOROC,SCOROC$Name,add=T,col="yellow4",lwd=.1,fill=T,dens=dens,angle=32)
    sp::plot(SCOROC,add=T,col="yellow4",lwd=.01,dens=dens,angle=32)
    maps::map(NIGFS_w84,NIGFS_w84$NAME,add=T,col="cyan4",lwd=.1,fill=T,dens=dens,angle=64)
    sp::plot(NIGFS_w84,add=T,col="cyan4",lwd=.01,dens=dens,angle=64)
    sp::plot(IGFS_w84,add=T,col="green",lwd=.01,dens=dens,angle=96)
    sp::plot(Porc_w84,add=T,col="red",lwd=.01,dens=dens,angle=128)
    sp::plot(CGFS,add=T,col="navy",lwd=.1,dens=dens,angle=160)
    sp::plot(WCGFS,add=T,col="violet",lwd=.1,dens=dens,angle=192)
    sp::plot(EVHOE_w84,add=T,col="blue",lwd=.1,dens=dens,angle=224)
    sp::plot(Sp_North_w84,add=T,col="orange",lwd=.1,dens=dens,angle=256)
    sp::plot(PT_IBTS,add=T,col="lightgreen",lwd=.1,dens=dens,angle=288)
    sp::plot(Sp_Cadiz_w84,add=T,col="sienna",lwd=.1,dens=dens,angle=320)
  }
  maps::map(database = "worldHires",xlim = xlims, ylim = c(sl,nl),fill=T,col=ifelse(bw,"gray","burlywood3"),add=T,fg="blue",interior = T,boundary = T,lty=1,lwd=.0005)
  if(places) { 
    #map.cities(minpop = minpop,label = T,font=2,cex=.9,pch=21,bg="green4")
    #maps::map(database = "worldHires",xlim = xlims, ylim = c(sl,nl),fill=T,col=ifelse(bw,"gray",add=T,bg="blue",interior=bords)
    points(-6.2602777777778,53.349722222222, pch=18)
    text(-6.2602777777778,53.349722222222, "Dublin",cex=.5*cex.leg,font = 2,pos=2)
    points(-9.1393,38.7223,pch=18)
    text(-9.1393,38.7223,"Lisbon",cex=.5*cex.leg,font=2,pos=4)
    points(2.3488,48.85341,pch=18)
    text(2.3488,48.85341,"Paris",cex=.5*cex.leg,font=2,pos=3)
    points(-3.713,40.2085,pch=18)
    text(-3.713,40.2085,"Madrid",cex=.5*cex.leg,font=2,pos=3)
    points(4.34878,50.85045,pch=18)
    text(4.34878,50.85045,"Brussels",cex=.5*cex.leg,font=2,pos=3)
    points(-0.12776,51.50735,pch=18)
    text(-0.12776,51.50735,"London",cex=.5*cex.leg,font=2,pos=3)
    points(12.56833,55.67611,pch=18)
    text(12.56833,55.67611,"Copenhagen",cex=.5*cex.leg,pos=3)
  }
  box()
  if (leg & NS) {survs<-c("NS-IBTS","SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC","FR-CGFS",
                          "FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
    legend(legpos,legend=dplyr::filter(IBTSsurvs,survey %in% survs)$survey,fill=dplyr::filter(IBTSsurvs,survey %in% survs)$color,
                  cex=cex.leg,inset=c(.03,.03),title="SURVEYS",bg="white",text.col="black",
                  dens=dens,angle=seq(0,350,by=32))
      }
  if (leg & !NS) {survs<-c("SCOWCGFS","SCOROC","NIGFS","IE-IGFS","SP-PORC","FR-CGFS",
                           "FR-WCGFS","EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
    legend(legpos,legend=dplyr::filter(IBTSsurvs,survey %in% survs)$survey,fill=dplyr::filter(IBTSsurvs,survey %in% survs)$color,
                        cex=cex.leg,inset=c(.03,.03),bg="white",text.col="black",
                        dens=dens,angle=seq(0,350,by=32))
      }
    if (!is.logical(graf)) {
    dev.off()
    message(paste0("figura: ",getwd(),"/",graf,".png"))
  }
}
