surveys<-c("UK-SCOSWCGFS","UK-SCOROC","UK-NIGFS","IE-IGFS","SP-PORC","FR-CGFS","FR-WCGFS","FR-EVHOE","SP-NORTH","PT-IBTS","SP-ARSA")
colores<-c("yellow","yellow4","darkgreen","green","red","blue","violet","steelblue","brown","salmon","green4")
cbind(surveys,colores,seq(0,350,by=32))

library(NeAtlIBTS)
png("NeAtlIBTS_map_old.png",width = 1500,height = 2200,pointsize = 40)
IBTSNeAtl_map(shpdir="c:/GitHubRs/shapes/",dens=100)
dev.off()

png("NeAtlIBTS_map_new.NS.png",width = 2000,height = 2200,pointsize = 40)
IBTSNeAtl_map(dens=50,xlims = c(-18,14))
dev.off()

ciudades=c("Galway","Vigo","Santander","Cadiz","Nantes","Belfast","Oslo","Berlin","Lisbon","Aberdeen","Hamburg")

png("NeAtlIBTS_map_new.NS.png",width = 2000,height = 2200,pointsize = 40)
IBTSNeAtl_map(dens=50,nl=62,xlims = c(-18,15),load=F,leg = F)
points(HaulLat~HaulLong,icesDatras::getHHdata("NS-IBTS",2023,3),pch=21,bg="darkgoldenrod1")
points(HaulLat~HaulLong,icesDatras::getHHdata("SCOWCGFS",2023,4),pch=21,bg="yellow")
points(HaulLat~HaulLong,icesDatras::getHHdata("SCOROC",2023,3),pch=21,bg="yellow4")
points(HaulLat~HaulLong,icesDatras::getHHdata("NIGFS",2023,4),pch=21,bg="darkgreen")
points(HaulLat~HaulLong,icesDatras::getHHdata("IE-IGFS",2023,4),pch=21,bg="green")
points(HaulLat~HaulLong,icesDatras::getHHdata("SP-PORC",2023,3),pch=21,bg="red")
points(HaulLat~HaulLong,icesDatras::getHHdata("FR-WCGFS",2023,3),pch=21,bg="violet")
points(HaulLat~HaulLong,icesDatras::getHHdata("FR-CGFS",2023,4),pch=21,bg="blue")
points(HaulLat~HaulLong,icesDatras::getHHdata("EVHOE",2023,4),pch=21,bg="steelblue")
points(HaulLat~HaulLong,icesDatras::getHHdata("SP-NORTH",2023,4),pch=21,bg="brown")
points(HaulLat~HaulLong,icesDatras::getHHdata("PT-IBTS",2023,4),pch=21,bg="salmon")
points(HaulLat~HaulLong,icesDatras::getHHdata("SP-ARSA",2023,4),pch=21,bg="green4")
colores<-c("darkgoldenrod1","yellow","yellow4","darkgreen","green","red","blue","violet","steelblue","brown","salmon","green4")
map.cities(world.cities[world.cities$name %in% ciudades,],label=T,cex=.7,font=2,pch=21,bg="white")
legend("bottomright",c("NS-IBTS","UK-SCOSWCGFS","UK-SCOROC","UK-NIGFS","IE-IGFS","SP-PORC","FR-CGFS",
                         "FR-WCGFS","FR-EVHOE","SP-NORTH","PT-IBTS","SP-ARSA"),fill=colores,
                cex=1.3,inset=c(.03,.03),title="Surveys",bg="white",text.col="black",
                dens=1000,angle=c(0,seq(0,350,by=32)))
dev.off()
