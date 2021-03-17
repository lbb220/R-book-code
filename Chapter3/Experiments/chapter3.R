##########################################
# 第3章 R语言空间数据处理
##3.1 本章所需R函数包
install.packages("sp")
install.packages("maptools")
library(sp)
library(maptools)
library(rgdal)

EPSG <- make_EPSG()
EPSG[grep("China Geodetic Coordinate System", EPSG$note), ]
CRS("+init=epsg:4490") 
showWKT("+init=epsg:4490")
showP4(showWKT("+init=epsg:4490"))

##3.2 R中空间数据基本类型
library(sp)
getClass("Spatial")

###3.2.1 点数据
x = c(1,2,3,4,5)
y = c(3,2,5,1,4)
Spt <- SpatialPoints(cbind(x,y))
class(Spt)
plot(Spt)
Spt <- SpatialPoints(list(x,y))
class(Spt)
plot(Spt)
Spt <- SpatialPoints(data.frame(x,y))
class(Spt)
plot(Spt,cex=2)
Spt.df <- SpatialPointsDataFrame(Spt, data=data.frame(x,y))
class(Spt.df)
str(Spt.df)
Spt.df@data
coordinates(Spt.df)

###3.2.2 线数据
l1 <- cbind(c(1,2,3),c(3,2,2))
l1a <- cbind(l1[,1]+.05,l1[,2]+.05)
l2 <- cbind(c(1,2,3),c(1,1.5,1))
Sl1 <- Line(l1)
Sl1a <- Line(l1a)
Sl2 <- Line(l2)
S1 = Lines(list(Sl1, Sl1a), ID="a")
S2 = Lines(list(Sl2), ID="b")
Sl = SpatialLines(list(S1,S2))
cols <- data.frame(c("red", "blue"))
Sl.df <-SpatialLinesDataFrame(Sl, cols,match.ID = F)
summary(Sl.df)

str(Sl.df)
plot(Sl.df, col=c("red", "blue"))

###3.2.3 面数据
Poly1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
Poly2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
Poly3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
Poly4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
Polys1 = Polygons(list(Poly1), "s1")
Polys2 = Polygons(list(Poly2), "s2")
Polys3 = Polygons(list(Poly3, Poly4), "s3/4")
SPoly = SpatialPolygons(list(Polys1,Polys2,Polys3), 1:3)
SPoly.df <- SpatialPolygonsDataFrame(SPoly, data.frame(coordinates(SPoly)),match.ID = F)


str(SPoly.df)
plot(SPoly.df, col = 1:3, pbg="white")

###3.2.4 栅格数据
sp.df = data.frame(z = c(1:6,NA,8,9),
                xc = c(1,1,1,2,2,2,3,3,3),
                yc = c(rep(c(0, 1.5, 3),3)))
coordinates(sp.df) <- ~xc+yc
gridded(sp.df) <- TRUE
str(sp.df)
image(sp.df["z"])
cc = coordinates(sp.df)
z=sp.df[["z"]]
zc=as.character(z)
zc[is.na(zc)]="NA"
text(cc[,1],cc[,2],zc)



grd <- GridTopology(c(1,1), c(1,1), c(10,10))
sg.df <- SpatialGridDataFrame(grid = grd, data = data.frame(coordinates(grd)))
str(sg.df)                              
plot(sg.df)
text(coordinates(sg.df), labels=row.names(sg.df))

##3.3 空间数据导入导出
require(maptools)
setwd("E:\\R_course\\Chapter3\\Data")
getwd()

LNHP <- readShapePoints("LNHP", verbose=T,proj4string = CRS("+init=epsg:27700"))
summary(LNHP)
plot(LNHP)
LNNT <- readShapeLines("LNNT", verbose=T,proj4string = CRS("+init=epsg:27700")) 
summary(LNNT)
plot(LNNT)

LN.bou <- readShapePoly("LondonBorough", verbose=T,proj4string = CRS("+init=epsg:27700"))
summary(LN.bou)
plot(LN.bou)

writePointsShape(LNHP, fn="LNHP_w")
writeLinesShape(LNNT, fn="LNNT_w")
writePolyShape(LN.bou, fn="LondonBorough_w")

#3.4 空间数据处理
class(LNHP@data)
summary(LNHP@data)
new.df <- LNHP@data
new.df$coords_x1_ <- NULL
new.df$coords_x2_ <- NULL
LNHP@data <- new.df
summary(LNHP@data)

price <-new.df$PURCHASE
floorsz <- new.df$FLOORSZ
aveP <-price/floorsz 
new.df["AveragePrice"] <- aveP
LNHP@data <- new.df
summary(LNHP@data)  

aveP <- data.frame(aveP)
names(aveP) <-"AveragePrice" 
LNHP <-spCbind(LNHP,aveP) 
summary(LNHP@data)


coord.spt<- coordinates(LNHP)
plot(LNHP)
points(coord.spt, col="red")

coord.spl<- coordinates(LNNT)
plot(LNNT)
for(i in 1:length(coord.spl))
   points(coord.spl[[i]][[1]], col="red",cex=0.4)

coord.spol<- coordinates(LN.bou)
plot(LN.bou)
points(coord.spol, col="red")

LNNT.nod <- gPolygonize(gNode(LNNT))
plot(LNNT)
plot(LNNT.nod, col="red",cex=0.4)


bbox(LNHP)
bbox(LNNT)
bbox(LN.bou)

LNHP.gCH <- gConvexHull(LNHP)
plot(LNHP)
plot(LNHP.gCH, col=NULL, border="red", lwd=2, add=T)

LNbou.gEl <- gEnvelope(LN.bou)
plot(LN.bou)
plot(LNbou.gEl, col=NULL, border="red", lwd=2, add=T)

gLength(LNNT)
gLength(LNNT, byid=T)

gArea(LN.bou)
gArea(LN.bou, byid=T)

LN.bou1 <- readShapePoly("LN_bou_p1", proj4string = CRS("+init=epsg:27700"))
LN.bou2 <- readShapePoly("LN_bou_p2", proj4string = CRS("+init=epsg:27700"))
plot(LN.bou1, border="blue", xlim=bbox(LN.bou)[1,], ylim=bbox(LN.bou)[2,])
plot(LN.bou2, border="red",add=T)
LN.bou.un <- gUnion(LN.bou1, LN.bou2, byid=T)
plot(LN.bou.un, border="blue")

plot(LN.bou.un, border="grey")
plot(gUnaryUnion(LN.bou1), border= "blue", add=T, lwd=2)
plot(gUnaryUnion(LN.bou2), border= "red", add=T, lwd=2)

LNNT.a <- LNNT[LNNT$RoadType=="a",]
plot(LNNT, col="grey")
 plot(LNNT.a, col="red", lwd=1.5, add=T)



LNHP.buf <- gBuffer(LNHP, width=500)
plot(LNHP.buf, col="green")
plot(LNHP, add=T, cex=0.5)

LNNTa.buf <- gBuffer(LNNT.a, width=200)
plot(LNNTa.buf, col="green")
plot(LNNT.a, add=T)

LNBO.buf <- gBuffer(LN.bou, width=1000)
plot(LNBO.buf, col="green")
plot(LN.bou, col="grey",add=T)

spt.rand <- spsample(LN.bou, 100,type="random")
spt.rand.dt <- gDelaunayTriangulation(spt.rand)
plot(LN.bou, col="green")
plot(spt.rand.dt, add=T)
plot(spt.rand, col="red", add=T)

### 3.4.4 空间关系处理与分析
dmat1 <- gDistance(LNHP, byid=T)
dim(dmat1)

dist100 <- gWithinDistance(LNHP, LNNT.a, dist=100, byid=T)
plot(LNNT.a)
plot(LNHP[as.logical(apply(dist100, 2, sum)),], pch=16, col="red", add=T)


l1 <- Line(cbind(c(1,2,3),c(3,2,2)))
S1 = SpatialLines(list(Lines(list(l1), ID="a")))
Poly1 = Polygon(cbind(c(2,4,4,1,2),c(3,3,5,4,4)))
Polys1 = SpatialPolygons(list(Polygons(list(Poly1), "s1")))
plot(S1, col="blue",xlim=c(1,4), ylim=c(2,5))
plot(Polys1, add=T)
plot(gNearestPoints(S1, Polys1), add=TRUE, col="red", pch=7)
lines(coordinates(gNearestPoints(S1, Polys1)), col="red", lty=3)




LN.bou.dt <- gIntersection(LN.bou, spt.rand.dt)
plot(LN.bou.dt, col="green", xlim=bbox(LN.bou)[1,], ylim=bbox(LN.bou)[2,],lwd=3)
plot(LN.bou, lty=2, add=T)
plot(spt.rand.dt, lty=3, add=T)

river <- readShapePoly("River", proj4string = CRS("+init=epsg:27700"))
plot(LN.bou, col="grey")
plot(river, col="green", add=T)

LN.bou.dif <- gDifference(LN.bou, river, byid=T)
plot(LN.bou.dif, col="grey")
LN.bou.symdif <- gSymdifference(LN.bou, river)
plot(LN.bou.symdif , col="grey")