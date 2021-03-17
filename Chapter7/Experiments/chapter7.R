##########################################
# 学校选址案例分析
##7.2 案例分析与实施
###7.2.1 人口增长要求
library(sp)
library(maptools)
library(RColorBrewer)
library(rgeos)
setwd("E:\\R_course\\Chapter7\\Data")

DublinEDs <- readShapePoly("DublinEDs", verbose=T,proj4string = CRS("+init=epsg:27700"))
plot(DublinEDs)
str(DublinEDs@data)

mypalette <- brewer.pal(7, "Blues")
map.na = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(325500,262500), scale = 4000, col=1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(299000,218000), scale = 5000, col=1, fill=c("transparent","skyblue"))
map.scale.2  = list("sp.text", c(299000,218000), "0", cex=0.9, col=1)
map.scale.3  = list("sp.text", c(305000,218000), "5km", cex=0.9, col=1)
map.layout<- list(map.na,map.scale.1,map.scale.2,map.scale.3)
spplot(DublinEDs, "POP_Change", main = "Pop Change in Dublin", key.space = "right", col.regions =mypalette,cuts=6,sp.layout=map.layout)

DublinEDs.Pop2k <- DublinEDs[DublinEDs$POP_Change>2000,]
plot(DublinEDs, col="#BDD7E7")
plot(DublinEDs.Pop2k, col="#3182BD", add=T)

###7.2.2 交通可达性要汿

library(rgeos)
DublinRoads <- readShapeLines("DublinRoads", verbose=T,proj4string = CRS("+init=epsg:27700"))
schools <- read.csv("Dublin_PrimarySchools.csv")
x <- schools$EASTING_
y <- schools$NORTHING
DublinSchools <- SpatialPoints(data.frame(x,y) ,proj4string = CRS("+init=epsg:27700"))

Dist3k <- gWithinDistance(DublinSchools, DublinRoads, dist=3000, byid=T)
DublinRoads.buf <- gBuffer(DublinRoads, width=3000)
SchoolDist3k <- DublinSchools[as.logical(apply(Dist3k, 2, sum)),]
plot(DublinRoads.buf, col="skyblue")
plot(DublinRoads, add=T, cex=0.5)
plot(SchoolDist3k, pch=16, col="blue", add=T)
plot(DublinSchools[!as.logical(apply(Dist3k, 2, sum)),], pch=16, col="grey", add=T)



###7.2.3 宗教要求
DublinSchools.df <- SpatialPointsDataFrame(DublinSchools, data=data.frame(x,y))
DublinSchools.df@data$DENOMINATI <- schools$DENOMINATI
SchoolMulti <- DublinSchools[DublinSchools.df@data$DENOMINATI=="MULTI DENOMINATIONAL"]
Multi <- gContains(DublinEDs.Pop2k, SchoolMulti, returnDense=F, byid = T)
NoMulti <- c()
for (i in 1:length(Multi)){
	if(length(Multi[[i]]) == 0){
		NoMulti[i] <- TRUE
	} 
	else{
		NoMulti[i] <- FALSE
	}
}		
EDNoMulti <- DublinEDs.Pop2k[as.logical(matrix(NoMulti)),]
plot(DublinEDs, col="#BDD7E7")
plot(EDNoMulti, col="#3182BD", add=T)


###7.2.4 招生数量要求
DublinSchools.df@data$ENROLLMENT <- schools$ENROLLMENT

mypalette <- brewer.pal(5, "Blues")
spplot(DublinSchools.df, "ENROLLMENT", main = "School Enrollment", key.space = "right", cex = DublinSchools.df@data$ENROLLMENT/180, col.regions =mypalette,cuts=4,sp.layout=map.layout)

DublinSchools.Enroll2b <- DublinSchools[DublinSchools.df@data$ENROLLMENT<200,]
plot(DublinEDs)
plot(DublinSchools, pch=16, cex=0.5, col="skyblue",add=T)
plot(DublinSchools.Enroll2b, col="blue", pch=16, add=T)

Pop_Multi_Enr1 <- gContains(EDNoMulti, DublinSchools.Enroll2b, returnDense=F, byid = T)
Pop_Multi_Enr2 <- c()
for (i in 1:length(Pop_Multi_Enr1)){
	if(length(Pop_Multi_Enr1[[i]]) == 0){
		Pop_Multi_Enr2[i] <- FALSE
	} 
	else{
		Pop_Multi_Enr2[i] <- TRUE
	}
}		
EDPop_Multi_Enr <- EDNoMulti[as.logical(matrix(Pop_Multi_Enr2)),]
plot(DublinEDs, col="#BDD7E7")
plot(EDPop_Multi_Enr, col="#3182BD", add=T)

###7.2.5 学校分布要求
Dublin.Inter <- gIntersection(DublinRoads.buf, EDPop_Multi_Enr)
plot(DublinEDs, col="#BDD7E7")
plot(Dublin.Inter, col="#3182BD", add=T)

plot(DublinSchools, pch=16, col="white", cex= 0.8, add=T)
x <- c(312073.2, 305995.5, 319630.7)
y <- c(242611.0, 226809.0, 224589.3)
points(x,y, pch=13, col="red", cex=1.5)