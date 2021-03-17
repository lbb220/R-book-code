##########################################
# 第6章 R语言统计分析
##6.1 本章所需R函数包
install.packages("gstat")
install.packages("automap")
install.packages("spdep")
library(gstat)
library(automap)
library(spdep)

##6.2 插值
require(maptools)
setwd("E:\\R_course\\Chapter6\\Data")
LNHP <- readShapePoints("LNHP", verbose=T,proj4string = CRS("+init=epsg:27700"))
LN.bou <- readShapePoly("LondonBorough", verbose=T,proj4string = CRS("+init=epsg:27700"))
require(raster)
LN.lat <- raster(nrow=30, ncol=60, ext=extent(LN.bou))
LN.lat <- rasterize(LN.bou, LN.lat, "NAME")

grid = rasterToPoints (LN.lat, spatial = TRUE)

dist = gDistance(LNHP,grid, byid = TRUE)
nearest_dat = apply(dist, 1, which.min)

grid$nn = LNHP$PURCHASE[nearest_dat]
grid = rasterize(grid,LN.lat, "nn")
plot(grid,main = "GW regression residuals(IDW interpolate)")

###6.2.1 IDW插值
g1 = gstat(formula = LNHP$PURCHASE~ 1, data = LNHP,set = list(idp=0.3))
g2 = gstat(formula = LNHP$PURCHASE~ 1, data = LNHP,set = list(idp=10))

z1 = interpolate(LN.lat, g1)
z2 = interpolate(LN.lat, g2)

z1 = mask(z1, LN.lat)
z2 = mask(z2, LN.lat)

plot(z1,main = "London Purchase(IDW interpolate)",sub = "beta = 0.3")
plot(z2,main = "London Purchase(IDW interpolate)",sub = "beta = 10")

###6.2.2 克里金插值
g = gstat(formula = LNHP$PURCHASE~ 1, data = LNHP)
ev = variogram(g) 
plot(ev)

test<-data.frame(LNHP$X,LNHP$Y)
index<-duplicated(test)

LNHP_new<-LNHP[!index,]
v = autofitVariogram(formula = LNHP_new$PURCHASE~ 1, input_data = LNHP_new)
plot(v)

g_OK = gstat(formula = LNHP_new$PURCHASE~1, data = LNHP_new, model = v$var_model)
z = interpolate(LN.lat, g_OK)

z = mask(z, LN.lat)
plot(z,main= "London Purchase(Ordinary Kriging)")

##6.3 空间自相关
###6.3.1 全局空间自相关
LNHPnb <- knn2nb(knearneigh(LNHP, k = 8, longlat = TRUE))
LNHPnb_s <- make.sym.nb(LNHPnb) 
plot(nb2listw(LNHPnb_s), cbind(LNHP$X, LNHP$Y))

col.W<-nb2listw(LNHPnb_s,style = "W")
str(moran( LNHP$PURCHASE ,col.W,length(LNHP$PURCHASE),Szero(col.W)))

moran_LNHP <- moran.test(LNHP$PURCHASE, listw = nb2listw(LNHPnb_s))
moran_LNHP

moran_LNHP_ran <- moran.test(LNHP$PURCHASE, listw = nb2listw(LNHPnb_s),randomisation = FALSE)
moran_LNHP_ran

str(geary( LNHP$PURCHASE ,col.W,length(LNHP$PURCHASE),length(LNHP$PURCHASE)-1,Szero(col.W)))

GR_LNHP <- geary.test(LNHP$PURCHASE, listw = nb2listw(LNHPnb_s))
GR_LNHP

GR_LNHP_ran<- geary.test(LNHP$PURCHASE, listw = nb2listw(LNHPnb_s),randomisation = FALSE)
GR_LNHP_ran

###6.3.2 局部空间自相关
lm_LNHP <- localmoran(LNHP$PURCHASE, listw = nb2listw(LNHPnb_s ,style = "W"))

library(RColorBrewer)
LNHP$lm<-lm_LNHP[,1]
lenData<-length(LNHP$lm)
delCol<-c()
for(i in 1:lenData){
	if (LNHP$lm[i]<=-1||LNHP$lm[i]>1){
		delCol<-c(delCol,i)
	}
}
LNHP_del<-LNHP[-c(delCol),]
mypalette <- brewer.pal(5, "Blues")
LN_bou <-  list("sp.polygons",LN.bou)
map.layout<- list(LN_bou)
spplot(LNHP_del, "lm", main = "Local Moran statistic", key.space = "right", pch=16,cex = (LNHP_del$lm+0.5)*2,col.regions =mypalette,cuts=6,sp.layout=map.layout)


moran.plot(LNHP$PURCHASE,col.W,pch = 19)

G_LNHP <- localG(LNHP$PURCHASE, listw = nb2listw(LNHPnb_s,style = "W"))
LNHP$G<- G_LNHP[1: lenData]
spplot(LNHP, "G", main = "Local Geary’s C statistic", key.space = "right", pch=16,cex = (LNHP$G+5)/3,col.regions =mypalette,cuts=6,sp.layout=map.layout)

##6.4 空间回归分析
###6.4.1 线性回归

lm_LN<-lm(PURCHASE~FLOORSZ,data=LNHP)
summary(lm_LN)

plot(LNHP$FLOORSZ,LNHP$PURCHASE)
abline(a = 8187.62,b  = 1552.39, col ="blue")

lm_LN<-lm(PURCHASE~FLOORSZ + TYPEDETCH + TYPEFLAT + BLDPWW1 + BLDPOSTW + BLD70S + BLD90S + BATH2 + PROF, data=LNHP)
summary(lm_LN)

###6.4.2 地理加权回归
DeVar <- "PURCHASE"
InDeVars<-c("FLOORSZ", "TYPEDETCH", "TYPEFLAT", "BLDPWW1", "BLDPOSTW", "BLD60S", "BLD70S", "BLD80S", "BLD90S", "BATH2", "PROF")
model.sel<-model.selection.gwr(DeVar,InDeVars,data=LNHP,kernel = "gaussian",adaptive=TRUE,bw=10000000000000)
sorted.models <- model.sort.gwr(model.sel, numVars = length(InDeVars), ruler.vector = model.sel[[2]][,2])
model.list <- sorted.models[[1]]
model.view.gwr(DeVar, InDeVars, model.list = model.list)
plot(sorted.models[[2]][,2], col = "black", pch = 20, lty = 5, main = "Alternative view of GWR model selection procedure",ylab = "PURCHASE", xlab = "Model number", type = "b")

bw.gwr.1<-bw.gwr(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S,data=LNHP,approach = "AICc",kernel = "gaussian", adaptive = TRUE)
bgwr.res <- gwr.basic(PURCHASE~FLOORSZ+PROF+BATH2+BLDPWW1+TYPEDETCH+BLD60S+BLD70S, data = LNHP,bw = bw.gwr.1, kernel = "gaussian", adaptive = TRUE)
print(bgwr.res)

mypalette.6 <- brewer.pal(6, "Spectral")
map.na = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(556000,195000), scale = 4000, col=1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(511000,158000), scale = 5000, col=1, fill=c("transparent","green"))
map.scale.2  = list("sp.text", c(511000,157000), "0", cex=0.9, col=1)
map.scale.3  = list("sp.text", c(517000,157000), "5km", cex=0.9, col=1)
LN_bou <-  list("sp.polygons",LN.bou)
map.layout<- list(LN_bou, map.na,map.scale.1,map.scale.2,map.scale.3)

spplot(bgwr.res$SDF, "residual", key.space = "right",col.regions = mypalette.6, at = c(-8, -6, -4, -2, 0, 2, 4),main = "Basic GW regression coefficient estimates for residual ",sp.layout = map.layout)
spplot(bgwr.res$SDF, "FLOORSZ", key.space = "right",col.regions = mypalette.6, at = c(-8, -6, -4, -2, 0, 2, 4),main = "Basic GW regression coefficient estimates for floor size ",sp.layout = map.layout)

require(rgeos)
dist = gDistance(LNHP,LN.bou, byid = TRUE)
nearest_dat = apply(dist, 1, which.min)
LN.bou$nn = bgwr.res$SDF$residual[nearest_dat]
spplot(LN.bou,"nn", sp.layout = map.layout,main = "GW regression residuals")
LN.bou$floosz = bgwr.res$SDF$FLOORSZ[nearest_dat]
spplot(LN.bou,"floosz", sp.layout = map.layout,main = "GW regression of floor size")

g = gstat(formula = bgwr.res$SDF$residual~ 1, data = LNHP,set = list(idp=0.3))
z = interpolate(LN.lat, g)
z = mask(z, LN.lat)
plot(z,main = "GW regression residuals(IDW interpolate)") 
