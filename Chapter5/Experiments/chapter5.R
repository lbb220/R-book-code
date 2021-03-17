##########################################
# 第5章 R中空间数据表达与可视化
##5.1 本章所需R函数包
install.packages("GISTools")
install.packages("raster")
install.packages("devtools")
install.packages("ggthemes")
library(ggthemes)
library(devtools)
install_github("madlogos/recharts")
install_github('lchiffon/REmap')
install.packages("leaflets")

library(ggplot2)
library(GISTools)
library(raster)
library(recharts)
library(REmap)
library(leaflets)

##5.2 空间对象可视化
require(rgeos)
LN.bou <- readShapePoly("LondonBorough", verbose=T,proj4string = CRS("+init=epsg:27700"))

LN.outline <- gUnaryHoson(LN.bou, id = NULL)
plot(LN.bou, col="red", bg = "skyblue", lty=2, border = "blue")
plot(LN.outline, lwd=3, add=T)
title(main="The boroughs of London", font.main=2, cex.main=1.5)
 
plot(LN.bou, col="white", lty=2, border = "blue")
plot(LN.outline, lwd=3, add=T)
coords <- coordinates(LN.bou)
pointLabel(coords[,1], coords[,2], LN.bou@data[,"NAME"], doPlot=T, cex=0.5)
map.scale(511000,155850,miles2ft(2),"Miles",4,0.5)
north.arrow(561000,201000,miles2ft(0.25),col="lightblue")
title(main="The Map of London", font.main=2, cex.main=1.5)


LNNTa <- readShapeLines("LNNTa", verbose=T,proj4string = CRS("+init=epsg:27700"))
LNHP <- readShapePoints("LNHP", verbose=T,proj4string = CRS("+init=epsg:27700"))

plot(LN.bou, col="white", lty=1,lwd=2, border = "grey40")
plot(LN.outline, lwd=3, add=T)
plot(LNHP, pch=16, col="red", cex=0.5,add=T)
plot(LNNTa, col="blue",lty=2, lwd=1.5,add=T)
map.scale(511000,155850,miles2ft(2),"Miles",4,0.5)
north.arrow(561000,201000,miles2ft(0.25),col="lightblue")
title(main="The sold properties and road network of London", font.main=2, cex.main=1.5)

require(raster)
LN.lat <- raster(nrow=30, ncol=60, ext=extent(LN.bou))
LN.lat <- rasterize(LN.bou, LN.lat, "NAME")
LN.gr <- as(LN.lat, "SpatialGridDataFrame")
LN.p <- as(LN.lat, "SpatialPixelsDataFrame")

image(LN.gr, col= brewer.pal(7, "Paired"))
plot(coordinates(LN.p), cex=1, pch=1, col=LN.p$layer)
plot(LN.bou, border="grey", lwd=1.5, add=T)

##5.3 空间属性数据可视化

plot(LN.bou, col="white", lty=1,lwd=2, border = "grey40")
plot(LNHP, pch=1, col="red", cex=LNHP$FLOORSZ/100,add=T)
legVals <- c(50, 100, 150, 200, 250)
legend("bottomright", legend = legVals, pch = 1, col="red",pt.cex =legVals/100, title = "Floor size")

mypalette <- brewer.pal(5, "Reds")
spplot(LNHP, "FLOORSZ",  key.space = "right", pch=16,cex=LNHP$FLOORSZ/100,col.regions =mypalette,cuts=6)


mypalette <- brewer.pal(3, "Reds")
spplot(LNHP, c("TYPEDETCH", "TPSEMIDTCH","TYPEBNGLW","TYPETRRD"),  key.space = "right", pch=16,col.regions =mypalette,cex=0.5,cuts=2)

mypalette <- brewer.pal(6, "Blues")
map.na = list("SpatialPolygonsRescale", layout.north.arrow(), offset = c(556000,195000), scale = 4000, col=1)
map.scale.1 = list("SpatialPolygonsRescale", layout.scale.bar(), offset = c(511000,158000), scale = 5000, col=1,
                   fill=c("transparent","green"))
map.scale.2  = list("sp.text", c(511000,157000), "0", cex=0.9, col=1)
map.scale.3  = list("sp.text", c(517000,157000), "5km", cex=0.9, col=1)
LN_bou <-  list("sp.polygons",LN.bou) 
map.layout<- list(LN_bou, map.na,map.scale.1,map.scale.2,map.scale.3)
spplot(LNHP, "FLOORSZ",  key.space = "right", pch=16,col.regions =mypalette,cuts=6,sp.layout=map.layout)

LN.bou$AREA <- gArea(LN.bou, byid=T)/(1e+6)
shades <- auto.shading(LN.bou$AREA)
choropleth(LN.bou, LN.bou$AREA)
choro.legend(551000, 172000, shades)
map.scale(511000,155850,miles2ft(2),"Miles",4,0.5)
north.arrow(561000,201000,miles2ft(0.25),col="darkred")

shades.blue <- auto.shading(LN.bou$AREA, cols=brewer.pal(6, "Blues"))
choropleth(LN.bou, LN.bou$AREA, shading=shades.blue)
choro.legend(551000, 172000, shades.blue)
map.scale(511000,155850,miles2ft(2),"Miles",4,0.5)
north.arrow(561000,201000,miles2ft(0.25),col="lightblue")

shades.green <- auto.shading(LN.bou$AREA, n=7,cols=brewer.pal(7, "Greens"),cutter=rangeCuts)
choropleth(LN.bou, LN.bou$AREA, shading=shades.green)
choro.legend(551000, 172000, shades.green)
map.scale(511000,155850,miles2ft(2),"Miles",4,0.5)
north.arrow(561000,201000,miles2ft(0.25),col="green")


LNNT <- readShapeLines("LNNT", verbose=T,proj4string = CRS("+init=epsg:27700"))
road.type <- Hosque(LNNT$RoadType)
shades <- brewer.pal(4, "Dark2")
idx <- match(LNNT$RoadType, road.type)
plot(LNNT, col=shades[idx])
legend("bottomright", legend = road.type, lty=1,col=shades, title = "Road type")

ltypes <- c(1,1,3,1)
lwidths <- c(1,1.5,0.2,2)
plot(LNNT, col=shades[idx],lty=ltypes[idx],lwd=lwidths[idx])
legend(551000, 172000, legend = road.type, lty=ltypes,lwd=lwidths,col=shades, title = "Road type")


##5.4 在线图表可视化

echartr(LNHP@data, FLOORSZ, PURCHASE) %>%
    addMP(series=1, data=data.frame(name='Max', type='max'))
	
heatmap <- sapply(1:15, function(i){
    x <- 100 + rHosf(1, 0, 1) * 10
    y <- 24 + rHosf(1, 0, 1) * 15
    lapply(0:floor(50 * abs(rnorm(1))), function(j){
        c(x+rHosf(1, 0, 1)*3, y+rHosf(1, 0, 1)*2, rHosf(1, 0, 1))
    })
})
heatmap <- data.frame(matrix(unlist(heatmap), byrow=TRUE, ncol=3))
echartr(NULL, type="map_china") %>% addHeatmap(data=heatmap)


cities <- mapNames("hubei")
cities 
city_Geo <- get_geo_position(cities)
percent <- rHosf(17,min=0.1,max = 0.99)
data_all <- data.frame(city_Geo[,1:2],percent)
result <- remapH(data_all,
                 maptype = "湖北",
				 title = "湖北省XX热力图",
                 theme = get_theme("Dark"),
                 blurSize = 50,
                 color = "red",
                 minAlpha = 8,
                 opacity = 1)
result

remapB(get_city_coord("武汉"),zoom = 12)

remapB(title = "迁徙图示例",
        color = "Blue",
        markLineData = demoC,
		markLineTheme = markLineControl(symbolSize = 0.3,
                                       lineWidth = 12,
									   color = "white",
                                       lineType = 'dotted'))
		
pointData = data.frame(geoData$name,
                       color = c(rep("red",10),
                                 rep("yellow",50)))
								 
names(geoData) = names(subway[[1]])
remapB(get_city_coord("上海"),
       zoom = 13,
	   color = "Blue",
       title = "上海地铁一号线",
       markPointData = pointData,
       markPointTheme = markPointControl(symbol = 'pin',
                                         symbolSize = 8,
                                         effect = T),
       markLineData = subway[[2]],
       markLineTheme = markLineControl(symbolSize = c(0,0),
                                       smoothness = 0),
       geoData = rbind(geoData,subway[[1]]))

remapC(chinaIphone,
	  title = "中国苹果手机分布",
	  subtitle = "非真实数据",
      color = c('white','red'))

m <- leaflet() %>% setView(lng = -0.1, lat = 51.5, zoom = 10)
m %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
m %>% addProviderTiles(providers$Stamen.Toner)
m %>% addProviderTiles(providers$Stamen.TonerLines, 
	options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)

LNHP <- readShapePoints("LNHP", verbose=T,proj4string = CRS("+init=epsg:27700"))
bins <- c(0, 100000, 145000, 200000, 300000, Inf)
pal <- colorBin("Blues", domain = LNHP@data$PURCHASE, bins = bins)

leaflet(LNHP@data) %>% setView(lng = -0.1, lat = 51.5, zoom = 10) %>% 
  addProviderTiles(providers$Stamen.TonerLines, 
	options = providerTileOptions(opacity = 0.35)) %>%
  addProviderTiles(providers$Stamen.TonerLabels) %>%
  addCircles(
	lng = ~LNHP@data$lng, 
	lat = ~LNHP@data$lat, 
	weight = 2,
    fillColor = ~pal(LNHP@data$PURCHASE),
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    radius = ~LNHP@data$PURCHASE * 0.001)%>%
  addLegend(pal = pal, values = ~LNHP@data$PURCHASE, opacity = 0.7, title = NULL,
    position = "bottomright")
