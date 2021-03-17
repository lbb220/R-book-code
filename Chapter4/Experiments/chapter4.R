##########################################
# 第4章 R中基础数据表达与可视化
##4.1 本章所需R函数包
install.packages("ggplot2")
install.packages("lattice")
install.packages("RColorBrewer")
library(ggplot2)
library(lattice)
library(RColorBrewer)

##4.2 基础plot函数
require(maptools)
setwd("E:\\R_course\\Chapter4")
LNHP <- readShapePoints("LNHP", verbose=T,proj4string = CRS("+init=epsg:27700"))

par(mfrow=c(1,2))
plot(LNHP@data[,"FLOORSZ"], LNHP@data[,"PURCHASE"], xlab="房屋面积(平方米)",ylab="房屋价格(英镑)")
plot(LNHP@data[,"FLOORSZ"], LNHP@data[,"PURCHASE"], xlab="房屋面积(平方米)",ylab="房屋价格(英镑)",cex=0.5,pch=3,col="blue")

type <-LNHP@data[,"TYPEDETCH"]+ 2*LNHP@data[,"TPSEMIDTCH"] +3* LNHP@data[,"TYPEBNGLW"] + 4*LNHP@data[,"TYPETRRD"]
idx1 <- which(type==1)
idx2 <- which(type==2)
idx3 <- which(type==3)
idx4 <- which(type==4)
idx5 <- which(type==0)
pchs <- c(3,4,14,17,19)
cols <- c("black","blue","red","green","purple") 
plot(LNHP@data[idx1,"FLOORSZ"], LNHP@data[idx1,"PURCHASE"], col=cols[1],pch=pchs[1],xlim=range(LNHP@data[,"FLOORSZ"]),ylim=range(LNHP@data[,"PURCHASE"]),xlab="房屋面积(平方米)",ylab="房屋价格(英镑)",cex=0.5)
points(LNHP@data[idx2,"FLOORSZ"], LNHP@data[idx2,"PURCHASE"], col=cols[2],pch=pchs[2],cex=0.8)
points(LNHP@data[idx3,"FLOORSZ"], LNHP@data[idx3,"PURCHASE"], col=cols[3],pch=pchs[3],cex=0.8)
points(LNHP@data[idx4,"FLOORSZ"], LNHP@data[idx4,"PURCHASE"], col=cols[4],pch=pchs[4],cex=0.8)
points(LNHP@data[idx5,"FLOORSZ"], LNHP@data[idx5,"PURCHASE"], col=cols[5],pch=pchs[5],cex=0.8)
legend("topleft",legend=c("独栋","联排","排屋","公寓","其他"), col=cols,pch=pchs)

grid(nx=20,ny=20,col="grey")
grid(nx=5,ny=5,col="grey",lty=1,lwd=1)

par(mfrow=c(1,2))
x <- seq(0, 2*pi, len=100)
y<- sin(x)+rnorm(100, 0, 0.1)
plot(x,y,type="l", col="darkblue", lwd=3)
plot(x,y,type="b", col="darkblue",pch=16)

plot(x,sin(x), type="l", col="darkblue", lwd=5)
points(x,y,col="darkred", pch=16)

##4.3 基础统计可视化


qplot(FLOORSZ, PURCHASE, data=LNHP@data)


LNHP@data[as.logical(LNHP@data[,"TYPEDETCH"]),"ptype"] <- "独栋"
LNHP@data[as.logical(LNHP@data[,"TPSEMIDTCH"]),"ptype"] <- "联排"
LNHP@data[as.logical(LNHP@data[,"TYPEBNGLW"]),"ptype"] <- "排屋"
LNHP@data[as.logical(LNHP@data[,"TYPETRRD"]),"ptype"] <- "公寓"
LNHP@data[is.na(LNHP@data[,"ptype"]),"ptype"] <- "其他"
p <- ggplot(LNHP@data, aes(x=FLOORSZ, y=PURCHASE,colour=ptype,  shape=ptype))
p + geom_point()

p <- ggplot(LNHP@data, aes(x=FLOORSZ, y=PURCHASE))
p + geom_point(colour="red", shape=15) +geom_smooth(method=lm,se=F) 

p <- ggplot(LNHP@data, aes(x=FLOORSZ, y=PURCHASE))
p + geom_point(colour="green",shape=3) +geom_smooth(method = 'loess')

x <- seq(0, 2*pi, len=100)
y<- sin(x)+rnorm(100, 0, 0.1)
qplot(x, y, geom="line")
qplot(x, y, geom=c("line", "point"))


x <- 1:100
log.1 <- log(x)
log.2 <- 2*log(x) 
log.3 <- 3*log(x)
log.df <- rbind(data.frame(values=log.1, types="log.1"),data.frame(values=log.2, types="log.2"),data.frame(values=log.3, types="log.3"))
p <- ggplot(log.df, aes(c(x,x,x),y=values, linetype=types, colour=types))
p + geom_line(size=2)

par(mfrow=c(1,2))
barplot(table(LNHP@data[,"PURCHASE"]))
barplot(table(LNHP@data[,"ptype"]))

p <- ggplot(LNHP@data, aes(x=ptype, y=PURCHASE))
p + geom_bar(stat="identity", colour="black") 


boxplot(PURCHASE~ptype, data=LNHP@data)
qplot(ptype,PURCHASE, data=LNHP@data, geom="boxplot")
ggplot(LNHP@data, aes(x=ptype, y=PURCHASE)) + geom_boxplot(colour="red")

##4.4 多元数据可视化

histogram(~ PURCHASE| factor(ptype), data = LNHP@data)

dengraph <- densityplot(~PURCHASE|ptype,data = LNHP@data)
plot(dengraph)
update(dengraph,col = "red", lwd = 2)

attach(diamonds)
weight<- equal.count(diamonds$carat,number = 4,overlap = 0)
xyplot(price~ table|weight, data = diamonds, main = "Diamonds Price vs Diamonds Table by Weight",xlab = "Diamonds Table",ylab = "Diamonds Price",layout = c(4,1),aspect = 1.5)

mypanel <- function(x,y){
  panel.xyplot(x,y,pch = 23)
  panel.grid(h = -1, v = -1)
  panel.lmline(x,y,col = "red", lwd = 1,lty = 2)
}
xyplot(price~ table|weight, data = diamonds, panel=mypanel,main = "Diamonds Price vs Diamonds Table by Weight",xlab = "Diamonds Table",ylab = "Diamonds Price",layout = c(4,1),aspect = 1.5)
 
densityplot(~PURCHASE, data=LNHP@data, group=ptype, auto.key=TRUE)


