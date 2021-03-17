##########################################
# 第2章 R语言基础数据处理
##2.1 本章所需R函数包
install.packages("readr")
install.packages("readlx")
install.packages("dplyr")
install.packages("tidyr")
install.packages("rlist")
install.packages("lubridate")
library(readr)
library(readlx)
library(dplyr)
library(tidyr)
library(rlist)
library(lubridate)

##2.2 基础数据读写
setwd("E:\\R_course\\Chapter2\\Data")
getwd()

###2.2.1 基础数据读入
read_csv("x,y\n1,2\n3,4")
a <- read_csv("x,y\n1,2\n3,4", col_types = list(col_double(), col_character()))
b <- read_csv("x\n1\n2\nb", col_types = list(col_double())) 
b 
problems(b)

cp <-read_delim("comp.csv", ",")
cp.csv <- read_csv("comp.csv")
summary(cp.csv)
spec(cp.csv)
cp.xl <- read_excel("comp.xlsx")
summary(cp.xl)

system.time(read_csv("data.csv"))
system.time(read.csv("data.csv"))

###2.2.2 基础数据写出
df <- data.frame(x = c(1, NA, 2, 3, NA))
write_delim(df, "df.txt", na = "*",delim=",")
write_csv(cp.csv,"comp_w.csv")

##2.3 基础数据文件存储
history(5)
setwd("E:\\R_course\\Chapter2")
save.image(".RData")
savehistory(".Rhistory")
ls()
rm(x)
ls()
rm(list=ls())
ls()
load(".RData")
loadhistory(".Rhistory")
ls()
save(cp, y, file="objectlist.R")
rm(list=ls())
ls()
load("objectlist.R")
ls()

##2.4 基础数据操作与处理
###2.4.1 基础数据提取
cp[2,3]
cp[2,'Dame']
select(cp,Dame)
filter(cp,Dame==0)

person <- 
      list(
        p1=list(name="Ken",age=24,
          interest=c("reading","music","movies"),
          lang=list(r=2,csharp=4,python=3)),
        p2=list(name="James",age=25,
          interest=c("sports","music"),
          lang=list(r=3,java=2,cpp=5)),
        p3=list(name="Penny",age=24,
          interest=c("movies","reading"),
          lang=list(r=1,cpp=4,python=2)))
str(person)
list.map(person, age)
list.map(person, names(lang))
p.age25 <- list.filter(person, age >= 25)
str(p.age25)
p.py3 <- list.filter(person, lang$python >= 3)
str(p.py3)

dateString <- c('20131113','120315','12/17/1996','09-01-01','2015 12 23','2009-1, 5','Created on 2013 4 6')
date <- parse_date_time(date,order = c('ymd','mdy','dmy','ymd'))
wday(date[1])
wday(date, label = TRUE)
month(date)

###2.4.2 基础数据整理
arrange(cp, Dame)
arrange(cp, desc(Dame))

str(list.sort(person, age))
str(list.sort(person, desc(lang$r)))

widedata <- data.frame(person=c('Alex','Bob','Cathy'),grade=c(2,3,4),score=c(78,89,88),age=c(18,19,18))
widedata 
longdata <- gather(widedata, variable, value,-person)
longdata
widedata2 <- spread(longdata, variable, value)
widedata2

wideunite<-unite(widedata, information, person, grade, score, age, sep= "-")
wideunite
widesep <- separate(wideunite, information,c("person","grade","score","age"), sep = "-")
widesep

begin1 <- ymd_hms("20150903, 12:00:00")
end1 <- ymd_hms("20160804, 12:30:00")
begin2 <- ymd_hms("20151203, 12:00:00")
end2 <- ymd_hms("20160904, 12:30:00")
date_1 <- interval(begin1, end1)
date_1
date_2 <- interval(begin2, end2)
date_2
int_overlaps(date_1, date_2)

###2.4.3 管道操作
wideunite <- widedata %>% unite(information, person, grade, score, age, sep= "-")
wideunite
widesep <- wideunite %>% separate(information,c("person","grade","score","age"), sep = "-")
widesep

cp %>% select(starts_with("D")) %>% `*`(2) %>% unlist() %>% matrix(nrow=2) %>% colMeans() %>% plot()

plot(colMeans(matrix(unlist(2*select(cp,starts_with("D"))), nrow=2)))

D <- select(cp,starts_with("D"))
v <- unlist(D*2)
m <- matrix(v,nrow=2)
plot(colMeans(m))




