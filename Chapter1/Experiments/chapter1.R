##########################################
# 第1章 R语言基础
##1.1 R语言准备
install.packages("GWmodel")
library(GWmodel)

##1.2 数据类型
###1.2.1 基础数据类型
class(TRUE)
class(32.6)
class(2L)
class('a')
class("a")
class(3+2i)
class(charToRaw("a"))

###1.2.2 结构体对象数据类型
V <- c(1,2,7,3,6,8,1,2,9)
V
class(V)
length(V)
V[3]
L <- list(V)
class(L)
L[[3]]
V[3] <- '7'
V
M <- matrix(V, nrow=3)
M
M[3,1]
Fa <- factor(V)
Fa
class(F)
F[3]
DF <- data.frame(as.numeric(V), V)
DF
class(DF[,1])
class(DF[,2])

## 1.3 变量及运算符号
### 1.3.1 变量
ls()
print(V)
cat("The 3rd element of variable V is: ", V[3], "\n")
c(1,2,3,4) ->V
cat("The 3rd element of variable V now is: ", V[3], "\n")

### 1.3.2 运算符号
2 + 3
2 * 3
2 / 3
2 - 3
2 + 3 * 4
2 + (3 * 4)
2^2
2^0.5
v <- c( 2,5.5,6)
s <- c(8, 3, 4)
v^s
v%%s
v%/%s
v <- c(2,5.5,6,9)
s <- c(8,2.5,14,9)
v>s
v>s
v>=s
v<=s
v==s
v!=s
(v>=s)&(v==s)
(v>s)|(v==s)
v <- 1:5
5 %in% v
v*t(v)
v%*%t(v)

## 1.4 R语言基础编程语法
### 1.4.1 判断体
x <- 30L
if(is.integer(x)) {
  print("X is an Integer")
}

x <- c("what","is","truth")

if("Truth" %in% x) {
  print("Truth is found")
} else {
  print("Truth is not found")
}

if("Truth" %in% x) {
  print("Truth is found the first time")
} else if ("truth" %in% x) {
  print("truth is found the second time")
} else {
  print("No truth found")
}

x <- switch(
  3,
  "first",
  "second",
  "third",
  "fourth"
)
print(x)

### 1.4.2 循环体
v <- c("Hello","loop")
cnt <- 2

repeat {
  print(v)
  cnt <- cnt+1
  
  if(cnt > 5) {
    break
  }
}

v <- c("Hello","while loop")
cnt <- 2

while (cnt < 7) {
  print(v)
  cnt = cnt + 1
}

v <- LETTERS[1:4]
for ( i in v) {
  print(i)
}

### 1.4.3 函数
new.function1 <- function() {
  for(i in 1:5) {
    print(i^2)
  }
}	
new.function1()

new.function2 <- function(a,b,c) {
  result <- a * b + c
  print(result)
}
new.function1(5,3,11)
new.function1(a = 11, b = 5, c = 3)

new.function2 <- function(a = 3, b = 6) {
  result <- a * b
  print(result)
}
new.function2()
new.function2(5)
new.function2(b=5)
