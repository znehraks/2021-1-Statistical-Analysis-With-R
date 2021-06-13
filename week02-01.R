no = c(1,2,3,4)
name = c("Apple","Banana","Peach","Berry")
price = c(500,200,300,400)
qty = c(5,2,7,9)
fruit = data.frame(N0=no, Name=name, Price=price, Quantity=qty)
fruit

getwd()
save(fruit, file = "test.dat")

rm(fruit)
load("test.dat")
fruit

getwd()

score = read.csv("conciergeResult.csv")
score

write.csv(fruit, "fruit.csv")


b = scan("birth.txt", what="")
b
c=read.table("birth.txt", header = T)
c

vec1 = c(1,2,3)
vec2 = c(4,5,6)
mat = rbind(vec1, vec2)
mat
save(mat, file = "testmat.txt")
dfile = load("testmat.txt")
dfile
mat
x = read.csv("a.csv", stringsAsFactors = FALSE)
str(x)
x = read.csv("b.csv", header=FALSE)
x
names(x) = c("id","name","score")
x

x=read.csv("c.csv", na.strings = c("nil"))
x
str(x)

x = 1:5
y = 6:10
save(x,y, file="xy.RData")
rm(list=ls())
x
y
load("xy.RData")
x
y
library(dplyr)
x = read.csv("csv_exam.csv")
x %>% filter(class == 1)
x %>% filter(class != 1)
x %>% filter(math > 50 & class == 1)
x %>% filter(math > 90 | english > 90)
x %>% filter(class %in% c(1,3,5))
x %>% select(class, english)
x %>% select(-english)
x %>% filter(class == 1) %>% select(class,english) 
x %>% arrange(class, desc(math))

x %>% arrange(class,desc(math))
x %>% arrange(desc(math)) %>% head(3)

test = x %>% filter(class == 1) %>%  arrange(class,math) %>% head()
test$id
