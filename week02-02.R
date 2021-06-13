dim(midwest)
str(midwest)

#1
midwest$ratio = midwest$popasian/midwest$poptotal
x = mean(midwest$ratio)
midwest$grade = ifelse(midwest$ratio >= x, "large", "small")
table(midwest$grade)
qplot(midwest$grade)

#2
library(dplyr)
midwest_new = midwest %>% 
  arrange(desc(midwest$ratio)) %>% 
  select(county, ratio) %>% head(10)

midwest_new

#3
write.csv(midwest_new, "asain_midwest.csv")

#--------------------------------------------------------------------------

#https://vincentarelbundock.github.io/Rdatasets/datasets.html
myData = read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/CASchools.csv")
str(myData)

#read + math 평균
#평균의 평균
myData$mymean = (myData$read + myData$math) / 2
CA_mean = mean(myData$mymean)

#상위 10개
myData %>% 
  filter(mymean > CA_mean) %>%
  arrange(desc(mymean)) %>% 
  head(10) %>% 
  select(county, school, mymean)

#하위 10개
myData %>% 
  filter(mymean < CA_mean) %>%
  arrange(desc(mymean)) %>% 
  head(10) %>% 
  select(county, school, mymean)



