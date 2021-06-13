library(dplyr)
library(ggplot2)
data = read.csv("welfare02.csv")

#1
str(data)
data$age = 2021-data$birth+1
data$income = ifelse(data$income == 9999,0,data$income)
data = data %>% 
  mutate(age_group = ifelse(age>=60,"노년(old)",
                            ifelse(age<30,"초년(young)",
                                   "중년(middle)")))

ggplot(data=data, aes(x=age_group,y=income)) + geom_col()

#2
region_name = c("서울", "광주/전남/전북/제주도",
                "수도권(인천/경기)",
                "부산/경남/울산",
                "대구/경북",
                "대전/충남",
                "강원/충북")
code_region = c(1,2,3,4,5,6,7)
d = data.frame(region_name = region_name, code_region=code_region)

rf_data = merge(data,d,by="code_region")
rf_summary = rf_data %>% 
  group_by(region_name) %>% 
  summarise(mean_income = mean(income))

ggplot(data=rf_summary, aes(x=mean_income, y=region_name))+geom_col()

#3
replace_na = function(x){
  m = mean(x, na.rm=T)
  x = ifelse(is.na(x), m, x)
  return(x)
}
x = c(7,12,9,15,NA,8,14,2,9,NA,8)
x
replace_na(x)

#4
male_score = c(57, 78, 42, 44, 91, 65, 63, 60, 97, 85,92,42, 86, 81, 64)
female_score = c(73, 96, 74, 55, 91, 50, 46, 82, 43, 79, 79, 50, 46, 81, 83)
m = rep("M",15)
f = rep("F",15)

d1 = data.frame(gender=m,score=male_score)
d2 = data.frame(gender=f,score=female_score)

total = bind_rows(d1,d2)
total

#4-(1)
ggplot(data=total, aes(group=gender,x=gender,y=score)) + geom_boxplot()

#4-(2)
#남학생의 1사분위수가 여학생보다 높고 평균도 높고, 3사분위수도 높다.
#하지만 중위수는 여학생이 남학생보다 높다.

#ctrl + shift + +
#ctrl + -