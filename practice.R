library(ggplot2)
library(dplyr)
data = read.csv("welfare02.csv")

#1
data$income = ifelse(data$income %in% c(9999.00,0.00), NA, data$income)
data$age = 2021-data$birth+1
data = data %>% 
  mutate(age_group= ifelse(age >= 60, "old", 
                           ifelse(age<30, "young","middle")))
data

mean_income = data %>% 
  group_by(age_group) %>% 
  summarise(mean_income = mean(income, na.rm=T))

ggplot(data=mean_income, aes(x=age_group, y=mean_income))+geom_col()
data

#2
cr = c(1,2,3,4,5,6,7)
rn = c("서울","광주/전남/전북/제주도","수도권(인천/경기)",
       "부산/경남/울산","대구/경북","대전/충남","강원/충북")

region_df = data.frame(code_region = cr, region_name=rn)

merged_df = left_join(data, region_df, by="code_region")
merged_df = merged_df %>% 
  group_by(code_region) %>% 
  summarise(mean_inc = mean(income, na.rm=T))

ggplot(data=merged_df, aes(x=mean_inc, y=region_name))+geom_col()

#3
x = c(7,12,9,15,NA,8,14,2,9,NA,8)
(7+12+9+15+8+14+2+9+8) / 9

replacena = function(x){
 sum = 0
 k=0
  for(i in x){
     if(!is.na(i)){
       k = k+1
       sum = sum + i
     }
  }
 avg = sum/k
 res = c()
 for(i in x){
   if(!is.na(i)){
     res = c(res, i)
   }else{
     res = c(res, avg)
   }
 }
 return(res)
}

result = replacena(x)
result

#4
m_gender = rep("M",15)
f_gender = rep("F",15)
m_score = c(57,78,42,44,91,65,63,60,97,85,92,42,86,81,64)
f_score = c(73,96,74,55,91,50,46,82,43,79,79,50,46,81,83)

male_df = data.frame(gender = m_gender, score=m_score)
female_df = data.frame(gender = f_gender ,score=f_score)

union_df = bind_rows(male_df,female_df)
union_df
ggplot(data=union_df, aes(x=gender,y=score, group=gender))+geom_boxplot()
