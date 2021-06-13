#60152572 유정민 week3 HW

#part1
welfare = read.csv("welfare.csv")
str(welfare)
#Q1
welfare$gender = ifelse(welfare$gender == 1, "male", "female")

#Q2
welfare$income = ifelse(welfare$income < 1 | welfare$income > 9998, NA, welfare$income)

#Q3
gender_income = welfare %>% group_by(gender) %>% 
  summarise(mean_income = mean(!is.na(income)))

#Q4
ggplot(data = gender_income, aes(x=gender, y=mean_income)) + geom_bar(stat = "identity")


#part2
#Q1
welfare$gender = ifelse(welfare$gender == 1, "male", "female")

#Q2
welfare$income = ifelse(welfare$income < 1 | welfare$income > 9998, NA, welfare$income)

#Q3
welfare
welfare$age = 2021-welfare$birth
age_gender_income = welfare %>% group_by(gender, age) %>% 
  summarise(mean_income = mean(!is.na(income)))

#Q4
ggplot(data = age_gender_income, aes(x=age, y = mean_income,  col=gender)) + geom_line()

