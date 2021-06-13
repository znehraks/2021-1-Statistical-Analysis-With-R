library(dplyr)
exam = read.csv("csv_exam.csv")
exam
exam %>% mutate(
  total = math+english+science,
  mean = (math+english+science)/3,
  test = ifelse(science >= 60, "pass", "fail")
) %>% arrange(desc(total)) %>% head(5)


exam %>% 
  group_by(class) %>% 
  summarise(mean_math = mean(math),
            sum_math=sum(math),
            median_math = median(math),
            n=n()
  )

library(ggplot2)
mpg %>% group_by(manufacturer, drv) %>% summarise(mean_cty=mean(cty)) %>% head(10)


name = data.frame(class=c(1,2,3,4,5), teacher=c("kim", "lee", "park", "choi", "jung"))
name
exam_new = left_join(exam, name, by="class")
exam_new

#실습 MPG 데이터 분석
head(mpg)
#Q1
mpg %>% group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  filter(class %in% c("compact", "suv"))

#Q2
mpg %>% filter(manufacturer == "audi") %>% 
  arrange(desc(hwy)) %>% 
  head(5)

#Q3
mpg %>% filter(class=="compact") %>% 
  group_by(manufacturer) %>% 
  summarise(n = n()) %>% 
  head(1)

library("tidyr")

df = tibble(
  year = c(2010,2010,2010,2010,2012,2012,2012),
  qtr = c(1,2,3,4,1,2,3),
  revenue=c(10,20,30,40,NA,60,70)
)
df
df %>% complete(year=full_seq(year,1),qtr)
