#60152572_q_0311
mpg

#Q1
mpg_under_4 = mpg %>% filter(mpg$displ <= 4)
mpg_under_4_hwymean = mean(mpg_under_4$hwy)
mpg_under_4_hwymean

mpg_over_5 = mpg %>% filter(mpg$displ >= 5)
mpg_over_5_hwymean = mean(mpg_over_5$hwy)
mpg_over_5_hwymean

ifelse(mpg_under_4_hwymean > mpg_over_5_hwymean, 
       "배기량 4이하가 연비가 더 높음", "배기량 5이상이 연비가 더 높음")

#Q2
#미국 해당 자동차 연비 평균
mpg_USA = mpg %>% filter(mpg$manufacturer %in% c("chevrolet", "dodge", "ford", "lincoln"))
mpg_USA$ymean = (mpg_USA$cty + mpg_USA$hwy)/2
mean_mpg_USA = mean(mpg_USA$ymean)
mean_mpg_USA

#일본 해당 자동차 연비 평균
mpg_JP = mpg %>% filter(mpg$manufacturer %in% c("honda", "nissan", "subaru", "toyota"))
mpg_JP$ymean = (mpg_JP$cty + mpg_JP$hwy)/2
mean_mpg_JP = mean(mpg_JP$ymean)
mean_mpg_JP

#연비가 더 높은 나라
ifelse(mean_mpg_USA > mean_mpg_JP, 
       "미국이 더 높다", "일본이 더 높다")

#Q3
#도로 유형을 통틀어 가장 연비가 높은 자동차 모델
mpg %>% 
  group_by(mpg$manufacturer) %>% 
  summarise(mean_total = mean((cty + hwy)/2)) %>% 
  arrange(desc(mean_total)) %>% 
  head(1)

