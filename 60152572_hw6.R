library(ggplot2)
library(ggmap)
library(openxlsx)
library(dplyr)
library(stringr)

#대기정보 엑셀 불러오기
df = read.xlsx(file.choose(), sheet=1, startRow=2)
str(df)

#도시이름 정제
city = c()
for(i in 2:ncol(df)-1){
  city = c(city,df[,i][1])
}
city = city[-1]
city

#시간평균 정제
time_mean = c()
for(i in 2:ncol(df)-1){
  time_mean = c(time_mean,df[,i][2])
}
time_mean
time_mean = str_trim(time_mean)
time_mean = time_mean[-1]
time_mean = as.numeric(time_mean)
time_mean

#위도 경도 불러오기
register_google(key = "AIzaSyB0VT3DiYbWzNqMxnHlNZInD6kgfyiP9CE")
lon = c()
lat = c()
for(i in 1:length(city)){
  geo = geocode(enc2utf8(city[i]))
  lon = c(lon,geo[1])
  lat = c(lat,geo[2])
}
lon = as.data.frame(lon)
refined_lon = c()
for(i in 1:length(lon)){
  refined_lon = c(refined_lon,lon[1,i])
}
refined_lon

lat = as.data.frame(lat)
refined_lat = c()
for(i in 1:length(lat)){
  refined_lat = c(refined_lat,lat[1,i])
}
refined_lat

#위의 벡터를 기준으로 정제된 데이터프레임 생성
refined_df = data.frame(city=city,pm=time_mean,lon=refined_lon,lat=refined_lat)
refined_df

#Question 1
str(refined_df)
ggplot(data=refined_df, aes(x=reorder(city, -pm),y=pm)) + geom_col()

#Question 2
cen = c((max(refined_lon)+min(refined_lon))/2
        ,((max(refined_lat)+min(refined_lat))/2))

map = get_googlemap(center=cen, zoom=7)
gmap = ggmap(map)
gmap
gmap + geom_point(data=refined_df,
                  aes(x=lon,y=lat),
                  color="red",
                  size=refined_df$pm,
                  alpha=0.5)