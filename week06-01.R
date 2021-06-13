install.packages("ggplot2")
library(ggplot2)

ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_point()
ggplot(data=mpg, aes(x=drv, y=hwy)) + geom_boxplot()

graph_base = ggplot(data=mpg, aes(x=drv, y=hwy))
graph_base+geom_point()
graph_base+geom_boxplot()

help("economics")

ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line(col="violet")

#--------------
data(iris)
dim(iris)
head(iris)
qplot(Sepal.Length, Petal.Length, data=iris, color=Species, size=Petal.Width)

#--------------
data(Orange)
View(Orange)
help(Orange)
qplot(age, circumference, data=Orange, geom="line",
      colour=Tree,
      main="How does orange tree circumference vary with age?")

#--------------google
install.packages("ggmap")
install.packages("openxlsx")
library(ggmap)
library(openxlsx)

register_google(key = "AIzaSyB0VT3DiYbWzNqMxnHlNZInD6kgfyiP9CE")

map = get_googlemap(center=c(126.975684, 37.572752),maptype = "roadmap",
                    zoom = 17,
                    size=c(320,320))
ggmap(map, extent = "device")


gc = geocode(enc2utf8("호미곶"))
gc

lonlat = c(gc$lon, gc$lat)
lonlat

map = get_googlemap(center=lonlat)
ggmap(map)


register_google(key = "AIzaSyB0VT3DiYbWzNqMxnHlNZInD6kgfyiP9CE")
df = read.xlsx(file.choose(), sheet=1, startRow=4)
head(df)
df[,6] = gsub("N", "", df[,6])
df[,7] = gsub("E", "", df[,7])
df2 = data.frame(lon=df[,7], lat=df[,6], mag=df[,3])
str(df2)
df2[,1] = as.numeric(as.character(df2[,1]))
df2[,2] = as.numeric(as.character(df2[,2]))
df2[,3] = as.numeric(as.character(df2[,3]))
str(df2)

cen = c((max(df2$lon) + min(df2$lon))/2,
        (max(df2$lat) + min(df2$lat))/2)
map = get_googlemap(center=cen, zoom=6)
gmap = ggmap(map)
gmap + geom_point(data=df2,
                  aes(x=lon,y=lat),
                  color="red",
                  size=df2$mag,
                  alpha=0.5)
