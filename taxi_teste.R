install.packages("leaflet")
install.packages("ggplot2")
install.packages("Rmisc")
install.packages("rgl")
install.packages("plot3D")
install.packages("caTools")
install.packages("dplyr")
install.packages("geosphere")

library(leaflet)
library(ggplot2)
library(Rmisc)
library(rgl)
library(plot3D)
library(caTools)
library(dplyr)
library(geosphere)

taxi = read.csv("train.csv")
split = sample.split(taxi$trip_duration, SplitRatio=0.6)
train = subset(taxi, split == TRUE)
test = subset(taxi, split == FALSE)
train$pickup_hour <- hour(train$pickup_datetime)
train$pickup_week <- week(train$pickup_datetime)
train$pickup_month <- month(train$pickup_datetime)

train$pickup_datetime = as.Date(train$pickup_datetime )
train$dropoff_datetime = as.Date(train$dropoff_datetime )
train$pickup_weekdays <- weekdays(train$pickup_datetime)
train$pickup_weekend <- ifelse(train$pickup_weekdays==1 | train$pickup_weekdays==7,"Weekend","not-Weekend")


train$distance_km <- distHaversine(
 matrix(c(train$pickup_longitude, train$pickup_latitude),ncol=2),
 matrix(c(train$dropoff_longitude, train$dropoff_latitude),ncol=2)
 )/1000
train1$speed <- (train1$distance_km)/(train1$trip_duration/3600)

train1 = filter(train, vendor_id == 1)
train2 = filter(train, vendor_id == 2)

train1 <- filter(train1, distance_km < 200000)
train1$distance_km = train1$distance_km / 1000
train1$speed <- (train1$distance_km)/(train1$trip_duration/3600)
train1$trip_duration_hours = train1$trip_duration / 3600
train1 <- filter(train1, trip_duration_hours < 8)

vF <- ggplot(data=train1, aes(x=trip_duration_hours, y=distance_km)) + geom_point() + stat_smooth(method="lm", col = "red") + labs(title="Duração X Distância") + coord_cartesian(xlim= c(0,16), ylim=c(0,250))
vFG <- ggplot(data=train1, aes(x=trip_duration_hours, y=distance_km)) + geom_point() + stat_smooth(method="lm", col = "red") + labs(title="Duração X Distância") + coord_cartesian(xlim= c(0,1200), ylim=c(0,1200))
vI <- ggplot(data=train, aes(x=trip_duration_hours, y=distance_km)) + geom_point() + stat_smooth(method="lm", col = "red") + labs(title="Duração X Distância") + coord_cartesian(xlim= c(0,16), ylim=c(0,250))
vIG <- ggplot(data=train, aes(x=trip_duration_hours, y=distance_km)) + geom_point() + stat_smooth(method="lm", col = "red") + labs(title="Duração X Distância") + coord_cartesian(xlim= c(0,1200), ylim=c(0,1200))

multiplot(vI,vIG,vF,vFG,cols = 2)
open3d() 

 plot3d(train1$trip_duration_hours, train1$distance_km, train1$speed,xlab="Duracao (Horas)", ylab="Distancia (KM)", zlab="Velocidade(KM/H)")
 leaflet(data=sample_n(train1, 8e3)) %>% addProviderTiles("Esri.NatGeoWorldMap") %>% addCircleMarkers(~pickup_longitude,~pickup_latitude, radius=1, color="blue", fillOpacity = 0.3)

