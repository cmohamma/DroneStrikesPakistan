drone<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/dronestrikesdata.csv")
fata<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/fata_tehsils", "FATA_Tehsils")
install.packages("lubridate")
library(lubridate)
drone$Date<-as.Date(drone$Date,  "%d/%m/%Y")

library(sp)
library(spatialEco)
drone$year<-year(drone$Date)
drone$month<-month(drone$Date)
drone$y<-drone$Latitude
drone$x<-drone$Longitude
coordinates(drone) <- ~ x + y
proj4string(drone) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
drone.fata<-point.in.poly(drone, fata)


library(rgdal)
pak<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/Pak_adm2_pco_20110324_2", "Pak_adm2_pco_20110324")
fata<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/fata_tehsils", "FATA_Tehsils")
terror<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/terror.csv")
library(dplyr)
terror<-filter(terror, terror$country_txt=="Pakistan")
terror$country_txt<-NULL
terror$country<-NULL
terror$region_txt<-NULL
terror<-filter(terror, iyear>="2004")
terror<-filter(terror, provstate=="Federally Administered Tribal Areas")
terror$provstate<-NULL
terror$summary<-NULL
terror$alternative_txt<-NULL
terror$location<-NULL
write.csv(terror, "/users/cyrusmohammadian/desktop/tr.csv")

terror<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")
terror$y<-terror$latitude
terror$x<-terror$longitude
coordinates(terror) <- ~ x + y
proj4string(terror) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
terror.fata<-point.in.poly(terror, fata)

#____week
terror.wk<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")
terror.wk$day[terror.wk$day==0]<-1
terror.wk$date<-paste(terror.wk$year, terror.wk$month, terror.wk$day, sep="-")
terror.wk$date<-as.Date(terror.wk$date)
terror.wk$date<-ymd(terror.wk$date)
terror.wk$week<-week(terror.wk$date)

terror.wk$y<-terror.wk$latitude
terror.wk$x<-terror.wk$longitude
coordinates(terror.wk) <- ~ x + y
proj4string(terror.wk) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
terror.fata.wk<-point.in.poly(terror.wk, fata)
#____month
terror.month<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")
terror.month$day[terror.month$day==0]<-1
terror.month$date<-paste(terror.month$year, terror.month$month, terror.month$day, sep="-")
terror.month$date<-as.Date(terror.month$date)
terror.month$date<-ymd(terror.month$date)
terror.month$month<-month(terror.month$date)

terror.month$y<-terror.month$latitude
terror.month$x<-terror.month$longitude
coordinates(terror.month) <- ~ x + y
proj4string(terror.month) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
terror.fata.month<-point.in.poly(terror.month, fata)

#----Create dataframe in agency-week and agency-month
fata<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/fata_tehsils", "FATA_Tehsils")
library(ggplot2)
drone<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/dronestrikesdata.csv")
terror<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")

library(plyr)
library(maptools)
fata@data$id = rownames(fata@data)
fata.points = fortify(fata, region="id")
fata.df = join(fata.points, fata@data, by="id")
detach("package:plyr", unload=F)

drone<-as.data.frame(drone)
ggplot(fata)+geom_polygon(aes(x=long, y=lat, group=id))+
 geom_point(data=terror, aes(x=longitude,y=latitude, color="terror")) + 
 geom_point(data=drone, aes(x=Longitude, y=Latitude, color="drone")) +
 coord_equal()

fata@data$group<-fata@data$OBJECTID
fata.test<-fata@data
fata.test$start<-2004
fata.test$end<-2013 

library(plyr)
fata.test<-ddply(fata.test, .(group), function(x){
  data.frame(
    group=x$group,
    year=seq(x$start, x$end),
    TehsName=x$TehsName,
    DistName=x$DistName
  )
}
)
#______
fata.test.month<-fata.test
fata.test.month$start<-1
fata.test.month$end<-12
fata.test.month$yearname<-paste(fata.test.month$TehsName, fata.test.month$year)

fata.test.month<-ddply(fata.test.month, .(yearname), function(x){
  data.frame(
    group=x$group,
    TehsName=x$TehsName,
    DistName=x$DistName,
    year=x$year,
    month=seq(x$start, x$end)
  )
}
)
#__________
fata.test.week<-fata.test
fata.test.week$start<-1
fata.test.week$end<-53
fata.test.week$yearname<-paste(fata.test.week$TehsName, fata.test.week$year)

library(plyr)
fata.test.week<-ddply(fata.test.week, .(yearname), function(x){
  data.frame(
    group=x$group,
    TehsName=x$TehsName,
    DistName=x$DistName,
    year=x$year,
    week=seq(x$start, x$end)
  )
}
)

fata.test.week$yearname<-NULL
fata.test.month$yearname<-NULL

##Agency-Week#
#terror data
terror.wk<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")
terror.wk$day[terror.wk$day==0]<-1
terror.wk$date<-paste(terror.wk$year, terror.wk$month, terror.wk$day, sep="-")
terror.wk$date<-as.Date(terror.wk$date)
terror.wk$date<-ymd(terror.wk$date)
terror.wk$week<-week(terror.wk$date)

terror.wk$y<-terror.wk$latitude
terror.wk$x<-terror.wk$longitude
coordinates(terror.wk) <- ~ x + y
proj4string(terror.wk) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
terror.fata.wk<-point.in.poly(terror.wk, fata)
terror.fata.wk<-as.data.frame(terror.fata.wk)
terror.fata.wk.terr.kill<-terror.fata.wk %>%
  group_by(TehsName, week, year) %>%
  summarize(terr.kill=sum(nkill))
terror.fata.wk.terr.evnt<-terror.fata.wk %>%
  group_by(TehsName, week, year) %>%
  summarize(terr.evnt=n())
fata.wk.agy<-merge(fata.test.week, terror.fata.wk.terr.kill, 
                   by=c("TehsName", "week", "year"), all.x=TRUE, all.y=FALSE)
fata.wk.agy<-merge(fata.wk.agy, terror.fata.wk.terr.evnt, 
                   by=c("TehsName", "week", "year"), all.x=TRUE, all.y=FALSE)
fata.wk.agy$terr.evnt[is.na(fata.wk.agy$terr.evnt)]<-0
fata.wk.agy$terr.kill[is.na(fata.wk.agy$terr.kill)]<-0
#drone data
drone<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/dronestrikesdata.csv")
drone$date<-paste(drone$year, drone$month, drone$day, sep="-")
drone$date<-as.Date(drone$date)
drone$date<-ymd(drone$date)
drone$week<-week(drone$date)
drone$y<-drone$Latitude
drone$x<-drone$Longitude
coordinates(drone) <- ~ x + y
proj4string(drone) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
drone.fata.wk<-point.in.poly(drone, fata)

drone.dt.wk<-as.data.frame(drone.fata.wk)
fata.drone.count.wk<-drone.dt.wk %>%
  group_by(year, week, TehsName) %>%
  summarize(drone=n())
fata.drone.cvl.wk<-drone.dt.wk %>%
  group_by(year, week, TehsName) %>%
  summarize(dron.cvl.kill=sum(Maximum.Civilians.Reported.Killed))

#merge drone and terror and full data
fata.ivars.wk.agy<-merge(fata.drone.cvl.wk,fata.drone.count.wk, by=c("year", "week", "TehsName"), all=T)

fata.wk.agy.full<-merge(fata.wk.agy,fata.ivars.wk.agy, 
                        by=c("year", "week", "TehsName"), all.x=TRUE,all.y=FALSE)
fata.wk.agy.full$dron.cvl.kill[is.na(fata.wk.agy.full$dron.cvl.kill)]<-0
fata.wk.agy.full$drone[is.na(fata.wk.agy.full$drone)]<-0

fata.wk.agy.full$terr.kill.bin<-ifelse(fata.wk.agy.full$terr.kill>0,1,0)
fata.wk.agy.full$terr.evnt.bin<-ifelse(fata.wk.agy.full$terr.evnt>0,1,0)

fata.wk.agy.full2<-fata.wk.agy.full %>%
  group_by(group) %>%
  mutate(time=1:530)

fata.wk.agy.full2<-fata.wk.agy.full2 %>%
  group_by(group) %>%
  mutate(terr.evnt.bin.lead=lead(terr.evnt.bin,order_by =time)) 

fata.wk.agy.full2<-fata.wk.agy.full2 %>%
  group_by(group) %>%
  mutate(terr.kill.bin.lead=lead(terr.kill.bin,order_by =time)) 

fata.wk.agy.full2<-fata.wk.agy.full2 %>%
  group_by(group) %>%
  mutate(drone.lag1=lag(drone,1,order_by =time)) %>%
  mutate(drone.lag2=lag(drone,2,order_by =time)) %>%
  mutate(drone.lag3=lag(drone,3,order_by =time)) %>%
  mutate(drone.lag4=lag(drone,4,order_by =time)) %>%
  mutate(drone.lag5=lag(drone,5,order_by =time)) %>%
  mutate(drone.lag6=lag(drone,6,order_by =time)) %>%
  mutate(drone.lag7=lag(drone,7,order_by =time)) %>%
  mutate(drone.lag8=lag(drone,8,order_by =time))


library(Zelig)
summary(zelig(terr.evnt.bin.lead ~ terr.evnt.bin + drone + TehsName + 
                as.factor(week), data=fata.wk.agy.full2, model="logit",
              robust=TRUE, cluster="TehsName"))

write.csv(fata.wk.agy.full2, "agency.week.csv")

##Agency-Month
#terror data
terror.mth<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")
terror.mth$day[terror.mth$day==0]<-1
terror.mth$date<-paste(terror.mth$year, terror.mth$month, terror.mth$day, sep="-")
terror.mth$date<-as.Date(terror.mth$date)
terror.mth$date<-ymd(terror.mth$date)
terror.mth$week<-month(terror.mth$date)

terror.mth$y<-terror.mth$latitude
terror.mth$x<-terror.mth$longitude
coordinates(terror.mth) <- ~ x + y
proj4string(terror.mth) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
terror.fata.mth<-point.in.poly(terror.mth, fata)
terror.fata.mth<-as.data.frame(terror.fata.mth)

detach("package:dplyr", unload=TRUE)
library(dplyr)
terror.fata.mth.terr.kill<-terror.fata.mth %>%
  group_by(TehsName, month, year) %>%
  summarize(terr.kill=sum(nkill))

terror.fata.mth.terr.evnt<-terror.fata.wk %>%
  group_by(TehsName, month, year) %>%
  summarize(terr.evnt=n())

fata.mth.agy<-merge(fata.test.month, terror.fata.mth.terr.kill, 
                   by=c("TehsName", "month", "year"), all.x=TRUE, all.y=FALSE)
fata.mth.agy<-merge(fata.mth.agy, terror.fata.mth.terr.evnt, 
                   by=c("TehsName", "month", "year"), all.x=TRUE, all.y=FALSE)
fata.mth.agy$terr.evnt[is.na(fata.mth.agy$terr.evnt)]<-0
fata.mth.agy$terr.kill[is.na(fata.mth.agy$terr.kill)]<-0
#drone data
drone<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/dronestrikesdata.csv")
drone$date<-paste(drone$year, drone$month, drone$day, sep="-")
drone$date<-as.Date(drone$date)
drone$date<-ymd(drone$date)
drone$week<-week(drone$date)
drone$y<-drone$Latitude
drone$x<-drone$Longitude
coordinates(drone) <- ~ x + y
proj4string(drone) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
drone.fata.mth<-point.in.poly(drone, fata)

drone.dt.mth<-as.data.frame(drone.fata.mth)
fata.drone.count.mth<-drone.dt.mth %>%
  group_by(year, month, TehsName) %>%
  summarize(drone=n())
fata.drone.cvl.mth<-drone.dt.mth %>%
  group_by(year, month, TehsName) %>%
  summarize(dron.cvl.kill=sum(Maximum.Civilians.Reported.Killed))
#merge drone and terror and full data
fata.ivars.mth.agy<-merge(fata.drone.cvl.mth,fata.drone.count.mth, by=c("year", "month", "TehsName"), all=T)

fata.mth.agy.full<-merge(fata.mth.agy,fata.ivars.mth.agy, 
                        by=c("year", "month", "TehsName"), all.x=TRUE,all.y=FALSE)
fata.mth.agy.full$dron.cvl.kill[is.na(fata.mth.agy.full$dron.cvl.kill)]<-0
fata.mth.agy.full$drone[is.na(fata.mth.agy.full$drone)]<-0

fata.mth.agy.full$terr.kill.bin<-ifelse(fata.mth.agy.full$terr.kill>0,1,0)
fata.mth.agy.full$terr.evnt.bin<-ifelse(fata.mth.agy.full$terr.evnt>0,1,0)

fata.mth.agy.full2<-fata.mth.agy.full %>%
  group_by(group) %>%
  mutate(time=1:120)

fata.mth.agy.full2<-fata.mth.agy.full2 %>%
  group_by(group) %>%
  mutate(terr.evnt.bin.lead=lead(terr.evnt.bin,order_by =time)) 

fata.mth.agy.full2<-fata.mth.agy.full2 %>%
  group_by(group) %>%
  mutate(terr.kill.bin.lead=lead(terr.kill.bin,order_by =time)) 

fata.mth.agy.full2<-fata.mth.agy.full2 %>%
  group_by(group) %>%
  mutate(drone.lag1=lag(drone,1,order_by =time)) %>%
  mutate(drone.lag2=lag(drone,2,order_by =time)) %>%
  mutate(drone.lag3=lag(drone,3,order_by =time)) %>%
  mutate(drone.lag4=lag(drone,4,order_by =time)) %>%
  mutate(drone.lag5=lag(drone,5,order_by =time)) %>%
  mutate(drone.lag6=lag(drone,6,order_by =time)) %>%
  mutate(drone.lag7=lag(drone,7,order_by =time)) %>%
  mutate(drone.lag8=lag(drone,8,order_by =time))

summary(zelig(terr.evnt.bin.lead ~ terr.evnt.bin + drone + TehsName + 
                as.factor(month), data=fata.mth.agy.full2, model="logit",
              robust=TRUE, cluster="TehsName"))

write.csv(fata.mth.agy.full2, "agency.month.csv")

#District-month
mth.dist<-fata.mth.agy.full2 %>%
  select(year, month, DistName, dron.cvl.kill, drone,drone.lag1, drone.lag2, drone.lag3, 
         drone.lag4, terr.evnt,terr.evnt.bin, terr.evnt.bin.lead, terr.kill, terr.kill.bin,
         terr.kill.bin.lead) %>%
  group_by(year, month, DistName) %>%
  summarize_each(funs(sum))

mth.dist$terr.evnt.bin<-ifelse(mth.dist$terr.evnt.bin>0,1,0)
mth.dist$terr.evnt.bin.lead<-ifelse(mth.dist$terr.evnt.bin.lead>0,1,0)
mth.dist$terr.kill.bin<-ifelse(mth.dist$terr.kill.bin>0,1,0)
mth.dist$terr.kill.bin.lead<-ifelse(mth.dist$terr.kill.bin.lead>0,1,0)
mth.dist$group<-NULL

summary(zelig(terr.evnt.bin.lead ~ terr.evnt.bin + drone + DistName + 
                as.factor(month), data=mth.dist, model="logit",
              robust=TRUE, cluster="DistName"))

#District-week
wk.dist<-fata.wk.agy.full2 %>%
  select(year, week, DistName, dron.cvl.kill, drone,terr.evnt,
         terr.evnt.bin, terr.evnt.bin.lead, terr.kill, terr.kill.bin,
         terr.kill.bin.lead, drone,drone.lag1, drone.lag2, drone.lag3, 
         drone.lag4, drone.lag5, drone.lag6, drone.lag7, drone.lag8) %>%
  group_by(year, week, DistName) %>%
  summarize_each(funs(sum))
wk.dist$group<-NULL

wk.dist$terr.evnt.bin<-ifelse(wk.dist$terr.evnt.bin>0,1,0)
wk.dist$terr.evnt.bin.lead<-ifelse(wk.dist$terr.evnt.bin.lead>0,1,0)

summary(zelig(terr.evnt.bin.lead ~ terr.evnt.bin + drone + DistName + 
                as.factor(week), data=wk.dist, model="logit",
              robust=TRUE, cluster="DistName"))

###Add spatial lag of dv and iv for agency-week

fata.wk.agy.full2<-subset(fata.wk.agy.full2, year>2004)

system.time({
  fata<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/fata_tehsils", "FATA_Tehsils")
  fata_nb<-poly2nb(fata)
  fata_lw <- nb2listw(fata_nb)
  data<-subset(fata.wk.agy.full2, week==1 & year==2005)
  data<-distinct(data, TehsName)
  fata@data$order<-rownames(fata@data)
  fata.df <- as(fata, "data.frame")
  fata.df<-merge(fata.df, data, by="TehsName", all.x=TRUE, all.y=FALSE)
  fata.df$order<-as.numeric(fata.df$order)
  fata.df<-arrange(fata.df, order)
  fata <- SpatialPolygonsDataFrame(fata,data=fata.df, match.ID=F)
  fata$terr.evnt.bin.slag<-lag.listw(fata_lw,fata$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
  fata$drone.slag<-lag.listw(fata_lw,fata$drone,zero.policy=TRUE, NAOK=T)
  fata.df2 <- as(fata, "data.frame")
  fata.df2$week<-1
  fata.df2$year<-2005
  fata.df5<-fata.df2
})

for (i in 2:53) {
  fata<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/fata_tehsils", "FATA_Tehsils")
  fata_nb<-poly2nb(fata)
  fata_lw <- nb2listw(fata_nb)
  data<-subset(fata.wk.agy.full2, week==i & year==2005)
  data<-distinct(data, TehsName)
  fata@data$order<-rownames(fata@data)
  fata.df <- as(fata, "data.frame")
  fata.df<-merge(fata.df, data, by="TehsName", all.x=TRUE, all.y=FALSE)
  fata.df$order<-as.numeric(fata.df$order)
  fata.df<-arrange(fata.df, order)
  fata <- SpatialPolygonsDataFrame(fata,data=fata.df, match.ID=F)
  fata$terr.evnt.bin.slag<-lag.listw(fata_lw,fata$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
  fata$drone.slag<-lag.listw(fata_lw,fata$drone,zero.policy=TRUE, NAOK=T)
  fata.df3 <- as(fata, "data.frame")
  fata.df3$week<-i
  fata.df3$year<-2005
  fata.df2<-rbind(fata.df2,fata.df3)
  
}

fata.df6<-rbind(fata.df5, fata.df2)
fata.df2<-fata.df6

system.time({
  for (i in 1:53) {
    for (j in 2006:2012){
      fata<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/fata_tehsils", "FATA_Tehsils")
      fata_nb<-poly2nb(fata)
      fata_lw <- nb2listw(fata_nb)
      data<-subset(fata.wk.agy.full2, week==i & year==j)
      data<-distinct(data, TehsName)
      fata@data$order<-rownames(fata@data)
      fata.df <- as(fata, "data.frame")
      fata.df<-merge(fata.df, data, by="TehsName", all.x=TRUE, all.y=FALSE)
      fata.df$order<-as.numeric(fata.df$order)
      fata.df<-arrange(fata.df, order)
      fata <- SpatialPolygonsDataFrame(fata,data=fata.df, match.ID=F)
      fata$terr.evnt.bin.slag<-lag.listw(fata_lw,fata$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
      fata$drone.slag<-lag.listw(fata_lw,fata$drone,zero.policy=TRUE, NAOK=T)
      fata.df3 <- as(fata, "data.frame")
      fata.df3$week<-i
      fata.df3$year<-j
      fata.df2<-rbind(fata.df2,fata.df3)
      
    }
  }
})

###Add spatial lag of dv and iv for district-week

wk.dist<-subset(wk.dist, year>2004)

  full<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/full_districts", "Pak_adm3_pco_20110324")
  full@data$DISTRICT<-as.character(full@data$DISTRICT)
  full@data$DISTRICT[full@data$DISTRICT=="Fr Tank"]<-"FR Tank"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Peshawar"]<-"FR Peshawar"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Kohat"]<-"FR Kohat"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Lakki Marwat"]<-"FR Lakki Marwat"
  full@data$DISTRICT[full@data$DISTRICT=="Fr D.i.khan"]<-"FR D.I. Khan"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Bannu"]<-"FR Bannu"
  full@data$DistName<-full@data$DISTRICT
  full@data$DistName<-as.factor(full@data$DISTRICT)
  full_nb<-poly2nb(full)
  full_lw <- nb2listw(full_nb)
  data<-subset(wk.dist, week==1 & year==2005)
  data<-distinct(data, DistName)
  full@data$order<-rownames(full@data)
  full.df <- as(full, "data.frame")
  full.df<-merge(full.df, data, by="DistName", all.x=TRUE, all.y=FALSE)
  full.df$order<-as.numeric(full.df$order)
  full.df<-arrange(full.df, order)
  full <- SpatialPolygonsDataFrame(full,data=full.df, match.ID=F)
  full$terr.evnt.bin[is.na(full$terr.evnt.bin)]<-0
  full$drone[is.na(full$drone)]<-0
  full$drone.lag1[is.na(full$drone.lag1)]<-0
  full$drone.lag2[is.na(full$drone.lag2)]<-0
  full$drone.lag3[is.na(full$drone.lag3)]<-0
  full$drone.lag4[is.na(full$drone.lag4)]<-0
  full$drone.lag5[is.na(full$drone.lag5)]<-0
  full$drone.lag6[is.na(full$drone.lag6)]<-0
  full$drone.lag7[is.na(full$drone.lag7)]<-0
  full$drone.lag8[is.na(full$drone.lag8)]<-0
  full$terr.evnt.bin.slag<-lag.listw(full_lw,full$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
  full$drone.slag<-lag.listw(full_lw,full$drone,zero.policy=TRUE, NAOK=T)
  full$drone.lag1.slag<-lag.listw(full_lw,full$drone.lag1,zero.policy=TRUE, NAOK=T)
  full$drone.lag2.slag<-lag.listw(full_lw,full$drone.lag2,zero.policy=TRUE, NAOK=T)
  full$drone.lag3.slag<-lag.listw(full_lw,full$drone.lag3,zero.policy=TRUE, NAOK=T)
  full$drone.lag4.slag<-lag.listw(full_lw,full$drone.lag4,zero.policy=TRUE, NAOK=T)
  full$drone.lag5.slag<-lag.listw(full_lw,full$drone.lag5,zero.policy=TRUE, NAOK=T)
  full$drone.lag6.slag<-lag.listw(full_lw,full$drone.lag6,zero.policy=TRUE, NAOK=T)
  full$drone.lag7.slag<-lag.listw(full_lw,full$drone.lag7,zero.policy=TRUE, NAOK=T)
  full$drone.lag8.slag<-lag.listw(full_lw,full$drone.lag8,zero.policy=TRUE, NAOK=T)
  full.df2 <- as(full, "data.frame")
  full.df2$week<-1
  full.df2$year<-2005
  full.df5<-full.df2

for (i in 2:53) {
  full<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/full_districts", "Pak_adm3_pco_20110324")
  full@data$DISTRICT<-as.character(full@data$DISTRICT)
  full@data$DISTRICT[full@data$DISTRICT=="Fr Tank"]<-"FR Tank"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Peshawar"]<-"FR Peshawar"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Kohat"]<-"FR Kohat"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Lakki Marwat"]<-"FR Lakki Marwat"
  full@data$DISTRICT[full@data$DISTRICT=="Fr D.i.khan"]<-"FR D.I. Khan"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Bannu"]<-"FR Bannu"
  full@data$DistName<-full@data$DISTRICT
  full@data$DistName<-as.factor(full@data$DISTRICT)
  full_nb<-poly2nb(full)
  full_lw <- nb2listw(full_nb)
  data<-subset(wk.dist, week==i & year==2005)
  data<-distinct(data, DistName)
  full@data$order<-rownames(full@data)
  full.df <- as(full, "data.frame")
  full.df<-merge(full.df, data, by="DistName", all.x=TRUE, all.y=FALSE)
  full.df$order<-as.numeric(full.df$order)
  full.df<-arrange(full.df, order)
  full <- SpatialPolygonsDataFrame(full,data=full.df, match.ID=F)
  full$terr.evnt.bin[is.na(full$terr.evnt.bin)]<-0
  full$drone[is.na(full$drone)]<-0
  full$drone.lag1[is.na(full$drone.lag1)]<-0
  full$drone.lag2[is.na(full$drone.lag2)]<-0
  full$drone.lag3[is.na(full$drone.lag3)]<-0
  full$drone.lag4[is.na(full$drone.lag4)]<-0
  full$drone.lag5[is.na(full$drone.lag5)]<-0
  full$drone.lag6[is.na(full$drone.lag6)]<-0
  full$drone.lag7[is.na(full$drone.lag7)]<-0
  full$drone.lag8[is.na(full$drone.lag8)]<-0
  full$terr.evnt.bin.slag<-lag.listw(full_lw,full$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
  full$drone.slag<-lag.listw(full_lw,full$drone,zero.policy=TRUE, NAOK=T)
  full$drone.lag1.slag<-lag.listw(full_lw,full$drone.lag1,zero.policy=TRUE, NAOK=T)
  full$drone.lag2.slag<-lag.listw(full_lw,full$drone.lag2,zero.policy=TRUE, NAOK=T)
  full$drone.lag3.slag<-lag.listw(full_lw,full$drone.lag3,zero.policy=TRUE, NAOK=T)
  full$drone.lag4.slag<-lag.listw(full_lw,full$drone.lag4,zero.policy=TRUE, NAOK=T)
  full$drone.lag5.slag<-lag.listw(full_lw,full$drone.lag5,zero.policy=TRUE, NAOK=T)
  full$drone.lag6.slag<-lag.listw(full_lw,full$drone.lag6,zero.policy=TRUE, NAOK=T)
  full$drone.lag7.slag<-lag.listw(full_lw,full$drone.lag7,zero.policy=TRUE, NAOK=T)
  full$drone.lag8.slag<-lag.listw(full_lw,full$drone.lag8,zero.policy=TRUE, NAOK=T)
  full.df3 <- as(full, "data.frame")
  full.df3$week<-i
  full.df3$year<-2005
  full.df2<-rbind(full.df2,full.df3)
  
}
  
full.df6<-rbind(full.df5, full.df2)
full.df2<-full.df6

  for (i in 1:53) {
    for (j in 2006:2012){
      full<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/full_districts", "Pak_adm3_pco_20110324")
      full@data$DISTRICT<-as.character(full@data$DISTRICT)
      full@data$DISTRICT[full@data$DISTRICT=="Fr Tank"]<-"FR Tank"
      full@data$DISTRICT[full@data$DISTRICT=="Fr Peshawar"]<-"FR Peshawar"
      full@data$DISTRICT[full@data$DISTRICT=="Fr Kohat"]<-"FR Kohat"
      full@data$DISTRICT[full@data$DISTRICT=="Fr Lakki Marwat"]<-"FR Lakki Marwat"
      full@data$DISTRICT[full@data$DISTRICT=="Fr D.i.khan"]<-"FR D.I. Khan"
      full@data$DISTRICT[full@data$DISTRICT=="Fr Bannu"]<-"FR Bannu"
      full@data$DistName<-full@data$DISTRICT
      full@data$DistName<-as.factor(full@data$DISTRICT)
      full_nb<-poly2nb(full)
      full_lw <- nb2listw(full_nb)
      data<-subset(wk.dist, week==i & year==j)
      data<-distinct(data, DistName)
      full@data$order<-rownames(full@data)
      full.df <- as(full, "data.frame")
      full.df<-merge(full.df, data, by="DistName", all.x=TRUE, all.y=FALSE)
      full.df$order<-as.numeric(full.df$order)
      full.df<-arrange(full.df, order)
      full <- SpatialPolygonsDataFrame(full,data=full.df, match.ID=F)
      full$terr.evnt.bin[is.na(full$terr.evnt.bin)]<-0
      full$drone[is.na(full$drone)]<-0
      full$drone.lag1[is.na(full$drone.lag1)]<-0
      full$drone.lag2[is.na(full$drone.lag2)]<-0
      full$drone.lag3[is.na(full$drone.lag3)]<-0
      full$drone.lag4[is.na(full$drone.lag4)]<-0
      full$drone.lag5[is.na(full$drone.lag5)]<-0
      full$drone.lag6[is.na(full$drone.lag6)]<-0
      full$drone.lag7[is.na(full$drone.lag7)]<-0
      full$drone.lag8[is.na(full$drone.lag8)]<-0
      full$terr.evnt.bin.slag<-lag.listw(full_lw,full$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
      full$drone.slag<-lag.listw(full_lw,full$drone,zero.policy=TRUE, NAOK=T)
      full$drone.lag1.slag<-lag.listw(full_lw,full$drone.lag1,zero.policy=TRUE, NAOK=T)
      full$drone.lag2.slag<-lag.listw(full_lw,full$drone.lag2,zero.policy=TRUE, NAOK=T)
      full$drone.lag3.slag<-lag.listw(full_lw,full$drone.lag3,zero.policy=TRUE, NAOK=T)
      full$drone.lag4.slag<-lag.listw(full_lw,full$drone.lag4,zero.policy=TRUE, NAOK=T)
      full$drone.lag5.slag<-lag.listw(full_lw,full$drone.lag5,zero.policy=TRUE, NAOK=T)
      full$drone.lag6.slag<-lag.listw(full_lw,full$drone.lag6,zero.policy=TRUE, NAOK=T)
      full$drone.lag7.slag<-lag.listw(full_lw,full$drone.lag7,zero.policy=TRUE, NAOK=T)
      full$drone.lag8.slag<-lag.listw(full_lw,full$drone.lag8,zero.policy=TRUE, NAOK=T)
      full.df3 <- as(full, "data.frame")
      full.df3$week<-i
      full.df3$year<-j
      full.df2<-rbind(full.df2,full.df3)
    }
  }

wk.dist.slag2<-filter(full.df2, PROVINCE=="Fata")
wk.dist.slag$drone.slag.bin<-ifelse(wk.dist.slag$drone.slag>1,1,0)

write.csv(wk.dist.slag, "wk.dist.slag.final2.csv")
summary(zelig(terr.evnt.bin.lead ~ terr.evnt.bin + terr.evnt.bin.slag+drone+drone.slag.bin + DistName+as.factor(week), data=wk.dist.slag, model="logit",
              robust=TRUE, cluster="DistName"))

drone.strike <-wk.dist.slag$drone.slag + wk.dist.slag$drone
drone.strike.1 <-wk.dist.slag$drone.lag1.slag + wk.dist.slag$drone.lag1
drone.strike.2 <-wk.dist.slag$drone.lag2.slag + wk.dist.slag$drone.lag2
drone.strike.3 <-wk.dist.slag$drone.lag3.slag + wk.dist.slag$drone.lag3
drone.strike.4 <-wk.dist.slag$drone.lag4.slag + wk.dist.slag$drone.lag4

summary(zelig(terr.evnt.bin.lead ~ terr.evnt.bin + terr.evnt.bin.slag 
              +drone.strike +drone.strike.1+drone.strike.2+
                drone.strike.3+drone.strike.4+DistName+
                as.factor(week), data=wk.dist.slag, 
              model="logit",robust=TRUE, cluster="DistName"))

write.csv(wk.dist.slag, "wk.dist.slag.final.csv")




###Add spatial lag of dv and iv for district-month

full<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/full_districts", "Pak_adm3_pco_20110324")
full@data$DISTRICT<-as.character(full@data$DISTRICT)
full@data$DISTRICT[full@data$DISTRICT=="Fr Tank"]<-"FR Tank"
full@data$DISTRICT[full@data$DISTRICT=="Fr Peshawar"]<-"FR Peshawar"
full@data$DISTRICT[full@data$DISTRICT=="Fr Kohat"]<-"FR Kohat"
full@data$DISTRICT[full@data$DISTRICT=="Fr Lakki Marwat"]<-"FR Lakki Marwat"
full@data$DISTRICT[full@data$DISTRICT=="Fr D.i.khan"]<-"FR D.I. Khan"
full@data$DISTRICT[full@data$DISTRICT=="Fr Bannu"]<-"FR Bannu"
full@data$DistName<-full@data$DISTRICT
full@data$DistName<-as.factor(full@data$DISTRICT)
full_nb<-poly2nb(full)
full_lw <- nb2listw(full_nb)
data<-subset(mth.dist, month==1 & year==2005)
data<-distinct(data, DistName)
full@data$order<-rownames(full@data)
full.df <- as(full, "data.frame")
full.df<-merge(full.df, data, by="DistName", all.x=TRUE, all.y=FALSE)
full.df$order<-as.numeric(full.df$order)
full.df<-arrange(full.df, order)
full <- SpatialPolygonsDataFrame(full,data=full.df, match.ID=F)
full$terr.evnt.bin[is.na(full$terr.evnt.bin)]<-0
full$drone[is.na(full$drone)]<-0
full$terr.evnt.bin.slag<-lag.listw(full_lw,full$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
full$drone.slag<-lag.listw(full_lw,full$drone,zero.policy=TRUE, NAOK=T)
full.df2 <- as(full, "data.frame")
full.df2$month<-1
full.df2$year<-2005
full.df5<-full.df2

for (i in 2:12) {
  full<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/full_districts", "Pak_adm3_pco_20110324")
  full@data$DISTRICT<-as.character(full@data$DISTRICT)
  full@data$DISTRICT[full@data$DISTRICT=="Fr Tank"]<-"FR Tank"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Peshawar"]<-"FR Peshawar"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Kohat"]<-"FR Kohat"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Lakki Marwat"]<-"FR Lakki Marwat"
  full@data$DISTRICT[full@data$DISTRICT=="Fr D.i.khan"]<-"FR D.I. Khan"
  full@data$DISTRICT[full@data$DISTRICT=="Fr Bannu"]<-"FR Bannu"
  full@data$DistName<-full@data$DISTRICT
  full@data$DistName<-as.factor(full@data$DISTRICT)
  full_nb<-poly2nb(full)
  full_lw <- nb2listw(full_nb)
  data<-subset(mth.dist, month==i & year==2005)
  data<-distinct(data, DistName)
  full@data$order<-rownames(full@data)
  full.df <- as(full, "data.frame")
  full.df<-merge(full.df, data, by="DistName", all.x=TRUE, all.y=FALSE)
  full.df$order<-as.numeric(full.df$order)
  full.df<-arrange(full.df, order)
  full <- SpatialPolygonsDataFrame(full,data=full.df, match.ID=F)
  full$terr.evnt.bin[is.na(full$terr.evnt.bin)]<-0
  full$drone[is.na(full$drone)]<-0
  full$terr.evnt.bin.slag<-lag.listw(full_lw,full$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
  full$drone.slag<-lag.listw(full_lw,full$drone,zero.policy=TRUE, NAOK=T)
  full.df3 <- as(full, "data.frame")
  full.df3$month<-i
  full.df3$year<-2005
  full.df2<-rbind(full.df2,full.df3)
  
}

full.df6<-rbind(full.df5, full.df2)
full.df2<-full.df6

for (i in 1:12) {
  for (j in 2006:2012){
    full<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/full_districts", "Pak_adm3_pco_20110324")
    full@data$DISTRICT<-as.character(full@data$DISTRICT)
    full@data$DISTRICT[full@data$DISTRICT=="Fr Tank"]<-"FR Tank"
    full@data$DISTRICT[full@data$DISTRICT=="Fr Peshawar"]<-"FR Peshawar"
    full@data$DISTRICT[full@data$DISTRICT=="Fr Kohat"]<-"FR Kohat"
    full@data$DISTRICT[full@data$DISTRICT=="Fr Lakki Marwat"]<-"FR Lakki Marwat"
    full@data$DISTRICT[full@data$DISTRICT=="Fr D.i.khan"]<-"FR D.I. Khan"
    full@data$DISTRICT[full@data$DISTRICT=="Fr Bannu"]<-"FR Bannu"
    full@data$DistName<-full@data$DISTRICT
    full@data$DistName<-as.factor(full@data$DISTRICT)
    full_nb<-poly2nb(full)
    full_lw <- nb2listw(full_nb)
    data<-subset(mth.dist, month==i & year==j)
    data<-distinct(data, DistName)
    full@data$order<-rownames(full@data)
    full.df <- as(full, "data.frame")
    full.df<-merge(full.df, data, by="DistName", all.x=TRUE, all.y=FALSE)
    full.df$order<-as.numeric(full.df$order)
    full.df<-arrange(full.df, order)
    full <- SpatialPolygonsDataFrame(full,data=full.df, match.ID=F)
    full$terr.evnt.bin[is.na(full$terr.evnt.bin)]<-0
    full$drone[is.na(full$drone)]<-0
    full$terr.evnt.bin.slag<-lag.listw(full_lw,full$terr.evnt.bin,zero.policy=TRUE, NAOK=T)
    full$drone.slag<-lag.listw(full_lw,full$drone,zero.policy=TRUE, NAOK=T)
    full.df3 <- as(full, "data.frame")
    full.df3$month<-i
    full.df3$year<-j
    full.df2<-rbind(full.df2,full.df3)
  }
}

mth.dist.slag<-filter(full.df2, PROVINCE=="Fata")
mth.dist.slag$drone.slag.bin<-ifelse(mth.dist.slag$drone.slag>1,1,0)

summary(zelig(terr.evnt.bin.lead ~ 
                terr.evnt.bin + terr.evnt.bin.slag+ 
                drone+drone.slag.bin + DistName+as.factor(month), 
              data=mth.dist.slag, model="logit", 
              robust=TRUE, cluster="DistName"))

write.csv(mth.dist.slag, "mth.dist.slag.csv")

bb<-wk.dist.slag %>%
  group_by(year, week, DistName) %>%
  mutate(drone.lag2=lag())
  

##########################

#___month
detach("package:plyr", unload=FALSE)
library(dplyr)
terror.dt<-as.data.frame(terror.fata)
fata.count<-terror.dt %>%
  group_by(year, month, TehsName) %>%
  summarize(count=n())
fata.nkill<-terror.dt %>%
  group_by(year, month, TehsName) %>%
  summarize(nkill=sum(nkill))

#____week
terror.fata.wk<-as.data.frame(terror.fata.wk)
fata.count.wk<-terror.fata.wk %>%
  group_by(year, week, TehsName) %>%
  summarize(count=n())
fata.nkill.wk<-terror.fata.wk %>%
  group_by(year, week, TehsName) %>%
  summarize(nkill=sum(nkill))

drone<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/dronestrikesdata.csv")
drone$Date<-as.Date(drone$Date,  "%d/%m/%Y")
drone$Date<-ymd(drone$Date)
drone$week<-week(drone$Date)
drone$y<-drone$Latitude
drone$x<-drone$Longitude
coordinates(drone) <- ~ x + y
proj4string(drone) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
drone.fata.wk<-point.in.poly(drone, fata)

drone.dt.wk<-as.data.frame(drone.fata.wk)
drone.dt.wk$year<-year(drone.dt.wk$Date)
fata.drone.count.wk<-drone.dt.wk %>%
  group_by(year, week, TehsName) %>%
  summarize(drone=n())
fata.drone.cvl.wk<-drone.dt.wk %>%
  group_by(year, week, TehsName) %>%
  summarize(cvl.kill=sum(Maximum.Civilians.Reported.Killed))

fata.ivars.wk<-merge(fata.drone.cvl.wk,fata.drone.count.wk, by=c("year", "week", "TehsName"), all=T)

fata.dvars.wk<-merge(fata.count.wk,fata.nkill.wk, by=c("year", "week", "TehsName"), all=T)
fata.data<-merge(fata.dvars.wk,fata.test3, by=c("year", "week", "TehsName"), all.x=F, all.y=T)
fata.data$count[is.na(fata.data$count)] <- 0
fata.data$nkill[is.na(fata.data$nkill)] <- 0

fata.data<-merge(fata.ivars.wk,fata.data, by=c("year", "week", "TehsName"), all.x=F,all.y=T)
fata.data$drone[is.na(fata.data$drone)] <- 0
fata.data$cvl.kill[is.na(fata.data$cvl.kill)] <- 0

write.csv(fata.data,"/users/cyrusmohammadian/desktop/fata.week.tehs.csv")
fata.data<-read.csv("/users/cyrusmohammadian/desktop/fata.week.tehs.csv")

fata.data<-fata.data %>%
  filter(year>2004) %>%
  group_by(group) %>%
  mutate(time=1:468)

fata.data<-fata.data %>%
  group_by(group) %>%
  mutate(count.lead=lead(count,order_by =time)) 

drone.dist<-drone.dt.wk %>%
  group_by(year, week, DistName) %>%
  summarize(drone=n())
cvl.dist<-drone.dt.wk %>%
  group_by(year, week, DistName) %>%
  summarize(cvl.kill=sum(Maximum.Civilians.Reported.Killed))

drone.cvl.dist<-merge(drone.dist,cvl.dist, by=c("year", "week", "DistName"), all=T)
final.data<-merge(fata.data,drone.cvl.dist, by=c("year", "week", "DistName"), all.x=T)

final.data
final.data$cvl.kill.x[is.na(final.data$cvl.kill.x)] <- 0
final.data$cvl.kill.y[is.na(final.data$cvl.kill.y)] <- 0

final.data$drone.y[is.na(final.data$drone.y)] <- 0
final.data$drone.y[is.na(final.data$drone.y)] <- 0
final.data$drone.dist<-final.data$drone.y
final.data$cvl.kill.dist<-final.data$cvl.kill.y

final.data$drone.y<-NULL
final.data$cvl.kill.y<-NULL

#-----

drone.dt<-as.data.frame(drone.fata)
drone.dt$month<-NULL
drone.dt$year<-year(drone.dt$Date)
drone.dt$month<-month(drone.dt$Date)
fata.drone.count<-drone.dt %>%
  group_by(year, month, TehsName) %>%
  summarize(drone=n())
fata.drone.cvl<-drone.dt %>%
  group_by(year, month, TehsName) %>%
  summarize(cvl.kill=sum(Maximum.Civilians.Reported.Killed))

fata.ivars<-merge(fata.drone.cvl,fata.drone.count, by=c("year", "month", "TehsName"), all=T)

fata.dvars<-merge(fata.count,fata.nkill, by=c("year", "month", "TehsName"), all=T)
fata.data<-merge(fata.dvars,fata.data, by=c("year", "month", "TehsName"), all=T)
fata.data$count[is.na(fata.data$count)] <- 0
fata.data$nkill[is.na(fata.data$nkill)] <- 0

fata.data<-merge(fata.ivars,fata.data, by=c("year", "month", "TehsName"), all=T)
fata.data$drone[is.na(fata.data$drone)] <- 0
fata.data$cvl.kill[is.na(fata.data$cvl.kill)] <- 0

fata.data <- streg.sort(fata.data, "week","TehsName")
fata.data <- streg.sort(fata.data, "year","TehsName")
write.csv(fata.data,"/users/cyrusmohammadian/desktop/fata.data2.wk.csv")

fata.data<-read.csv("/users/cyrusmohammadian/desktop/fata.data2.csv")

fata.data<-fata.data %>%
  filter(year>2004) %>%
  group_by(group) %>%
  mutate(time=1:108)

fata.data<-fata.data %>%
  group_by(group) %>%
  mutate(count.lead=lead(count,order_by =time)) 

#____agency-week
drone<-read.csv("/Users/cyrusmohammadian/Documents/Phd/Drone Data/dronestrikesdata.csv")
drone$Date<-as.Date(drone$Date,  "%d/%m/%Y")
drone$year<-year(drone$Date)
drone$week<-week(drone$Date)

drone$y<-drone$Latitude
drone$x<-drone$Longitude
coordinates(drone) <- ~ x + y
proj4string(drone) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
pak<-readOGR("/Users/cyrusmohammadian/Documents/Phd/Drone Data/Pak_adm2_pco_20110324_2", "Pak_adm2_pco_20110324")
drone.pak<-point.in.poly(drone, pak)

drone.pak<-as.data.frame(drone.pak)
drone.district<-drone.pak %>%
  filter(PROVINCE=="Fata")
drone.district$TehsName<-drone.district$DISTRICT

drone.district.count<-drone.district %>%
  group_by(year, week, TehsName) %>%
  summarize(drone=n())
drone.district.cvl<-drone.district %>%
  group_by(year, week, TehsName) %>%
  summarize(cvl.kill=sum(Maximum.Civilians.Reported.Killed))

terror.wk<-read.csv("/users/cyrusmohammadian/desktop/tr.csv")
terror.wk$date<-paste(terror.wk$year, terror.wk$month, terror.wk$day, sep="-")
terror.wk$date<-as.Date(terror.wk$date)
terror.wk$date<-ymd(terror.wk$date)
terror.wk$week<-week(terror.wk$date)

terror.wk$y<-terror.wk$latitude
terror.wk$x<-terror.wk$longitude
coordinates(terror.wk) <- ~ x + y
proj4string(terror.wk) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
terror.pak<-point.in.poly(terror.wk, pak)

terror.pak<-as.data.frame(terror.pak)
terror.district<-terror.pak %>%
  filter(PROVINCE=="Fata")
terror.district$TehsName<-terror.district$DISTRICT

terror.district.count<-terror.district %>%
  group_by(year, week, TehsName) %>%
  summarize(count=n())
terror.district.nkill<-terror.district %>%
  group_by(year, week, TehsName) %>%
  summarize(nkill=sum(nkill))

pak.data<-pak@data
pak.data<-pak.data %>%
  filter(PROVINCE=="Fata")
pak.data$TehsName<-pak.data$DISTRICT

pak@data$group<-pak@data$HRpcode
pak.data<-pak@data
pak.data<-pak.data %>%
  filter(PROVINCE=="Fata")
pak.data$start<-2004
pak.data$end<-2013 

library(plyr)
pak.data<-ddply(pak.data, .(group), function(x){
  data.frame(
    group=x$group,
    year=seq(x$start, x$end),
    TehsName=x$DISTRICT
  )
}
)

pak.data$start<-1
pak.data$end<-52
pak.data$yearname<-paste(pak.data$TehsName, pak.data$year)

pak.data<-ddply(pak.data, .(yearname), function(x){
  data.frame(
    group=x$group,
    TehsName=x$TehsName,
    year=x$year,
    week=seq(x$start, x$end)
  )
}
)

fata.ivars.wk<-merge(drone.district.cvl,drone.district.count, by=c("year", "week", "TehsName"), all=T)

fata.dvars.wk<-merge(terror.district.count,terror.district.nkill, by=c("year", "week", "TehsName"), all=T)
pak.data<-merge(fata.dvars.wk,pak.data, by=c("year", "week", "TehsName"), all.x=F, all.y=T)
pak.data$count[is.na(pak.data$count)] <- 0
pak.data$nkill[is.na(pak.data$nkill)] <- 0

pak.data<-merge(fata.ivars.wk,pak.data, by=c("year", "week", "TehsName"), all.x=F,all.y=T)
pak.data$drone[is.na(pak.data$drone)] <- 0
pak.data$cvl.kill[is.na(pak.data$cvl.kill)] <- 0

write.csv(pak.data,"/users/cyrusmohammadian/desktop/pak.data.wk.csv")
pak.data<-read.csv("/users/cyrusmohammadian/desktop/pak.data.wk.csv")

pak.data<-pak.data %>%
  group_by(group) %>%
  mutate(time=1:520)

pak.data<-pak.data %>%
  group_by(group) %>%
  mutate(count.lead=lead(count,order_by =time)) %>%
  filter(year>2004)

write.csv(pak.data,"/users/cyrusmohammadian/desktop/pak.data.wk2.csv")
write.csv(final.data,"/users/cyrusmohammadian/desktop/final.data.fata.csv")
final.data<-read.csv("/users/cyrusmohammadian/desktop/final.data.fata.csv")
final.data$X<-NULL

final.data$dv.lead<-ifelse(final.data$count.lead>0,1,0)
final.data$dv.lag<-ifelse(final.data$count>0,1,0)

##Analysis
#Agency-week
library(Zelig)
summary(zelig(dv.lead ~ dv.lag 
              + cvl.kill.x + drone.x + TehsName, 
              data=final.data, model="logit", 
              robust=TRUE, cluster="TehsName"))

#District-week









  
