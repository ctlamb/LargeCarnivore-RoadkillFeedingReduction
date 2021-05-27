Roadkill Carcass Attraction of Large Carnivores and Solutions
================
Clayton Lamb
27 May, 2021

# Test for Attraction of Collared Grizzly Bears Before/After Mitigation

## Load Data

``` r
library(here)
library(tidyverse)
library(sf)
library(readxl)
library(lubridate)
library(mapview)
library(ggmap)
library(raster)
library(stars)
library(velox)
library(adehabitatHR)
library(lme4)
library(MuMIn)
library(broom)
library(hrbrthemes)
library(RColorBrewer)

###grizz
ev.grizz <- read.csv(here::here("data","EVcollar_Relocs.csv"))


##pits
pits<-read.csv(here::here("data","EV_carcasspits.csv"))%>%
  st_as_sf(coords=c("long","lat"),
           crs=4326)%>%
  st_transform("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs")%>%
  mutate(pit=1)
```

## Clean

``` r
###grizz
ev.grizz<- ev.grizz%>%
  mutate(datetime=DateTime%>%ymd_hms(tz="MST"),
         sp="grizzlybear",
         study="EV_Grizz_Lamb",
         fixtype="3D",
         sensortype="gps",
         Name=as.character(Name),
         id_yr=paste(Name,year(datetime),sep="_"))%>%
  dplyr::select(sp,
         study,
         id=Name,
         id_yr,
         datetime,
         lat=Latitude,
         long=Longitude,
         dop=DOP,
         fixtype,
         sensortype)%>%
  tibble()%>%
  st_as_sf(coords=c("long","lat"),
           crs=4326)%>%
  st_transform("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs")

##keep my bears
ev.grizz <- ev.grizz%>%filter(year(datetime)>2015)

##keep only animal-years with >50 locations
keep <- ev.grizz%>%
  group_by(id_yr)%>%
  summarize(n=n())%>%
  filter(n>50)%>%
  pull(id_yr)

ev.grizz <- ev.grizz%>%filter(id_yr%in%keep)


##classify pits
pit.uncontrolled.hwy <- pits%>%
  filter(pit_name%in%c("Olsen","Tunnel","Brule"))%>%
  st_buffer(500)

pit.controlled <- pits%>%
  filter(type%in%c("controlled"))%>%
  st_buffer(500)

# 
# mapview(pit.uncontrolled.hwy)
# mapview(pit.uncontrolled.other)
# mapview(pit.controlled)
```

## Prep availability and estimate selection ratio

``` r
##ESTIMATE AVAILABILITY FOR EACH BEAR
animalyrs  <- unique(ev.grizz$id_yr)
df <-tibble()
for(i in 1:length(animalyrs)){
  bear.i <- ev.grizz%>%filter(id_yr%in%animalyrs[i])%>%as("Spatial")
  
  mcp <- mcp(bear.i%>%as("SpatialPoints"), percent=99)%>%
    st_as_sf()

  uncont <- ifelse(nrow(st_intersection(pit.uncontrolled.hwy,mcp))>0,
                   st_area(st_intersection(pit.uncontrolled.hwy,mcp))/st_area(mcp),
                   0)
                   
  cont <- ifelse(nrow(st_intersection(pit.controlled,mcp))>0,
                   st_area(st_intersection(pit.controlled,mcp))/st_area(mcp),
                   0)
  
  uncont.use <- ifelse(nrow(st_intersection(bear.i%>%st_as_sf,pit.uncontrolled.hwy))>0,
                       nrow(st_intersection(bear.i%>%st_as_sf,pit.uncontrolled.hwy))/nrow(bear.i),
                       0)

  
  cont.use <- ifelse(nrow(st_intersection(bear.i%>%st_as_sf,pit.controlled))>0,
                       nrow(st_intersection(bear.i%>%st_as_sf,pit.controlled))/nrow(bear.i),
                       0)

                
  b <- tibble(id=bear.i$id[1],
                  id_yr=bear.i$id_yr[1],
                  uncont=uncont,
                  cont=cont,
                  uncont.use=uncont.use,
                  cont.use=cont.use)
  
 df <- rbind(df,b)
                  
}


#####select bears with availability
df.uncont <- df%>%
  filter(uncont>0)%>%
  mutate(s=uncont.use/uncont,
         year=str_sub(id_yr,-4,-1),
         id=str_split(id_yr,"_", simplify=TRUE)[,1])


ggplot(df.uncont, aes(x=id, y=s))+
  #geom_linerange(aes(ymin=uncont-uncont.se, ymax=uncont+uncont.se))
  geom_point()+
  facet_grid(vars(year))+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=45, hjust=1))
```

![](README_files/figure-gfm/avail-1.png)<!-- -->

``` r
df.cont <- df%>%
  filter(cont>0)%>%
  mutate(s=cont.use/cont,
         year=str_sub(id_yr,-4,-1),
         id=str_split(id_yr,"_", simplify=TRUE)[,1])


ggplot(df.cont, aes(x=id, y=s))+
  #geom_linerange(aes(ymin=cont-cont.se, ymax=cont+cont.se))
  geom_point()+
  facet_grid(vars(year))+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=45, hjust=1))
```

![](README_files/figure-gfm/avail-2.png)<!-- -->

## Plot

``` r
df.uncont%>%
  mutate(period=case_when(year<2019~"Carcasses",
                          year>=2019~"No carcasses"),
         group="Uncontrolled")%>%
  group_by(period, group)%>%
  summarise(mean=median(s,na.rm=TRUE),
            se=sd(s)/sqrt(n()))%>%
  rbind(
    df.cont%>%
      mutate(period=case_when(year<2019~"No carcasses",
                              year>=2019~"Carcasses"),
             group="Controlled")%>%
      group_by(period, group)%>%
      summarise(mean=median(s,na.rm=TRUE),
                se=sd(s)/sqrt(n())))%>%
  mutate(lower=case_when(mean-se>0~mean-se, TRUE~0))%>%
  ungroup%>%
  ggplot(aes(x=fct_relevel(period,"Carcasses", "No carcasses"),y=mean, group=group))+
  geom_linerange(aes(ymin=lower, ymax=mean+se))+
  geom_point()+
  geom_line(linetype="dashed")+
  facet_grid(vars(fct_relevel(group, "Uncontrolled", "Controlled")))+
  theme_ipsum()+
  geom_hline(yintercept = 1)+
  labs(x="Year",y="Selection ratio", title="Selection for Carcass Pits Before/After Mitigation")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/plot%20selection-1.png)<!-- -->

# Test for Attraction of Large Carnivores to cameras at pits vs bunkers

## Load Data

``` r
##load data
df <- read_csv(here::here("data","LER_CarcassPits_report.csv"))
deploy <- read_csv(here::here("data","Camera Deployment.csv"))
```

## Prep

``` r
##clean
df <- df%>%
  filter(!common_name %in% c("NONE", "Common Raven", "Deer", "Golden eagle", "Magpie", "STAFF/SETUP","Turkey Vulture","Voles, Mice and Allies", 
                             "Wolves, Coyotes and Allies","Striped Skunk","UNKNOWN"))%>%
  filter(!location%in%"OLSENPITCORNER2018")

##identify distinct events
df <- df%>%
  group_by(location,common_name)%>%
  arrange(date_detected)%>%
  mutate(timedif=c(0.1,diff(date_detected))/60,
         start = timedif > 5,
         event.id=paste(cumsum(start),common_name,location, sep="_"))

##time cameras were out
deploy <- deploy%>%
  mutate(removed=mdy_hm(removed),
         deployed=mdy_hm(deployed),
         dur=removed- deployed)
         


##create time in front of camera
tif <- df%>%
  dplyr::select(location,date_detected,common_name,age_class,sex,number_individuals, timedif, event.id)%>%
  mutate(night_day=(date_detected-hours(6))%>%yday,
         number_individuals=case_when(number_individuals%in%"VNA"~"1",TRUE~number_individuals)%>%as.numeric)
```

## Plot

``` r
df%>%
  group_by(common_name,location)%>%
  count%>%
  mutate(n=1)%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Pic count",
       title="Counts")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/explore%20and%20clean-1.png)<!-- -->

``` r
tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & timedif<5)%>%
  mutate(number_individuals=case_when(number_individuals%in%"VNA"~1,TRUE~number_individuals),
         timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif))%>%
  left_join(deploy%>%dplyr::select(location,dur,type))%>%
  mutate(hr_days=n/(as.numeric(dur)))%>%
  group_by(type, common_name)%>%
  summarise(hr_days=mean(hr_days))%>%
  ggplot(aes(x=type,y=hr_days,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Time per day (minutes)",
       title="Time")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/explore%20and%20clean-2.png)<!-- -->

``` r
tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & timedif<5)%>%
  mutate(number_individuals=case_when(number_individuals%in%"VNA"~1,TRUE~number_individuals),
         timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif))%>%
  left_join(deploy%>%dplyr::select(location,dur,type))%>%
  mutate(hr_days=n/(as.numeric(dur)))%>%
  group_by(type, common_name)%>%
  summarise(hr_days=mean(hr_days))%>%
  ggplot(aes(x=type,y=hr_days,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Time per day (minutes)",
       title="Time")+
  facet_wrap(vars(common_name),scales="free_y")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/explore%20and%20clean-3.png)<!-- -->

``` r
tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & 
           timedif<5 & 
           common_name%in%c("Black Bear","Grizzly bear","Coyote","Gray Wolf"))%>%
  mutate(number_individuals=case_when(number_individuals%in%"VNA"~1,TRUE~number_individuals),
         timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif))%>%
  left_join(deploy%>%dplyr::select(location,dur,type))%>%
  mutate(hr_days=n/(as.numeric(dur)))%>%
  group_by(type, common_name)%>%
  summarise(min_days=mean(hr_days))%>%
  filter(type%in%c("Bunker","Pit"))%>%
  ggplot(aes(x=type,y=min_days,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Time per day (minutes)",
       title="Time")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
```

![](README_files/figure-gfm/explore%20and%20clean-4.png)<!-- -->
