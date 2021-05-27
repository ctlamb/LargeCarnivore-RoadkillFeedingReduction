library(here)
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(tidylog)
options(scipen=999)

##load data
df <- read_csv(here::here("data","LER_CarcassPits_report.csv"))
deploy <- read_csv(here::here("data","Camera Deployment.csv"))


##clean
df <- df%>%
  filter(!common_name %in% c("NONE", "Common Raven", "Deer", "Golden eagle", "Magpie", "STAFF/SETUP","Turkey Vulture","Voles, Mice and Allies", 
                             "Wolves, Coyotes and Allies","Striped Skunk","UNKNOWN"))%>%
  filter(!location%in%"OLSENPITCORNER2018")

##summarise
df%>%
  group_by(common_name,location)%>%
  count%>%
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
  select(location,date_detected,common_name,age_class,sex,number_individuals, timedif, event.id)%>%
  mutate(night_day=(date_detected-hours(6))%>%yday,
         number_individuals=case_when(number_individuals%in%"VNA"~"1",TRUE~number_individuals)%>%as.numeric)


tif%>%
  group_by(common_name,location)%>%
  summarise(n=n_distinct(event.id))%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Event count",
       title="Unique Events")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))

tif%>%
  left_join(deploy%>%select(location,dur,type))%>%
  group_by(common_name,location,night_day,dur,type)%>%
  summarise(events=n_distinct(event.id),
            nind=median(number_individuals))%>%
  group_by(common_name,location,dur,type)%>%
  summarise(events=sum(events*nind))%>%
  group_by(common_name,type)%>%
  summarise(event_days=sum(events)/sum(as.numeric(dur)))%>%
  ggplot(aes(x=type,y=event_days,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Events per day",
       title="Unique Events")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))




tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & timedif<5)%>%
  mutate(timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif)/60)%>%
  ggplot(aes(x=location,y=n,fill=common_name))+
  geom_col()+
  theme_ipsum()+
  labs(x="Location", y="Total time (hours)",
       title="Counts")+
  theme(axis.title.y = element_text(size=15),
        axis.title.x = element_text(size=15),
        axis.text.x = element_text(size=12, angle=45,hjust=1),
        strip.text.x = element_text(size=15),
        strip.text.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))



tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & timedif<5)%>%
  mutate(number_individuals=case_when(number_individuals%in%"VNA"~1,TRUE~number_individuals),
         timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif))%>%
  left_join(deploy%>%select(location,dur,type))%>%
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


tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & 
           timedif<5 & 
           common_name%in%c("Black Bear","Grizzly bear","Coyote","Gray Wolf"))%>%
  mutate(number_individuals=case_when(number_individuals%in%"VNA"~1,TRUE~number_individuals),
         timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif))%>%
  left_join(deploy%>%select(location,dur,type))%>%
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


tif%>%
  left_join(tif %>%group_by(event.id)%>%count)%>%
  filter(n>1 & timedif<5)%>%
  mutate(number_individuals=case_when(number_individuals%in%"VNA"~1,TRUE~number_individuals),
         timedif=case_when(n==1~1,TRUE~timedif*as.numeric(number_individuals)))%>%
  group_by(common_name,location)%>%
  summarise(n=sum(timedif))%>%
  left_join(deploy%>%select(location,dur,type))%>%
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
