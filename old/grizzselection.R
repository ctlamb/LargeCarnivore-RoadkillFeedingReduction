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

pits<-read.csv(here::here("data","EV_carcasspits.csv"))%>%
  st_as_sf(coords=c("long","lat"),
           crs=4326)%>%
  st_transform("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs")%>%
  mutate(pit=1)


pit.uncontrolled.hwy <- pits%>%
  filter(pit_name%in%c("Olsen","Tunnel","Brule"))%>%
  st_buffer(500)

pit.uncontrolled.other <- pits%>%
  filter(pit_name%in%c("Hosmer","Sparwood","Hosmer2"))%>%
  st_buffer(500)

pit.controlled <- pits%>%
  filter(type%in%c("controlled"))%>%
  st_buffer(500)


mapview(pit.uncontrolled.hwy)
mapview(pit.uncontrolled.other)
mapview(pit.controlled)


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
  
  uncont.other <- ifelse(nrow(st_intersection(pit.uncontrolled.other,mcp))>0,
                   st_area(st_intersection(pit.uncontrolled.other,mcp))/st_area(mcp),
                   0)
                   
  cont <- ifelse(nrow(st_intersection(pit.controlled,mcp))>0,
                   st_area(st_intersection(pit.controlled,mcp))/st_area(mcp),
                   0)
  
  uncont.use <- ifelse(nrow(st_intersection(bear.i%>%st_as_sf,pit.uncontrolled.hwy))>0,
                       nrow(st_intersection(bear.i%>%st_as_sf,pit.uncontrolled.hwy))/nrow(bear.i),
                       0)
  
  uncont.other.use <- ifelse(nrow(st_intersection(bear.i%>%st_as_sf,pit.uncontrolled.other))>0,
                       nrow(st_intersection(bear.i%>%st_as_sf,pit.uncontrolled.other))/nrow(bear.i),
                       0)
  
  cont.use <- ifelse(nrow(st_intersection(bear.i%>%st_as_sf,pit.controlled))>0,
                       nrow(st_intersection(bear.i%>%st_as_sf,pit.controlled))/nrow(bear.i),
                       0)

                
  b <- tibble(id=bear.i$id[1],
                  id_yr=bear.i$id_yr[1],
                  uncont=uncont,
                  uncont.oth=uncont.other,
                  cont=cont,
                  uncont.use=uncont.use,
                  uncont.oth.use=uncont.other.use,
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


df.uncont.oth <- df%>%
  filter(uncont.oth>0)%>%
  mutate(s=uncont.oth.use/uncont.oth,
         year=str_sub(id_yr,-4,-1),
         id=str_split(id_yr,"_", simplify=TRUE)[,1])

ggplot(df.uncont.oth, aes(x=id, y=s))+
  #geom_linerange(aes(ymin=uncont-uncont.se, ymax=uncont+uncont.se))
  geom_point()+
  facet_grid(vars(year))+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=45, hjust=1))


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

##ROLL TOGETHER
df.uncont%>%
  mutate(period=case_when(year<2019~"before",
                          year>=2019~"after"),
         group="Pits")%>%
  group_by(period, group)%>%
  summarise(mean=median(s,na.rm=TRUE),
            se=sd(s)/sqrt(n()))%>%
  rbind(
  df.cont%>%
  mutate(period=case_when(year<2019~"before",
                          year>=2019~"after"),
         group="Bunker")%>%
  group_by(period, group)%>%
  summarise(mean=median(s,na.rm=TRUE),
            se=sd(s)/sqrt(n())))%>%
  rbind(
    df.uncont.oth%>%
      mutate(period=case_when(year<2019~"before",
                              year>=2019~"after"),
             group="Pits-other")%>%
      group_by(period, group)%>%
      summarise(mean=median(s,na.rm=TRUE),
                se=sd(s)/sqrt(n())))%>%
  ungroup%>%
  ggplot(aes(x=fct_relevel(period,"before", "after"),y=mean, group=group))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se))+
  geom_point()+
  geom_line(linetype="dashed")+
  facet_grid(vars(fct_relevel(group, "Pits", "Bunker","Pits-other")))+
  theme_ipsum()+
  geom_hline(yintercept = 1)+
  labs(x="Year",y="Selection ratio", title="Selection for Carcass Pits Before/After Mitigation")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))
  



df.uncont%>%
  mutate(period=case_when(year<2019~"before",
                          year>=2019~"after"),
         group="Uncontrolled")%>%
  group_by(period, group)%>%
  summarise(mean=median(s,na.rm=TRUE),
            se=sd(s)/sqrt(n()))%>%
  rbind(
    df.cont%>%
      mutate(period=case_when(year<2019~"before",
                              year>=2019~"after"),
             group="Controlled")%>%
      group_by(period, group)%>%
      summarise(mean=median(s,na.rm=TRUE),
                se=sd(s)/sqrt(n())))%>%
  mutate(lower=case_when(mean-se>0~mean-se, TRUE~0))%>%
  ungroup%>%
  ggplot(aes(x=fct_relevel(period,"before", "after"),y=mean, group=group))+
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


















###START OF RSF



##load rasters
files <- list.files(here::here("data","spatial") , pattern = "*.tif$", full.names = TRUE)
stack <- stack(files)

##clip to extent of bear data
stack <- crop(stack,st_bbox(ev.grizz))
plot(stack)


##make pit raster
pits<-read.csv(here::here("data","EV_carcasspits.csv"))%>%
  st_as_sf(coords=c("long","lat"),
           crs=4326)%>%
  st_transform("+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs")%>%
  mutate(pit=1)

pit.rast <- rasterize(pits,
                      stack[["hwy"]],
                      field="pit",
                      background=NA)

pit.rast.uncontrolled <- rasterize(pits%>%filter(type%in%c("uncontrolled")&users%in%c("highways","highways/hunters")),
                                   stack[["hwy"]],
                                   field="pit",
                                   background=NA)

pit.rast.controlled <- rasterize(pits%>%filter(type%in%c("controlled")&users%in%c("highways","highways/hunters")),
                                 stack[["hwy"]],
                                 field="pit",
                                 background=NA)


pit.rast.dist <- distance(pit.rast)
plot(pit.rast.dist)
names(pit.rast.dist) <-"pit.dist"

pit.rast.uncontrolled.dist <- distance(pit.rast.uncontrolled)
plot(pit.rast.uncontrolled.dist)
names(pit.rast.uncontrolled.dist) <-"pituncontrolled.dist"

pit.rast.controlled.dist <- distance(pit.rast.controlled)
plot(pit.rast.controlled.dist)
names(pit.rast.controlled.dist) <-"pitcontrolled.dist"

##add pits to stack
stack <- addLayer(stack, pit.rast.dist,
                  pit.rast.uncontrolled.dist,
                  pit.rast.controlled.dist)


##draw availables
avail.draw <- 30

animalyrs  <- unique(ev.grizz$id_yr)
df.add <-tibble()
for(i in 1:length(animalyrs)){
  bear.i <- ev.grizz%>%filter(id_yr%in%animalyrs[i])%>%as("Spatial")
  mcp <- mcp(bear.i%>%as("SpatialPoints"), percent=99)
  
  avail <- st_sample(mcp%>%st_as_sf(),size=nrow(bear.i)*avail.draw)%>%
    tibble%>%
    mutate(id=bear.i$id[1],
           id_yr=bear.i$id_yr[1],
           datetime=rep(bear.i$datetime, each=avail.draw))%>%
    dplyr::select(id,id_yr,datetime,geometry=".")
  
  df.add <- rbind(avail,df.add)
}


##make into single df
df <- rbind(ev.grizz%>%dplyr::select(id, id_yr, datetime)%>%tibble%>%mutate(use=1),
            df.add%>%mutate(use=0))%>%
  st_as_sf()



##add spatial info
df_buffer <- df%>%st_buffer(31)
df <- df%>%
mutate(pit.dist=velox(stack[["pit.dist"]])$extract(df_buffer, fun=mean),
       pit.dist.uncont=velox(stack[["pituncontrolled.dist"]])$extract(df_buffer, fun=mean),
       pit.dist.cont=velox(stack[["pitcontrolled.dist"]])$extract(df_buffer, fun=mean),
       evi=velox(stack[["evi.summer_2017_2020"]])$extract(df_buffer, fun=mean),
       forestage=velox(stack[["forestage_2020"]])$extract(df_buffer, fun=mean),
       hwy=velox(stack[["hwy"]])$extract(df_buffer, fun=mean),
       road=velox(stack[["nonhwy_roads"]])$extract(df_buffer, fun=mean),
       topo=velox(stack[["topo.diver"]])$extract(df_buffer, fun=mean),
       lc=velox(stack[["lc_simp2020"]])$extract(df_buffer, fun=mean))%>%
  mutate(period=case_when(year(datetime)<2019~"before",
                        year(datetime)>=2019~"after"))%>%
  tibble



###classify in pit
dist.cut <- 500
df <- df%>%mutate(in.pit=case_when(pit.dist.uncont<=dist.cut~1,pit.dist.uncont>dist.cut~0),
                  in.bunker=case_when(pit.dist.cont<=dist.cut~1,pit.dist.cont>dist.cut~0))


##define which animals had access to the pit and to the bunker
pit.avail <- df%>%
  group_by(id_yr,use)%>%
  summarise(pit=mean(in.pit))%>%
  mutate(bear.use.pit=case_when(pit>0~"Y",pit==0~"N"))

pit.avail.bears <- pit.avail%>%
  filter(bear.use.pit%in%"Y")%>%
  distinct(id_yr)%>%
  pull(id_yr)

pit.missing.avail.bears <- pit.avail%>%
  filter(id_yr%in%pit.avail.bears,
         use%in%0,
         pit==0)%>%
  pull(id_yr)

         
bunker.avail <- df%>%
  group_by(id_yr,use)%>%
  summarise(bunker=mean(in.bunker))%>%
  mutate(bear.use.bunker=case_when(bunker>0~"Y",bunker==0~"N"))

bunker.avail.bears <- bunker.avail%>%
  filter(bear.use.bunker%in%"Y")%>%
  distinct(id_yr)%>%
  pull(id_yr)
         
bunker.missing.avail.bears <- bunker.avail%>%
  filter(id_yr%in%bunker.avail.bears,
         use%in%0,
         bunker==0)%>%
  pull(id_yr)


###adding missing avails when we know it was available
df<-df%>%
  rbind(
    df%>%filter(id_yr%in%pit.missing.avail.bears & use%in%0)%>%
      group_by(id_yr)%>%
      slice(1)%>%
      mutate(in.pit=1))


##add year
df<-df%>%
mutate(year=str_sub(id_yr,-4,-1))




##run models
betas <- tibble()

for(i in 1:length(pit.avail.bears)){
  m <- glm(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + in.pit,  data = df%>%
             filter(id_yr%in%pit.avail.bears[i]&
                      month(datetime)%in%c(4:11)), family ="binomial")
  
  betas <- rbind(betas,
                 tibble(id_yr=pit.avail.bears[i],
                        uncont=tidy(m)%>%filter(term=="in.pit")%>%pull("estimate"),
                        uncont.se=tidy(m)%>%filter(term=="in.pit")%>%pull("std.error"),
                        uncont.p=tidy(m)%>%filter(term=="in.pit")%>%pull("p.value")))
}

betas <- betas%>%
  mutate(year=str_sub(id_yr,-4,-1),
         id=str_split(id_yr,"_", simplify=TRUE)[,1])
  
ggplot(betas, aes(x=id, y=uncont,  color=uncont.se))+
  #geom_linerange(aes(ymin=uncont-uncont.se, ymax=uncont+uncont.se))
  geom_point()+
  facet_grid(vars(year))+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=45, hjust=1))


betas%>%
  group_by(year)%>%
  summarise(mean=median(uncont,na.rm=TRUE),
            upper=quantile(uncont,0.9,na.rm=TRUE),
            lower=quantile(uncont,0.1,na.rm=TRUE),
            se=sd(uncont)/sqrt(n()))%>%
  ggplot(aes(x=year,y=mean))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se))+
  geom_point()+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  labs(x="Year",y="Selection")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))


betas%>%
  mutate(period=case_when(year<2019~"before",
                          year>=2019~"after"))%>%
  group_by(period)%>%
  summarise(mean=median(uncont,na.rm=TRUE),
            upper=quantile(uncont,0.9,na.rm=TRUE),
            lower=quantile(uncont,0.1,na.rm=TRUE),
            se=sd(uncont)/sqrt(n()))%>%
  ggplot(aes(x=fct_relevel(period,"before", "after"),y=mean))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se))+
  geom_point()+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  labs(x="Year",y="Selection", title="Selection for Carcass Pits Before/After Mitigation")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))



betas%>%
  mutate(period=case_when(year<2019~"before",
                          year>=2019~"after"))%>%
  group_by(period)%>%
  mutate(wt=1/(uncont.se/uncont)/sum(1/(uncont.se/uncont)))%>%
  summarise(mean=sum(uncont*wt),
            upper=quantile(uncont,0.9,na.rm=TRUE),
            lower=quantile(uncont,0.1,na.rm=TRUE),
            se=sd(uncont)/sqrt(n()))%>%
  ggplot(aes(x=fct_relevel(period,"before", "after"),y=mean))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se))+
  geom_point()+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  labs(x="Year",y="Selection", title="Selection for Carcass Pits Before/After Mitigation")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))










##run models
betas2 <- tibble()

for(i in 1:length(bunker.avail.bears)){
  m <- glm(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + in.bunker,  data = df%>%
             filter(id_yr%in%bunker.avail.bears[i] &
             month(datetime)%in%c(4:11)), family ="binomial")
  
  betas2 <- rbind(betas2,
                 tibble(id_yr=bunker.avail.bears[i],
                        uncont=tidy(m)%>%filter(term=="in.bunker")%>%pull("estimate"),
                        uncont.se=tidy(m)%>%filter(term=="in.bunker")%>%pull("std.error"),
                        uncont.p=tidy(m)%>%filter(term=="in.bunker")%>%pull("p.value")))
}

betas2 <- betas2%>%
  mutate(year=str_sub(id_yr,-4,-1),
         id=str_split(id_yr,"_", simplify=TRUE)[,1])

ggplot(betas2, aes(x=id, y=uncont,  color=uncont.se))+
  #geom_linerange(aes(ymin=uncont-uncont.se, ymax=uncont+uncont.se))
  geom_point()+
  facet_grid(vars(year))+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=45, hjust=1))


betas2%>%
  group_by(year)%>%
  summarise(mean=median(uncont,na.rm=TRUE),
            upper=quantile(uncont,0.9,na.rm=TRUE),
            lower=quantile(uncont,0.1,na.rm=TRUE),
            se=sd(uncont)/sqrt(n()))%>%
  ggplot(aes(x=year,y=mean))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se))+
  geom_point()+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  labs(x="Year",y="Selection")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))


betas2%>%
  mutate(period=case_when(year<2019~"before",
                          year>=2019~"after"))%>%
  group_by(period)%>%
  summarise(mean=median(uncont,na.rm=TRUE),
            upper=quantile(uncont,0.9,na.rm=TRUE),
            lower=quantile(uncont,0.1,na.rm=TRUE),
            se=sd(uncont)/sqrt(n()))%>%
  ggplot(aes(x=fct_relevel(period,"before", "after"),y=mean))+
  geom_linerange(aes(ymin=mean-se, ymax=mean+se))+
  geom_point()+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  labs(x="Year",y="Selection", title="Selection for Carcass Pits Before/After Mitigation")+
  theme(axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15),
        axis.text = element_text(size=10),
        legend.text = element_text(size=13),
        legend.title=element_text(size=15))





m1 <- lmer(use ~ 1 + (1 | id_yr) , data = df)
m2 <- lmer(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + (1 | id_yr) , data = df)
m3 <- lmer(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + pit.dist + (1 | id_yr) , data = df)
m4 <- lmer(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + pit.dist + period + (1 | id_yr) , data = df)
m5 <- lmer(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + pit.dist*period + (1 | id_yr) , data = df)


model.sel(m1,m2,m3,m4,m5)
summary(m5)

##pre
m1 <- lmer(use ~ 1 + (1 | id_yr) , data = df%>%filter(period%in%"before"))
m2 <- lmer(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + (1 | id_yr) , data = df%>%filter(period%in%"before"))
m3 <- lmer(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + pit.dist.uncont + (1 | id_yr) , data = df%>%filter(period%in%"before"))

model.sel(m1,m2,m3)

betas <- tibble()

for(i in 1:nrow(pit.bears)){
  m <- glm(use ~ evi + forestage + I(forestage^2) + hwy + road + topo + pit.dist.uncont,  data = df%>%filter(id_yr%in%pit.bears$id_yr[i]), family ="binomial")
  
  betas <- rbind(betas,
                 tibble(pit.bears[i,],
                        uncont=tidy(m)%>%filter(term=="pit.dist.uncont")%>%pull("estimate"),
                        uncont.se=tidy(m)%>%filter(term=="pit.dist.uncont")%>%pull("std.error"),
                        uncont.p=tidy(m)%>%filter(term=="pit.dist.uncont")%>%pull("p.value")))
}


ggplot(betas, aes(x=id, y=uncont, ymin=uncont-uncont.se, ymax=uncont+uncont.se))+
  geom_point()+
  geom_linerange()+
  facet_grid(vars(year))+
  theme_ipsum()+
  geom_hline(yintercept = 0)+
  theme(axis.text.x=element_text(angle=45))
  

betas%>%
  group_by(year)%>%
  summarise(mean(uncont),
            upper=quantile(uncont,0.9),
            lower=quantile(uncont,0.1))







a <- df%>%
  group_by(id_yr,use)%>%
  summarise(mean=mean(in.pit))

ggplot(a, aes(x=id_yr,y=mean,color=use))+
  geom_point()

missing.pits <- a%>%filter(use%in%0 & mean==0)


b <- df%>%
  group_by(id_yr,use)%>%
  summarise(mean=mean(in.bunker))

ggplot(b, aes(x=id_yr,y=mean,color=use))+
  geom_point()

missing.bunker <- b%>%filter(use%in%0 & mean==0)





