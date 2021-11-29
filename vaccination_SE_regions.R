# install.packages("dplyr")
#install.packages("tidyverse")
#install.packages('gganimate')
#install.packages("gifski")
#install.packages("av")

library(readxl)
library(tidyverse)
library(gganimate)
library(gifski)
theme_set(theme_bw())

setwd("~/Pablo/PABLO/2021/R/Vaccination_Sweden")

today_date <- format(Sys.time(), "%Y%m%d")

today_url <- paste0("https://fohm.maps.arcgis.com/sharing/rest/content/items/fc749115877443d29c2a49ea9eca77e9/data")

name_url <- paste0("Vaccination_report_"
                   ,today_date
                   ,".xlsx")

download.file(today_url,name_url)

df_5_se <- read_excel(name_url,3)
head(read_excel(today_url,3))

#sheet 1 number of vaccionation per week and region
#sheet 2 divided by 1 or 2 dosis
#sheet 3 region and age
#sheet 4 gender
#sheet 5 komun
#sheet 6 region, komun, age, population

#df_1_se <- read_excel(name_url,1)
df_2_se <- read_excel(name_url,2)
#df_3_se <- read_excel(name_url,3)
#df_4_se <- read_excel(name_url,4)
#df_5_se <- read_excel(name_url,5)
#names(df_3_se)[c(2,5)] <- "Age_Group"
head(df_2_se)

names(df_2_se)[1:6] <- c("Week","Year","Region","No_vaccinations","Percentage_vac","Status")
head(df_2_se)

#filter regions
df_3regions_week <- filter(df_2_se,Week< 51,Region %in% c("Stockholm","| Sverige |","Västra Götaland"))
head(df_3regions_week)
#df_4_se <- filter(df_3_se,Region %in% c("| Sverige |"))
#head(df_4_se)

#column types
sapply(df_3regions_week, class)
df_3regions_week$Week <- as.integer(df_3regions_week$Week)
df_3regions_week$Year <- as.integer(df_3regions_week$Year)
sapply(df_3regions_week, class) 

#merge columns
#df_3_se_regions <- df_3_se_regions %>%
#  unite("Region_age", Region:Age_Group,remove=FALSE)
#head(df_3_se_regions)
#df_3_se_regions

#df_3_se_regions <- df_3_se_regions %>%
#  unite("Region_age_dosis", Region_age;Status, remove=FALSE)
#head(df_3_se_regions)

#bar stacked plot
vaccine_stacked <- ggplot(df_3regions_week, aes(fill=Status, y=Percentage_vac*100, x=Region)) + 
  geom_bar(position=position_dodge(), stat="identity")+
  #geom_text(aes(label = format((Percentage_vac*100), nsmall=1)), vjust = -0.2)+
  ggtitle("% of Population Vaccinated per Region in Sweden and two largest regions")+
  xlab("Region")+
  ylab("% of population vaccinated")+
  ylim(0, 100)+
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90))
vaccine_stacked
ggsave(filename = paste0("Vaccination_stacked_",today_date,".png"), width = 640/72, height = 450/72)

# 3 regions small multiples
vaccine_stacked_sm <- ggplot(df_3regions_week, aes(fill=Status, y=Percentage_vac*100, x=Region)) + 
  geom_bar(position=position_dodge(), stat="identity")+
  #geom_text(aes(label = format((Percentage_vac*100), nsmall=1)), vjust = -0.2)+
  ggtitle("% of Population Vaccinated per Region in Sweden and two largest regions")+
  xlab("Region")+
  ylab("% of population vaccinated")+
  ylim(0, 100)+
  facet_wrap(~Week) +
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 90))
vaccine_stacked_sm
ggsave(filename = paste0("Vaccination_stacked_smallMultiples_week",today_date,".png"), width = 640/72, height = 450/72)



#dynamic
vaccine_dynamic_bar <- vaccine_stacked + transition_time(Week) +
  labs(title = "Week: {frame}")
vaccine_dynamic_bar
anim_save(filename = "animated_vaccionation3.gif", animation = last_animation(), path = NULL)

