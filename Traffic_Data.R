#library
library(dplyr)
library(tidyr)
library(ggplot2)

str(Traffic_Data)
summary(Traffic_Data)
class(Traffic_Data)

#Renaming some columns
Traffic_Data <- Traffic_Data %>% rename(Collision = "Collision Type", DayType = "Weekend?", Injury = "Injury Type", Cause =
"Primary Factor")
colnames(Traffic_Data)


#Lollipop chart with year of highest accidents. 2003 is the most months with accidents
Traffic_Data %>%
  group_by(Year) %>%
  summarise(Month = n()) %>%
  ggplot() +
  geom_point(aes(x=reorder(Year, -Month), y=Month),size =3) +
  geom_segment(aes(x=reorder(Year, -Month),
                   xend=reorder(Year, -Month),
                   y=0,
                   yend=Month)) +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1)) +
  labs(title= "Lollipop Chart of All Years by Total Months Count",
       x="Years",
       y="Total Month Count")

#Majority of the accidents happened in Month 10(October)
Traffic_Data %>%
  group_by(Month) %>%
  summarise(Day = n()) %>%
  ggplot() +
  geom_point(aes(x=reorder(Month, -Day), y=Day),size =3) +
  geom_segment(aes(x=reorder(Month, -Day),
                   xend=reorder(Month, -Day),
                   y=0,
                   yend=Day)) +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1)) +
  labs(title= "Lollipop Chart of All Months by Total Days Count",
       x="Months",
       y="Total Days Count")


#No injuries were sustained in the majority of accidents that  occured
Traffic_Data %>%
  group_by(Injury) %>%
  summarise(Day = n()) %>%
  ggplot() +
  geom_point(aes(x=reorder(Injury, -Day), y=Day),size =3) +
  geom_segment(aes(x=reorder(Injury, -Day),
                   xend=reorder(Injury, -Day),
                   y=0,
                   yend=Day)) +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1)) +
  labs(title= "All Injuries by Total Days Count",
       x="Injury",
       y="Total Days Count")

#The Major Collisions involved Two Cars, followd by One Car
ggplot(Traffic_Data,aes(x=toupper(Collision)))+
  geom_bar()+
  xlab(label = "Collision Type")+ylab(label = "Count")+
  theme_classic() +
  labs(title = "Collision Types")

#Top 10 Primary causes of the accidents
Traffic_Data %>%
  group_by(Cause) %>%
  summarise(Year = n()) %>%
  head(., n=10) %>%
  ggplot() +
  geom_point(aes(x=reorder(Cause, -Year), y=Year),size =3) +
  geom_segment(aes(x=reorder(Cause, -Year),
                   xend=reorder(Cause, -Year),
                   y=0,
                   yend=Year)) +
  theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1)) +
  labs(title= "Top 10 Causes of the Accidents",
       x="Causes",
       y="Total Month Count")

#Injury caused by Collision Type
ggplot(Traffic_Data,aes(x=Collision,fill=Injury))+
  geom_bar(position = "dodge")+theme_classic() +
  labs(title = "Injuries Caused by Collision Type")

#Weekend Vs Weekday with Accidents Rates. Most Accidents Happened on a Weekday
ggplot(Traffic_Data,aes(x=Year,fill=DayType))+
  geom_bar(position = "dodge")+theme_classic() +
  labs(title = "Weekend Vs Weekday with Accidents Rates")

#Day with Highest Accidents is Saturday
ggplot(Traffic_Data,aes(x=toupper(Day)))+geom_bar()+
  xlab(label = "Day")+ylab(label = "Count")+
  theme_classic()+
  labs(title = "Day with Highest Accidents Rates")




