library("dplyr")
library("readr")
library(ggplot2)
library(car)

Filelist = list.files(path="C:/Users/xhmik/Desktop/Zeno Data/Aryan's project", full.names = TRUE)  
Rawdata = lapply(Filelist,read.csv)%>%bind_rows()

#Label each groups
Rawdata = Rawdata %>% mutate(group = case_when(
  participant == 917920 ~ "intermediate",
  participant == 44453 ~ "last"
))

#Move response up one row to cover the last group

for (i in 1:length(Rawdata$cycle)) {
  if (Rawdata$group[i] == "last" && is.na(Rawdata$key_resp_4.keys[i])==FALSE){
    Rawdata$key_resp_4.keys[i-1]=Rawdata$key_resp_4.keys[i]
  }
}

#Remove columns and rows

DataR1 = Rawdata[!"" == Rawdata$colour_now,]
DataR2 = DataR1[is.na(DataR1$cycle)==FALSE,]

#Add catch/control/first colour
for (i in 1:length(DataR2$Catch)){
  if (is.na(DataR2$Catch[i])==FALSE){
    CountN = 1
  }
  else{
    DataR2$Catch[i] = DataR2$Catch[i-CountN]
    DataR2$Control[i] = DataR2$Control[i-CountN]
    DataR2$First_colour[i] = DataR2$First_colour[i-CountN]
    CountN = CountN+1
  }
} 

#Change response to numbers and add label to all trial types
DataR3 = DataR2 %>% mutate(Response = case_when(
  key_resp_4.keys == "c" ~ 1,
  key_resp_4.keys == "n" ~ 0,
  key_resp_4.keys == "" ~ 0,)) %>%
  mutate(trial_type = case_when(
    Catch == 1 & colour_now == First_colour~ "Catch Same 50%",
    Catch == 1 & colour_now != First_colour~ "Catch Random 50%",
    Control == 1 ~ "Control",
    Catch == 0 & Control == 0 ~ "Gradual Change"))

#Single out the last ring and count the response based on group and trial type
ringFinal = DataR3[DataR3$cycle==10,]
ringFinal = ringFinal %>% group_by(group,trial_type)%>%
  summarise(Percentage_Of_Change = mean(Response))

#Plot the data
ggplot(data = ringFinal, aes(x=trial_type,y=Percentage_Of_Change,fill=group))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=round(Percentage_Of_Change,digits = 5)), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Participants Change Response toward the final ring",
       x ="", y = "% of Change Response")+
  theme_minimal()

#Find the general trend 
ringTrend = DataR3 %>% group_by(cycle,trial_type,group)%>%
  summarise(Percentage_Of_Change = mean(Response))

ggplot(ringTrend)+
  geom_line(aes(x=cycle,y=Percentage_Of_Change,col=group))+
  facet_wrap(~ trial_type)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  labs(x = "Nth Ring", y = "% of Change Response")+
  ggtitle("Participants Change Response Across the Rings")

