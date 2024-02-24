library("dplyr")
library("readr")
library(ggplot2)
library(car)
library("ggpubr")

### These are for hand-scoring the CRT test
# allString = function(x){
#   x$textbox_2.text = as.character(x$textbox_2.text)
#   x$textbox_8.text = as.character(x$textbox_8.text)
#   x$textbox_9.text = as.character(x$textbox_9.text)
#   x$textbox_10.text = as.character(x$textbox_10.text)
#   x
# }
# 
# crtValue = function(x){
#   print(unique(x$textbox_8.text)[2])
#   print(unique(x$textbox_9.text)[2])
#   print(unique(x$textbox_10.text)[2])
#   crt = readline(prompt = "What is the crt value?")
#   x = x%>%mutate(crt = case_when(
#     is.na(expName) == FALSE ~ crt))
#   x
# }
###Read and write a new file which aggregates all data and include the CRT score
# Filelist = list.files(path="C:/Users/xhmik/Desktop/Zeno Data/Grating_Colour", full.names = TRUE)
# Rawdata = lapply(Filelist,read.csv)%>%lapply(allString)%>%lapply(crtValue)%>%bind_rows()
# write.csv(Rawdata, "C:/Users/xhmik/Desktop/Zeno Data/Rawdata.csv", row.names=FALSE)

#Can only be used as part of mutate function
string2Colour = function(x){
  colourList = x%>%substr(2,nchar(x)-1)%>%strsplit(",")%>%unlist()%>%as.numeric()
  colourT = rgb((colourList[1]+1)/2,(colourList[2]+1)/2,(colourList[3]+1)/2)
  colourT
}

#Read from the new csv
Rawdata = read.csv("C:/Users/xhmik/Desktop/Zeno Data/Rawdata.csv")
#Label each groups
Rawdata = Rawdata %>% mutate(group = case_when(
  expName == "Zeno_all_resp_revised" ~ "intermediate",
  expName == "Zeno_last_resp_revised" ~ "last"
))

#Label CRT into high and low
Rawdata = Rawdata %>% mutate(crtHigh = case_when(
  crt < 2 ~ "High",
  crt >= 2 ~ "Low"
))


#Remove columns and rows

DataR1 = Rawdata[!"" == Rawdata$colour_now,]
DataR1H = DataR1[is.na(DataR1$prac_trials.thisRepN)==TRUE,]
DataR2 = DataR1H[is.na(DataR1H$cycle)==FALSE,]

#Add catch/control/first colour
CountN=1
for (i in 1:length(DataR2$Catch)){
  if (DataR2$cycle[i]==2){
    CountN = 1
  }
  else{
    DataR2$Catch[i] = DataR2$Catch[i-CountN]
    DataR2$Control[i] = DataR2$Control[i-CountN]
    DataR2$First_colour[i] = DataR2$First_colour[i-CountN]
    CountN = CountN+1
  }
} 

#Add Meta Trial
DataR2$meta_trial = DataR2$cycle
meta=1
for (i in 1:nrow(DataR2)) {
  if(i>1){
    if(DataR2$participant[i]!=DataR2$participant[i-1]){
      meta=1
    }else{
      if(DataR2$First_colour[i]!=DataR2$First_colour[i-1]){
        meta=meta+1
      }
    }
    if(DataR2$meta_trial[i]!=0){
      DataR2$meta_trial[i]=meta
    }
  }else{
    DataR2$meta_trial[i]=meta
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

#Find the general trend within trials
ringTrend = DataR3 %>% group_by(cycle,trial_type,group)%>%
  summarise(Percentage_Of_Change = mean(Response))

ggplot(ringTrend)+
  geom_line(aes(x=cycle,y=Percentage_Of_Change,col=group))+
  facet_wrap(~ trial_type)+
  scale_x_continuous(breaks = seq(0, 10, by = 1))+
  labs(x = "Nth Ring", y = "% of Change Response")+
  ggtitle("Participants Change Response Across the Rings")

#Find the general trend across trials
gradualChange = DataR3[DataR3$trial_type=="Gradual Change",]
ringTrend2 = gradualChange %>% group_by(cycle,crtHigh,group)%>%
  summarise(Percentage_Of_Change = mean(Response))

ggplot(ringTrend2)+
  geom_line(aes(x=cycle,y=Percentage_Of_Change,col=group))+
  facet_wrap(~ crtHigh)+
  scale_x_continuous(breaks = seq(0, 60, by = 5))+
  labs(x = "Nth Ring", y = "% of Change Response")+
  ggtitle("% of Change Response in Gradual Change trials divided based on CRT")

#Find the general trend across trials
ringTrend3 = DataR3 %>% group_by(meta_trial,trial_type,group)%>%
  summarise(Percentage_Of_Change = mean(Response))

ggplot(ringTrend3)+
  geom_line(aes(x=meta_trial,y=Percentage_Of_Change,col=group))+
  facet_wrap(~ trial_type)+
  scale_x_continuous(breaks = seq(0, 60, by = 5))+
  labs(x = "Nth Trial", y = "% of Change Response")+
  ggtitle("Participants Change Response Across the Rings")

#Identify participants from the last group who responded when they shouldn't have
dataC = DataR3[DataR3$trial_type=="Catch Random 50%" | DataR3$trial_type=="Catch Same 50%"| DataR3$trial_type=="Control"| DataR3$trial_type=="Gradual Change",]
dataC2 = dataC[dataC$group == "last",]
dataC3 = dataC2[dataC2$cycle!=10,]
dataC4 = dataC3[dataC3$key_resp_4.keys!="",]
print(unique(dataC4$participant))

#Index all colour pairings in gradual change trials and isolate their 10th ring response
gradualtenth = gradualChange[gradualChange$cycle == 10,]
gradualtenth = gradualtenth%>%rowwise()%>%mutate(
              colourIndexF = string2Colour(First_colour),
              colourIndexN = string2Colour(colour_now),
              colourIndexS = string2Colour(Same_colour)
              )

#Group the data for the 1 + 3 graphs
gradualCleaned = gradualtenth %>% group_by(colourIndexF,colourIndexN,colourIndexS,group)%>%
  summarise(Percentage_Of_Change = mean(Response))%>%arrange(Percentage_Of_Change)

gradualColour = gradualCleaned %>% group_by(colourIndexF,colourIndexN,colourIndexS)%>%
  summarise(Percentage_Of_Change = mean(Percentage_Of_Change))%>%arrange(Percentage_Of_Change)

#Graph the changes response for each colour index
p1 = ggplot(gradualCleaned)+
  geom_line(aes(x=reorder(colourIndexF,Percentage_Of_Change,mean),y=Percentage_Of_Change,col=group,group=group),size=2)+
  theme(legend.position="top") +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank())+
  coord_fixed(12)

#Graph the reference colour using tiles
p2 = ggplot(gradualColour, aes(x=1:nrow(gradualColour), y = 1, fill=factor(1:nrow(gradualColour)),height=0.5, width=0.5)) +
  geom_tile()+
  scale_fill_manual(values = gradualColour$colourIndexF) +
  theme_void()+
  theme(legend.position="none") +
  coord_fixed(1)

#Graph the final colour using tiles
p3 = ggplot(gradualColour, aes(x=1:nrow(gradualColour), y = 1, fill=factor(1:nrow(gradualColour)),height=0.5, width=0.5)) +
  geom_tile()+
  scale_fill_manual(values = gradualColour$colourIndexN) +
  theme_void()+
  theme(legend.position="none") +
  coord_fixed(1)

#Graph the secondary colour using tiles
p4 = ggplot(gradualColour, aes(x=1:nrow(gradualColour), y = 1, fill=factor(1:nrow(gradualColour)),height=0.5, width=0.5)) +
  geom_tile()+
  scale_fill_manual(values = gradualColour$colourIndexS) +
  theme_void()+
  theme(legend.position="none") +
  coord_fixed(1)

#Combine all 4 graphs together
ggarrange(p1,p2,p3,p4, heights = c(9, 0.7,0.7,0.7), align = "v", nrow=4)

#Create a CSV so that I can sort it in Power BI
#write.csv(gradualCleaned, "C:/Users/xhmik/Desktop/Zeno Data/ColourSort.csv", row.names=FALSE)

#Check CRT scores of the cohorts
CRTCheck = DataR3 %>% count(group,crt)
CRTCheck

