library("dplyr")
library("readr")
library(ggplot2)
library(car)

selecteven = function(x,y){
  y = deparse(substitute(y))
  x = x[(x[[y]] %% 2)==0,]
  x
}

tonumeric = function(x){
  x = as.numeric(levels(x))[x]
  x
}

doubletrial = function(x){
  for (i in 1:nrow(x)){
    if (x$group[i]=="Short (1+9)"){
      x$Trials.thisN[i]=x$Trials.thisN[i]*2-1
    } 
  }
  x
}

Filelist = list.files(path="C:/Users/xhmik/Desktop/Zeno Data/Obs Manipulate/03122023", full.names = TRUE)
Filelist2 = list.files(path="C:/Users/xhmik/Desktop/Zeno Data/Obs Manipulate/26112023", full.names = TRUE)
File = Filelist2
Rawdata = lapply(File,read.csv)%>%bind_rows()

Raw1 = Rawdata[!"" == Rawdata$colour_now,]

Data = select(Raw1, First_colour, colour_now,key_resp_4.keys,Trials.thisN,Catch,Control,participant)


#Reset trial counts

for (i in 1:length(Data$Trials.thisN)){
  if (Data$First_colour[i]!=""){
    CountN = 2
    Data$Trials.thisN[i]= CountN
  }
  else{
    CountN = CountN+1
    Data$Trials.thisN[i]= CountN
  }
} 

#Add catch/control/first colour
for (i in 1:length(Data$Catch)){
  if (is.na(Data$Catch[i])==FALSE){
    CountN = 1
  }
  else{
    Data$Catch[i] = Data$Catch[i-CountN]
    Data$Control[i] = Data$Control[i-CountN]
    Data$First_colour[i] = Data$First_colour[i-CountN]
    CountN = CountN+1
  }
} 

#Label the two groups and correcting their column title

if (File[1] == Filelist[1]) {
  for (i in 1:nrow(Data)) {
    if(Data$participant[i]==616046){
      Data$participant[i] = "Short"
    }
    if(Data$participant[i]==757589){
      Data$participant[i] = "Long"
    }
  }
}else if(File[1] == Filelist2[1]){
  for (i in 1:nrow(Data)) {
    if(Data$participant[i]==599691){
      Data$participant[i] = "Short"
    }
    if(Data$participant[i]==594346){
      Data$participant[i] = "Long"
    }
  }
  }
Data = Data%>%rename("group"="participant")

#Create R changes for each group
Data$Rchanges = Data$Trials.thisN
for (i in 1:nrow(Data)) {
  if(Data$group[i]=="Short"){
    Data$Rchanges[i] = (Data$Trials.thisN[i]-1)*0.07
  }
  if(Data$group[i]=="Long"){
    Data$Rchanges[i] = (Data$Trials.thisN[i]-1)*0.035
  }
}

#Single out normal trials
Data1 = Data[Data$Catch==0,]
DataNormal = Data1[Data1$Control==0,]

#Catch Trials
DataCatch = Data[Data$Catch == 1,]

#Control Trials
DataControl = Data[Data$Control == 1,]

#Remove no response
DataNormal2 = DataNormal[DataNormal$key_resp_4.keys!="",]
DataCatch = DataCatch[DataCatch$key_resp_4.keys!="",]
DataControl = DataControl[DataControl$key_resp_4.keys!="",]

#Separate Catch into Change and No change
DataCatchN = DataCatch[DataCatch$First_colour==DataCatch$colour_now,]
DataCatchC = DataCatch[DataCatch$First_colour!=DataCatch$colour_now,]

#Separate them into Long and short Groups
DataLN = DataNormal2[DataNormal2$group=="Long",]
DataSN = DataNormal2[DataNormal2$group=="Short",]
DataLCaN = DataCatchN[DataCatchN$group=="Long",]
DataSCaN = DataCatchN[DataCatchN$group=="Short",]
DataLCaC = DataCatchC[DataCatchC$group=="Long",]
DataSCaC = DataCatchC[DataCatchC$group=="Short",]
DataLCo = DataControl[DataControl$group=="Long",]
DataSCo = DataControl[DataControl$group=="Short",]

#Count C and N response toward each R changes for normal trial
DataLN2 = with(DataLN,table(key_resp_4.keys,Rchanges))%>%prop.table(margin=2)%>%as.data.frame()
DataSN2 = with(DataSN,table(key_resp_4.keys,Rchanges))%>%prop.table(margin=2)%>%as.data.frame()
DataLCaN2 = with(DataLCaN,table(key_resp_4.keys,Trials.thisN))%>%prop.table(margin=2)%>%as.data.frame()
DataSCaN2 = with(DataSCaN,table(key_resp_4.keys,Trials.thisN))%>%prop.table(margin=2)%>%as.data.frame()
DataLCaC2 = with(DataLCaC,table(key_resp_4.keys,Trials.thisN))%>%prop.table(margin=2)%>%as.data.frame()
DataSCaC2 = with(DataSCaC,table(key_resp_4.keys,Trials.thisN))%>%prop.table(margin=2)%>%as.data.frame()
DataLCo2 = with(DataLCo,table(key_resp_4.keys,Trials.thisN))%>%prop.table(margin=2)%>%as.data.frame()
DataSCo2 = with(DataSCo,table(key_resp_4.keys,Trials.thisN))%>%prop.table(margin=2)%>%as.data.frame()
  
  
#Add back in the group label
DataLN2$group = DataLN2$Freq
long_name = "Long (1+16)"
short_name = "Short (1+9)"
for (i in 1:nrow(DataLN2)) {
  DataLN2$group[i] = long_name
}

DataSN2$group = DataSN2$Freq
for (i in 1:nrow(DataSN2)) {
  DataSN2$group[i] = short_name
}

DataLCaN2$group = DataLCaN2$Freq
for (i in 1:nrow(DataLCaN2)) {
  DataLCaN2$group[i] = long_name
}

DataSCaN2$group = DataSCaN2$Freq
for (i in 1:nrow(DataSCaN2)) {
  DataSCaN2$group[i] = short_name
}

DataLCaC2$group = DataLCaC2$Freq
for (i in 1:nrow(DataLCaC2)) {
  DataLCaC2$group[i] = long_name
}

DataSCaC2$group = DataSCaC2$Freq
for (i in 1:nrow(DataSCaC2)) {
  DataSCaC2$group[i] = short_name
}

DataLCo2$group = DataLCo2$Freq
for (i in 1:nrow(DataLCo2)) {
  DataLCo2$group[i] = long_name
}

DataSCo2$group = DataSCo2$Freq
for (i in 1:nrow(DataSCo2)) {
  DataSCo2$group[i] = short_name
}

#Only select C response since we are only interested in % of change
Nplot = rbind(DataLN2,DataSN2)
CaNplot = rbind(DataLCaN2,DataSCaN2)
CaCplot = rbind(DataLCaC2,DataSCaC2)
Coplot = rbind(DataLCo2,DataSCo2)
Nplot = Nplot[Nplot$key_resp_4.keys=="c",]
CaNplot = CaNplot[CaNplot$key_resp_4.keys=="c",]
CaCplot = CaCplot[CaCplot$key_resp_4.keys=="c",]
Coplot = Coplot[Coplot$key_resp_4.keys=="c",]
Nplot$Rchanges = tonumeric(Nplot$Rchanges)
CaNplot$Trials.thisN = tonumeric(CaNplot$Trials.thisN)
CaCplot$Trials.thisN = tonumeric(CaCplot$Trials.thisN)
Coplot$Trials.thisN = tonumeric(Coplot$Trials.thisN)


#Plot Out the graph
ggplot(Nplot)+
  geom_line(aes(x=Rchanges,y=Freq,col=group,group=group))+
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.05))+
  labs(x = "Changes in R", y = "% of Change Response")+
  ggtitle("2nd Experiment 9 vs. 16 (Experimental Trial)")

ggplot(CaCplot)+
  geom_line(aes(x=Trials.thisN,y=Freq,col=group,group=group))+
  scale_x_continuous(breaks = seq(2, 17, by = 1))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "nth Rings", y = "% of Change Response")+
  ggtitle("2nd Experiment 9 vs. 16 (50% Random Changed)")

ggplot(CaNplot)+
  geom_line(aes(x=Trials.thisN,y=Freq,col=group,group=group))+
  scale_x_continuous(breaks = seq(2, 17, by = 1))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "nth Rings", y = "% of Change Response")+
  ggtitle("2nd Experiment 9 vs. 16 (50% Random No-Change)")

ggplot(Coplot)+
  geom_line(aes(x=Trials.thisN,y=Freq,col=group,group=group),position = position_dodge(width = 0.2))+
  scale_x_continuous(breaks = seq(2, 17, by = 1))+
  scale_y_continuous(limits = c(0,1))+
  labs(x = "nth Rings", y = "% of Change Response")+
  ggtitle("2nd Experiment 9 vs. 16 (Control)")

