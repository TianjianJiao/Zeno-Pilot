library("dplyr")
library("readr")
library(ggplot2)
library(car)
Filelist = list.files(path="C:/Users/xhmik/Desktop/Zeno Data/Random Dot", full.names = TRUE)
Rawdata = lapply(Filelist,read.csv)%>%bind_rows()

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

#Separate into AllQ and LastQ
DataLast= Data[Data$participant==918168,]
DataAll = Data[Data$participant==191376,]
DataR3 = DataLast

#Find all the normal trials
Data1 = DataR3[0==DataR3$Catch,]
DataNormal = Data1[0==Data1$Control,]
#DataNormal = subset(DataNormal, Trials.thisN==3|Trials.thisN==5|Trials.thisN==7|Trials.thisN==9|Trials.thisN==11|Trials.thisN==13|Trials.thisN==15)

#Find all the catch/control trials
DataControl = DataR3[1==DataR3$Control,]
DataCatch = DataR3[1==DataR3$Catch,]

#Bar plot for normal trials
DataNormal2 = prop.table(with(DataNormal,table(key_resp_4.keys,Trials.thisN)),margin = 2)
barplot(DataNormal2,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataNormal2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)


#Bar plot for control trials
DataControl2 = prop.table(with(DataControl,table(key_resp_4.keys,Trials.thisN)),margin = 2)
barplot(DataControl2,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataControl2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)


#Bar plot for catch trials
DataCatchN = DataCatch[DataCatch$First_colour==DataCatch$colour_now,]
DataCatchC = DataCatch[DataCatch$First_colour!=DataCatch$colour_now,]
DataCatchN2 = with(DataCatchN,table(key_resp_4.keys,Trials.thisN))
DataCatchC2 = with(DataCatchC,table(key_resp_4.keys,Trials.thisN))
barplot(DataCatchN2,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataCatchN2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)
barplot(DataCatchC2,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataCatchC2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)

#Percentage Barplot for the catch trials
DataCatchC3=prop.table(DataCatchC2, margin = 2)
DataCatchN3=prop.table(DataCatchN2, margin = 2)
barplot(DataCatchN3,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"),
        legend = rownames(DataCatchN2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)
barplot(DataCatchC3,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataCatchC2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)

#Preparing for a T-test
DataAll2 = DataAll[DataAll$Trials.thisN==10,]
DataLast2 = DataLast[DataLast$Trials.thisN==10,]

#Normal Trials
DataAllN = DataAll2[0==DataAll2$Catch,]
DataAllN2 = DataAllN[0==DataAllN$Control,]
DataLastN = DataLast2[0==DataLast2$Catch,]
DataLastN2 = DataLastN[0==DataLastN$Control,]

#Comparing Last and All group for normal trials
NormalG = rbind(DataAllN2,DataLastN2)
NormalG2 = with(NormalG,table(participant,key_resp_4.keys))%>%prop.table(margin = 1)%>%as.data.frame()
NormalG2$key_resp_4.keys = factor(NormalG2$key_resp_4.keys,level = c("","n","c"))
ggplot(NormalG2, aes(fill=key_resp_4.keys, y=Freq, x=participant)) + 
  geom_bar(position = "stack", stat="identity")+
  labs(title = "Participants responses toward the Last Ring (Normal)",y="% of Responses",x="Experiment Group",fill="Types of Responses")+
  scale_fill_manual(values=c("yellow",
                             "red",
                             "darkblue")) 

