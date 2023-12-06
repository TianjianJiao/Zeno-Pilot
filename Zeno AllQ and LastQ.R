library("dplyr")
library("readr")
library(ggplot2)
library(car)

Filelist = list.files(path="C:/Users/xhmik/Desktop/Zeno Data/First Pilot", full.names = TRUE)  
Rawdata = lapply(Filelist,read.csv)%>%bind_rows()

#Remove columns and rows

DataR1 = Rawdata[!"" == Rawdata$colour_now,]
DataR2 = DataR1[is.na(DataR1$Prac_Trials.thisIndex)==TRUE,]

#Add trial counts
DataR2$trial = DataR2$key_resp_4.rt

for (i in 1:length(DataR2$trial)){
  if (DataR2$First_colour[i]!=""){
    CountN = 2
    DataR2$trial[i]= CountN
  }
  else{
    CountN = CountN+1
    DataR2$trial[i]= CountN
  }
} 

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

#Add Meta Trial
DataR2$meta_trial = DataR2$trial
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

#Separate into AllQ and LastQ
DataAll = DataR2[DataR2$expName=="Zeno_Effect_AllQ",]
DataLast =DataR2[DataR2$expName=="Zeno_Effect_LastQ",]
DataR3 = DataAll

#Find all the normal trials
Data1 = DataR3[0==DataR3$Catch,]
DataNormal = Data1[0==Data1$Control,]

#Find all the catch/control trials
DataControl = DataR3[1==DataR3$Control,]
DataCatch = DataR3[1==DataR3$Catch,]

#Bar plot for normal trials
DataNormal2 = prop.table(with(DataNormal,table(key_resp_4.keys,trial)),margin = 2)
barplot(DataNormal2,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataNormal2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)
DataNormal3 = DataNormal[DataNormal$trial==10,]
DataMetaN = prop.table(with(DataNormal3,table(key_resp_4.keys,meta_trial)),margin = 2)
barplot(DataMetaN,main="Participants' Responses toward the last ring across the 60 trials",
        xlab="Trials", col=c("yellow","darkblue","red"), 
        legend = rownames(DataNormal2),
        args.legend = list(x = "topright",inset = c(-0.06, -0.04)),
        beside=FALSE)


#Bar plot for control trials
DataControl2 = prop.table(with(DataControl,table(key_resp_4.keys,trial)),margin = 2)
barplot(DataControl2,main="Participants' Responses for Each Ring",
        xlab="Ring", col=c("yellow","darkblue","red"), 
        legend = rownames(DataControl2),
        args.legend = list(x = "topright",inset = c(-0.07, -0.04)),
        beside=FALSE)


#Bar plot for catch trials
DataCatchN = DataCatch[DataCatch$First_colour==DataCatch$colour_now,]
DataCatchC = DataCatch[DataCatch$First_colour!=DataCatch$colour_now,]
DataCatchN2 = with(DataCatchN,table(key_resp_4.keys,trial))
DataCatchC2 = with(DataCatchC,table(key_resp_4.keys,trial))
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
DataAll2 = DataAll[DataAll$trial==10,]
DataLast2 = DataLast[DataLast$trial==10,]

#Normal Trials
DataAllN = DataAll2[0==DataAll2$Catch,]
DataAllN2 = DataAllN[0==DataAllN$Control,]
DataLastN = DataLast2[0==DataLast2$Catch,]
DataLastN2 = DataLastN[0==DataLastN$Control,]

#Normal T-test
DataAllN3 = prop.table(with(DataAllN2,table(participant,key_resp_4.keys)),margin = 1)%>%as.data.frame.matrix()
DataLastN3 = prop.table(with(DataLastN2,table(participant,key_resp_4.keys)),margin = 1)%>%as.data.frame.matrix()
t.test(DataAllN3$c,DataLastN3$c,paired=FALSE)

#Add in Group and Condition for Normal Trials
DataAllN3$group = rep("All",nrow(DataAllN3))
DataAllN3$condition = rep("Normal",nrow(DataAllN3))
DataLastN3$group = rep("Last",nrow(DataLastN3))
DataLastN3$condition = rep("Normal",nrow(DataLastN3))

#Catch Trials
DataAllC = DataAll2[1==DataAll2$Catch,]
DataLastC = DataLast2[1==DataLast2$Catch,]

DataAllCN = DataAllC[DataAllC$First_colour==DataAllC$colour_now,]
DataAllCC = DataAllC[DataAllC$First_colour!=DataAllC$colour_now,]
DataLastCN = DataLastC[DataLastC$First_colour==DataLastC$colour_now,]
DataLastCC = DataLastC[DataLastC$First_colour!=DataLastC$colour_now,]

#Catch Trial Percentage form
DataAllCN2 = prop.table(with(DataAllCN,table(participant,key_resp_4.keys)),margin=1)%>%as.data.frame.matrix()
DataAllCC2 = prop.table(with(DataAllCC,table(participant,key_resp_4.keys)),margin=1)%>%as.data.frame.matrix()
DataLastCN2 = prop.table(with(DataLastCN,table(participant,key_resp_4.keys)),margin=1)%>%as.data.frame.matrix()
DataLastCC2 = prop.table(with(DataLastCC,table(participant,key_resp_4.keys)),margin=1)%>%as.data.frame.matrix()

#Catch T-test

t.test(DataAllCN2$c,DataLastCN2$c,paired=FALSE)
t.test(DataAllCC2$c,DataLastCC2$c,paired=FALSE)

#Add in Group and Condition for Catch Trials
DataAllCN2$group = rep("All",nrow(DataAllCN2))
DataAllCN2$condition = rep("Catch_NoChange",nrow(DataAllCN2))
DataLastCN2$group = rep("Last",nrow(DataLastCN2))
DataLastCN2$condition = rep("Catch_NoChange",nrow(DataLastCN2))
DataAllCC2$V1 = rep(0,nrow(DataAllCC2))
DataAllCC2$group = rep("All",nrow(DataAllCC2))
DataAllCC2$condition = rep("Catch_Changed",nrow(DataAllCC2))
DataLastCC2$group = rep("Last",nrow(DataLastCC2))
DataLastCC2$condition = rep("Catch_Changed",nrow(DataLastCC2))

#Control Trial
DataAllCon = DataAll2[1==DataAll2$Control,]
DataLastCon = DataLast2[1==DataLast2$Control,]

#Control T-test
DataAllCon2 = prop.table(with(DataAllCon,table(participant,key_resp_4.keys)),margin = 1)%>%as.data.frame.matrix()
DataLastCon2 = prop.table(with(DataLastCon,table(participant,key_resp_4.keys)),margin = 1)%>%as.data.frame.matrix()
t.test(DataAllCon2$c,DataLastCon2$c,paired=FALSE)

#Add in Group and Condition for Control Trials
DataAllCon2$group = rep("All",nrow(DataAllCon2))
DataAllCon2$condition = rep("Control",nrow(DataAllCon2))
DataLastCon2$group = rep("Last",nrow(DataLastCon2))
DataLastCon2$condition = rep("Control",nrow(DataLastCon2))

#Within-Group Comparison between Control and Catch-No_change
DataAllCon3 = DataAll2[1==DataAll2$Control,]
DataLastCon3 = DataLast2[1==DataLast2$Control,]

DataAllCon4 = prop.table(with(DataAllCon3,table(participant,key_resp_4.keys)),margin=1)%>%as.data.frame.matrix()
DataLastCon4 = prop.table(with(DataLastCon3,table(participant,key_resp_4.keys)),margin=1)%>%as.data.frame.matrix()

t.test(DataLastCN2$c,DataLastCon4$c,paired=FALSE)
t.test(DataAllCon4$c,DataAllCN2$c,paired=FALSE)

#Combine All the 8 Data Frames for ANOVA
DataANOVA = rbind(DataAllN3,DataLastN3,DataAllCon2,DataLastCon2,DataAllCC2,DataLastCC2,DataAllCN2,DataLastCN2)
DataANOVA$group = as.factor(DataANOVA$group)
DataANOVA$condition = as.factor(DataANOVA$condition)

#ANOVA with Interaction and a Custom Contract
contrastCon = contrasts(DataANOVA$condition)
contrastCon[,1]=c(-1/3,-1/3,-1/3,1)
contrastCon[,2]=c(0,-0.5,-0.5,1)
contrastCon[,3]=c(0,1,-1,0)
contrasts(DataANOVA$condition) = contrastCon
contrasts(DataANOVA$group) = c(1,-1)
Result = lm(c ~ group*condition,data = DataANOVA)
summary(Result)
Anova(Result,type="III")

#Comparing Last and All group for normal trials

NormalG = rbind(DataAllN2,DataLastN2)
NormalG2 = with(NormalG,table(expName,key_resp_4.keys))%>%prop.table(margin = 1)%>%as.data.frame()
NormalG2$key_resp_4.keys = factor(NormalG2$key_resp_4.keys,level = c("","n","c"))
ggplot(NormalG2, aes(fill=key_resp_4.keys, y=Freq, x=expName)) + 
  geom_bar(position = "stack", stat="identity")+
  labs(title = "Participants responses toward the Last Ring (Normal)",y="% of Responses",x="Experiment Group",fill="Types of Responses")+
  scale_fill_manual(values=c("yellow",
                             "red",
                             "darkblue")) 

#Comparing Last and All group for control trials

ControlG = rbind(DataAllCon,DataLastCon)
ControlG2 = with(ControlG,table(expName,key_resp_4.keys))%>%prop.table(margin = 1)%>%as.data.frame()
ControlG2$key_resp_4.keys = factor(ControlG2$key_resp_4.keys,level = c("","n","c"))
ggplot(ControlG2, aes(fill=key_resp_4.keys, y=Freq, x=expName)) + 
  geom_bar(position="stack", stat="identity")+
  labs(title = "Participants responses toward the Last Ring (Control)",y="% of Responses",x="Experiment Group",fill="Types of Responses")+
  scale_fill_manual(values=c("yellow",
                             "red",
                             "darkblue")) 
#Comparing Last and All group for the catch trials

CatchNG = rbind(DataAllCN,DataLastCN)
CatchNG2 = with(CatchNG,table(expName,key_resp_4.keys))%>%prop.table(margin = 1)%>%as.data.frame()
CatchNG2$key_resp_4.keys = factor(CatchNG2$key_resp_4.keys,level = c("","n","c"))
ggplot(CatchNG2, aes(fill=key_resp_4.keys, y=Freq, x=expName)) + 
  geom_bar(position="stack", stat="identity")+
  labs(title = "Participants responses toward the Last Ring (Random No-Change)",y="% of Responses",x="Experiment Group",fill="Types of Responses")+
  scale_fill_manual(values=c("yellow",
                             "red",
                             "darkblue"))  

CatchCG = rbind(DataAllCC,DataLastCC)
CatchCG2 = with(CatchCG,table(expName,key_resp_4.keys))%>%prop.table(margin = 1)%>%as.data.frame()
CatchCG2$key_resp_4.keys = factor(CatchCG2$key_resp_4.keys,level = c("","n","c"))
ggplot(CatchCG2, aes(fill=key_resp_4.keys, y=Freq, x=expName)) + 
  geom_bar(position="stack", stat="identity")+
  labs(title = "Participants responses toward the Last Ring (Random Change)",y="% of Responses",x="Experiment Group",fill="Types of Responses")+
  scale_fill_manual(values=c("yellow",
                             "red",
                             "darkblue")) 

#Bar Graph Based on First Colour in Normal Trials

ColourAll = prop.table(with(DataAllN2,table(First_colour,key_resp_4.keys)),margin = 1)%>%as.data.frame()
ColourLast = prop.table(with(DataLastN2,table(First_colour,key_resp_4.keys)),margin = 1)%>%as.data.frame()
ColourAll2 = ColourAll[ColourAll$key_resp_4.keys=="c",]
colnames(ColourAll2) = c("First_colour", "Resp_All","Freq_All")
ColourLast2 = ColourLast[ColourLast$key_resp_4.keys=="c",]
colnames(ColourLast2) = c("First_colour", "Resp_Last","Freq_Last")
ColourDiff = merge(ColourAll2,ColourLast2,by="First_colour")
ColourDiff$diff = ColourDiff$Freq_All - ColourDiff$Freq_Last
ggplot(ColourDiff, aes(y=diff, x=First_colour)) + 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_blank())


