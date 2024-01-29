library(ggplot2)
library("plotly")
library("dplyr")
library("readr")
library("readxl")

#Set up translation function from PsychoPy RGB to Rstudio rgb()
toClassic = function(x){
  x =(x+1)*0.5
  x
}

#Create functions that set up empty plot
emptyFrame = function(x){
  p= plot_ly(data = x, type = 'scatter3d', mode='markers') %>%
     layout(scene = list(xaxis = list(title = 'R'),
                        yaxis = list(title = 'G'),
                        zaxis = list(title = 'B')))
  p
}

#Read Files
Filelist = list.files(path="C:/Users/xhmik/Desktop/Colours", full.names = TRUE)  
Rawdata = read_excel(Filelist[1])

#Single out colours for experimental condition 
DataExp = Rawdata[Rawdata$catch_trial==0&Rawdata$control==0,]
DataR1 = DataExp[,1:3]%>%toClassic()

#Generate colour value of the first ring
colourCom = c()
for (i in 1:nrow(DataR1)) {
  colourCom[i] = with(DataR1, rgb(r[i],g[i],b[i]))
}

#Set up empty plot
p1=emptyFrame(DataR1)

#Assign trace individually
for (i in seq_along(colourCom)){
  p1 = p1 %>% add_trace(x=DataR1$r[i], y=DataR1$g[i], z=DataR1$b[i],
                       marker = list(color = colourCom[i]),
                       name = colourCom[i])
}

#Display
p1

#Determine changes to colour and the directions, and derive the colours of final ring
DataR2 = DataR1
change = 0.49/2
colourCom2=c()
for (i in 1:nrow(DataR1)) {
  if(DataR2$r[i]+change>1){
    DataR2$r[i] = DataR2$r[i]-change
  }else{
    DataR2$r[i] = DataR2$r[i]+change
  }
  colourCom2[i] = with(DataR2, rgb(r[i],g[i],b[i]))
}

#Set up empty plot
p2=emptyFrame(DataR2)

#Assign trace individually
for (i in seq_along(colourCom2)){
  p2 = p2 %>% add_trace(x=DataR1$r[i], y=DataR1$g[i], z=DataR1$b[i],
                        marker = list(color = colourCom2[i]),
                        name = colourCom2[i])
}

#Display
p2

