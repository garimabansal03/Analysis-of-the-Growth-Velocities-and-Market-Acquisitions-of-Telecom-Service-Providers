#PACKAGES
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(RColorBrewer)
coul=brewer.pal(5,"Set2")

#NORMALIZATION
norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


# TO CALCULATE AVG GROWTH RATE ---------weights alloted( 0.5, 0.25, 0.125,.........)
weight=(rev(c(0.5^(1:37))))

#__________________________________________________________________________________________

#TOTAL TELECOM SUBSCRIBERS(MONTHLY CIRCLE WISE)
total_subscribers=read.csv(file.choose())

#TOTAL AVERAGE NO. OF SUBSCRIBERS(CIRCLE WISE)
t_sub=data.frame(total_subscribers$Circle)
t_sub[2]=cbind(rowSums(total_subscribers[,-1]))/38
t_sub[2]=signif(t_sub[2]/1000000,4)
barplot(t_sub$V2~t_sub$total_subscribers.Circle,las=2,cex.names=0.6,xlab=" ",ylab="No. of subscribers (IN MILLIONS)",col=coul)

#_________________________________________________________________________________________________________________
#AIRTEL
airtel=read.csv(file.choose())

#GROWTH RATE
df=data.frame(airtel$Circle)
for (i in 1:37){
  df[i+1]=((airtel[i+2]-airtel[i+1])/airtel[i+1])*1000
}
A_growth_rate=df

df=data.frame(airtel$Circle)
for (i in 1:22){
                  a=vector(length=37)
                  for(j in 1:37){
                                  a[j]=(weight[j]*A_growth_rate[i,j+1])     }
                  a=sum(a)
                  df[i,2]=a 
}
Airtel_growth=df
colnames(Airtel_growth)=c("Circle","AVG_GROWTH_RATE")

RANK_Airtel_Growth= cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_Airtel_Growth)=c("Circle","AVG_GROWTH_RATE","RANK")

#MARKET ACQUISITION
#TOTAL AVERAGE NO. OF AIRTEL SUBSCRIBERS(CIRCLE WISE)
airtel_sub=data.frame(airtel$Circle)
airtel_sub[2]=cbind(rowSums(airtel[,-1]))/38
airtel_sub[2]=signif(airtel_sub[2]/1000000,4)

df=data.frame(airtel$Circle)

for (i in 1:22) {   df[i,2]=(airtel_sub[i,2]/t_sub[i,2])*100  }

Airtel_market_acq=df
colnames(Airtel_market_acq)=c("Circle","MARKET ACQUISITION")

RANK_Airtel_market_acq=cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_Airtel_market_acq)=c("Circle","MARKET ACQUISITION","RANK")

Airtel_data = as.data.frame(cbind(Airtel_growth$AVG_GROWTH_RATE,Airtel_market_acq$`MARKET ACQUISITION`))
rownames(Airtel_data)=c(airtel$Circle)
colnames(Airtel_data)=c("growth_rate","MARKET ACQUISITION")

par(mfrow=c(2,1))

a=Airtel_data[order(-Airtel_data$growth_rate),]
names=rownames(a)
barplot(a$growth_rate,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Month on month Growth (per 1000)",col=coul)

a=Airtel_data[order(-Airtel_data$`MARKET ACQUISITION`),]
names=rownames(a)
barplot(a$`MARKET ACQUISITION`,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Current Market Share(%)",col=coul)

#AIRTEL AVG Market Acquisition
airtel_avg_market_acq=mean(Airtel_market_acq$`MARKET ACQUISITION`)

#_________________________________________________________________________________________________________________
#JIO
jio=read.csv(file.choose())

#GROWTH RATE
df=data.frame(jio$Circle)
for (i in 1:37){
  df[i+1]=((jio[i+2]-jio[i+1])/jio[i+1])*1000
}
Jio_growth_rate=df

df=data.frame(jio$Circle)
for (i in 1:22){
  a=vector(length=37)
  for(j in 1:37){
    a[j]=(weight[j]*Jio_growth_rate[i,j+1])     }
  a=sum(a)
  df[i,2]=a 
}
Jio_growth=df
colnames(Jio_growth)=c("Circle","AVG_GROWTH_RATE")

#RANK
RANK_Jio_Growth= cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_Jio_Growth)=c("Circle","AVG_GROWTH_RATE","RANK")

#MARKET ACQUISITION
#TOTAL AVERAGE NO. OF JIO SUBSCRIBERS(CIRCLE WISE)
jio_sub=data.frame(jio$Circle)
jio_sub[2]=cbind(rowSums(jio[,-1]))/38
jio_sub[2]=signif(jio_sub[2]/1000000,4)

df=data.frame(jio$Circle)
for (i in 1:22){ 
  df[i,2]=((jio_sub[i,2])/(t_sub[i,2]))*100 }
Jio_market_acq=df 
colnames(Jio_market_acq)=c("Circle","MARKET ACQUISITION")

#RANK
RANK_Jio_market_acq=cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_Jio_market_acq)=c("Circle","PER_SHARE","RANK")

Jio_data = as.data.frame(cbind(Jio_growth$AVG_GROWTH_RATE,Jio_market_acq$`MARKET ACQUISITION`))
rownames(Jio_data)=c(jio$Circle)
colnames(Jio_data)=c("growth_rate","MARKET ACQUISITION")

par(mfrow=c(2,1))

a=Jio_data[order(-Jio_data$growth_rate),]
names=rownames(a)
barplot(a$growth_rate,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Month on month Growth (per 1000)",col=coul)

a=Jio_data[order(-Jio_data$`MARKET ACQUISITION`),]
names=rownames(a)
barplot(a$`MARKET ACQUISITION`,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Current Market Share(%)",col=coul)

#JIO AVG market_acq
jio_avg_market_acq=mean(Jio_market_acq$`MARKET ACQUISITION`)

#_________________________________________________________________________________________________________________
#VODAFONE IDEA
voda=read.csv(file.choose())

#GROWTH RATE
df=data.frame(voda$Circle)
for (i in 1:37){
  df[i+1]=((voda[i+2]-voda[i+1])/voda[i+1])*1000
}
voda_growth_rate=df

df=data.frame(voda$Circle)
for (i in 1:22){
  a=vector(length=37)
  for(j in 1:37){
    a[j]=(weight[j]*voda_growth_rate[i,j+1])     }
  a=sum(a)
  df[i,2]=a 
}
voda_growth=df
colnames(voda_growth)=c("Circle","AVG_GROWTH_RATE")

#RANK
RANK_voda_Growth= cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_voda_Growth)=c("Circle","AVG_GROWTH_RATE","RANK")

#MARKET ACQUISITION
#TOTAL AVERAGE NO. OF VODA SUBSCRIBERS(CIRCLE WISE)
voda_sub=data.frame(voda$Circle)
voda_sub[2]=cbind(rowSums(voda[,-1]))/38
voda_sub[2]=signif(voda_sub[2]/1000000,4)

df=data.frame(voda$Circle)
for (i in 1:22) {
  df[i,2]=((voda_sub[i,2])/(t_sub[i,2]))*100}
voda_market_acq=df
colnames(voda_market_acq)=c("Circle","MARKET ACQUISITION")

#RANK
RANK_voda_market_acq=cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_voda_market_acq)=c("Circle","MARKET ACQUISITION","RANK")

voda_data = as.data.frame(cbind(voda_growth$AVG_GROWTH_RATE,voda_market_acq$`MARKET ACQUISITION`))
rownames(voda_data)=c(voda$Circle)
colnames(voda_data)=c("growth_rate","MARKET ACQUISITION")

par(mfrow=c(2,1))

a=voda_data[order(-voda_data$growth_rate),]
names=rownames(a)
barplot(a$growth_rate,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Month on month Growth (per 1000)",col=coul)

a=voda_data[order(-voda_data$`MARKET ACQUISITION`),]
names=rownames(a)
barplot(a$`MARKET ACQUISITION`,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Current Market Share(%)",col=coul)

#VODA AVG MARKET ACQUISITION
voda_avg_market_acq=mean(voda_market_acq$`MARKET ACQUISITION`)

#_________________________________________________________________________________________________________________
#BSNL/MTNL----------------------------MTNL	DELHI AND MUMBAI	--------------BSNL	REST ALL	
mb=read.csv(file.choose())

#GROWTH RATE
df=data.frame(mb$Circle)
for (i in 1:37){
  df[i+1]=((mb[i+2]-mb[i+1])/mb[i+1])*1000
}
mb_growth_rate=df

df=data.frame(mb$Circle)
for (i in 1:22){
  a=vector(length=37)
  for(j in 1:37){
    a[j]=(weight[j]*mb_growth_rate[i,j+1])     }
  a=sum(a)
  df[i,2]=a 
}
mb_growth=df
colnames(mb_growth)=c("Circle","AVG_GROWTH_RATE")

#RANK
RANK_mb_Growth= cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_mb_Growth)=c("Circle","AVG_GROWTH_RATE","RANK")

#MARKET ACQUISITION
#TOTAL AVERAGE NO. OF MTNL/BSNL SUBSCRIBERS(CIRCLE WISE)
mb_sub=data.frame(mb$Circle)
mb_sub[2]=cbind(rowSums(mb[,-1]))/38
mb_sub[2]=signif(mb_sub[2]/1000000,4)

df=data.frame(mb$Circle)
for (i in 1:22) {
  df[i,2]=((mb_sub[i,2])/(t_sub[i,2]))*100}
mb_market_acq=df
colnames(mb_market_acq)=c("Circle","MARKET ACQUISITION")

#RANK
RANK_mb_market_acq=cbind(arrange(df, desc(df$V2)),1:22)
colnames(RANK_mb_market_acq)=c("Circle","MARKET ACQUISITION","RANK")

mb_data = as.data.frame(cbind(mb_growth$AVG_GROWTH_RATE,mb_market_acq$`MARKET ACQUISITION`))
rownames(mb_data)=c(mb$Circle)
colnames(mb_data)=c("growth_rate","MARKET ACQUISITION")

par(mfrow=c(2,1))

a=mb_data[order(-mb_data$growth_rate),]
names=rownames(a)
barplot(a$growth_rate,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Month on month Growth (per 1000)",col=coul)

a=mb_data[order(-mb_data$`MARKET ACQUISITION`),]
names=rownames(a)
barplot(a$`MARKET ACQUISITION`,names.arg=names,las=2,cex.names=0.6,xlab=" ",ylab="Current Market Share(%)",col=coul)

#MTNL/BSNL AVG MARKET ACQUISITION
mb_avg_market_acq=mean(mb_market_acq$`MARKET ACQUISITION`)
#____________________________________________________________________________________________________
#MARKET ACQUISITION AT A GLANCE
#____________________________________________________________________________________________________

Circle_summary=cbind(Airtel_market_acq$`MARKET ACQUISITION`,Jio_market_acq$`MARKET ACQUISITION`,voda_market_acq$`MARKET ACQUISITION`,mb_market_acq$`MARKET ACQUISITION`)
row.names(Circle_summary)=airtel$Circle
colnames(Circle_summary)=cbind("  AIRTEL  ","  JIO","  VODA IDEA  ","  MTNL/BSNL")
Circle_summary

#cbind(rowSums(Circle_summary))


operator_summary=cbind(jio_avg_market_acq,airtel_avg_market_acq,voda_avg_market_acq,mb_avg_market_acq)
barplot(operator_summary,names.arg = c("Jio","Airtel","VodaFone_Idea","MTNL_BSNL"),ylab="Market_Share(in %)",col=coul)

#___________________________________________________________________________________________________
#To CHECK THE SUM OF GROWTH
#______________________________________________________________________________________________________________

AIRTEL=sum(RANK_Airtel_Growth$AVG_GROWTH_RATE/1000)
JIO=sum(RANK_Jio_Growth$AVG_GROWTH_RATE/1000)
VI=sum(RANK_voda_Growth$AVG_GROWTH_RATE/1000)
MB=sum(RANK_mb_Growth$AVG_GROWTH_RATE/1000)
SGR=cbind(AIRTEL,JIO,VI,MB)
SGR

#___________________________________________________________________________________________________________________
#SATURATION  -  #max of avg (JAN21 TO FEB24) market capture--------Operators
#____________________________________________________________________________________________________________________

#MAX(Months_for each circle)-Operator wise data -------------------MAX------max/avg. of t_sub *100
sat=as.data.frame(airtel$Circle)
usat=as.data.frame(airtel$Circle)

a=c()
b=c()
c=c()
d=c()

for(i in 1:22){
                  for(j in 1:38){
                                   a[j]=airtel[i,j+1]
                                   b[j]=jio[i,j+1]
                                   c[j]=voda[i,j+1]
                                   d[j]=mb[i,j+1] }
                  a=max(a)
                  b=max(b)
                  c=max(c)
                  d=max(d)
                  sat[i,2]=max(a,b,c,d)/1000000 }

usat[,2]=sat[,2]*1000000
sat[,2]=(sat[,2]/t_sub[,2])*100
sat[,2]=sat[,2]+0.05*sat[,2]

barplot(sat$V2~sat$`airtel$Circle`,las=2,cex.names=0.6,xlab=" ",ylab="MARKET SATURATION (IN %)",col=coul)
#-------------------------------------------------------------------------------------------

#market capture/saturation --  HOW MUCH CLOSE TO SATURATION

#Airtel
Airtel_performance=as.data.frame(airtel$Circle)
Airtel_performance[2]=(Airtel_market_acq$`MARKET ACQUISITION`/sat$V2)

#Jio
Jio_performance=as.data.frame(jio$Circle)
Jio_performance[2]=(Jio_market_acq$`MARKET ACQUISITION`/sat$V2)

#Voda Idea
Voda_performance=as.data.frame(voda$Circle)
Voda_performance[2]=cbind((voda_market_acq$`MARKET ACQUISITION`/sat$V2))

#MTNL/BSNL
MB_performance=as.data.frame(mb$Circle)
MB_performance[2]=(mb_market_acq$`MARKET ACQUISITION`/sat$V2)



Airtel_final=data.frame(Airtel_performance,Airtel_data$growth_rate)
colnames(Airtel_final)=c("Circle","Current_Positioning","Growth_Rate")

Jio_final=data.frame(Jio_performance,Jio_data$growth_rate)
colnames(Jio_final)=c("Circle","Current_Positioning","Growth_Rate")

Voda_final=data.frame(Voda_performance,voda_data$growth_rate)
colnames(Voda_final)=c("Circle","Current Positioning","Growth Rate")

MB_final=data.frame(MB_performance,mb_data$growth_rate)
colnames(MB_final)=c("Circle","Current Positioning","Growth Rate")

#___________________________________________________________________________________________________________________________________
#-------------------------------------------------------------------------------------------------------
#DECISION METRIC


#   GROWTH  RANK   Current Positioning (MARKET ACQUISITION/MARKET SATURATION )        

#    +ve       5    above 75%  ------- Doing good, no need to interfere
#              4    45-75%             Need to change some strategies for fast growth(OTT Subscriptions, Low cost plans)  
#              3    below 45%          More investment is needed (To create awareness- advertisements)

#    -ve       1    above 75%  ------- Immediate attention is required(More Towers to be installed, OTT Subscriptions, Low cost plans)           
#              2    45-75%             More investment is needed (More Towers to be installed)
#              6    below 45%          Leave it, Focus on above 
#____________________________________________________________________________________________________________________________________
#                                            NORMALIZATION
#____________________________________________________________________________________________________________________________________

#Airtel
Airtel_norm= as.data.frame(lapply(Airtel_final[2:3],norm))
Airtel_norm=data=cbind(airtel$Circle,Airtel_norm)
Ax_1 <- (0.45-min(Airtel_final$Current_Positioning))/(max(Airtel_final$Current_Positioning)-min(Airtel_final$Current_Positioning))
Ax_2 <- (0.75-min(Airtel_final$Current_Positioning))/(max(Airtel_final$Current_Positioning)-min(Airtel_final$Current_Positioning))
Ay <- (0-min(Airtel_final$Growth_Rate))/(max(Airtel_final$Growth_Rate)-min(Airtel_final$Growth_Rate))

#JIO
Jio_norm= as.data.frame(lapply(Jio_final[2:3],norm))
Jio_norm=data=cbind(jio$Circle,Jio_norm)
Jx_1 <- (0.45-min(Jio_final$`Current_Positioning`))/(max(Jio_final$`Current_Positioning`)-min(Jio_final$`Current_Positioning`))
Jx_2 <- (0.75-min(Jio_final$`Current_Positioning`))/(max(Jio_final$`Current_Positioning`)-min(Jio_final$`Current_Positioning`))
Jy <- (0-min(Jio_final$`Growth_Rate`))/(max(Jio_final$`Growth_Rate`)-min(Jio_final$`Growth_Rate`))

#VODA
voda_norm= as.data.frame(lapply(Voda_final[2:3],norm))
voda_norm=cbind(voda$Circle,voda_norm)
Vx_1 <- (0.45-min(Voda_final$`Current Positioning`))/(max(Voda_final$`Current Positioning`)-min(Voda_final$`Current Positioning`))
Vx_2 <- (0.75-min(Voda_final$`Current Positioning`))/(max(Voda_final$`Current Positioning`)-min(Voda_final$`Current Positioning`))
Vy <- (0-min(Voda_final$`Growth Rate`))/(max(Voda_final$`Growth Rate`)-min(Voda_final$`Growth Rate`))

#MTNL/BSNL
mb_norm= as.data.frame(lapply(MB_final[2:3],norm))
mb_norm=cbind(mb$Circle,mb_norm)
mbx_1 <- (0.45-min(MB_final$`Current Positioning`))/(max(MB_final$`Current Positioning`)-min(MB_final$`Current Positioning`))
mbx_2 <- (0.75-min(MB_final$`Current Positioning`))/(max(MB_final$`Current Positioning`)-min(MB_final$`Current Positioning`))
mby <- (0-min(MB_final$`Growth Rate`))/(max(MB_final$`Growth Rate`)-min(MB_final$`Growth Rate`))


#plot

Airtel_plot<-ggplot(Airtel_norm, aes(x=Current_Positioning, y=Growth_Rate)) +
  geom_point() +
  lims(x=c(-1,2.5),y=c(-1,1)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = Ax_1) + geom_vline(xintercept = Ax_2) + geom_hline(yintercept = Ay) +
  geom_text(label=Airtel_norm$`airtel$Circle`) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

Airtel_plot +theme(axis.text.x=element_blank(), 
                   axis.ticks.x=element_blank(), 
                   axis.text.y=element_blank(), 
                   axis.ticks.y=element_blank()) 


Jio_plot<-ggplot(Jio_norm, aes(x=Current_Positioning, y=Growth_Rate)) +
  geom_point() +
  lims(x=c(-1,2.5),y=c(-1,1)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = Jx_1) + geom_vline(xintercept = Jx_2) + geom_hline(yintercept = Jy) +
  geom_text(label=Jio_norm$`jio$Circle`) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

Jio_plot +theme(axis.text.x=element_blank(), 
                   axis.ticks.x=element_blank(), 
                   axis.text.y=element_blank(), 
                   axis.ticks.y=element_blank()) 


Voda_plot<-ggplot(voda_norm, aes(x=Current.Positioning, y=Growth.Rate)) +
  geom_point() +
  lims(x=c(-1.1,2.5),y=c(-1,1.2)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = Vx_1) + geom_vline(xintercept = Vx_2) + geom_hline(yintercept = Vy) +
  geom_text(label=voda_norm$`voda$Circle`) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

Voda_plot +theme(axis.text.x=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(), 
                axis.ticks.y=element_blank()) 


MB_plot<-ggplot(mb_norm, aes(x=Current.Positioning, y=Growth.Rate)) +
  geom_point() +
  lims(x=c(-1.1,2.5),y=c(-1,1.2)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = mbx_1) + geom_vline(xintercept = mbx_2) + geom_hline(yintercept = mby) +
  geom_text(label=mb_norm$`mb$Circle`) +theme(axis.text.x = element_text(angle = 90, hjust = 1))

MB_plot +theme(axis.text.x=element_blank(), 
                 axis.ticks.x=element_blank(), 
                 axis.text.y=element_blank(), 
                 axis.ticks.y=element_blank()) 


#_____________________________________________________________________________________________________________________________________________
#REVENUE
#________________________________________________________________________________________________________________________________________________

revenue=read.csv(file.choose())

rev_pp=data.frame(airtel$Circle)
rev_pp=data.frame(airtel$Circle)
for(i in 1:22) {
                    rev_pp[i,2]=signif((revenue[i,2]*10000000)/(airtel_sub[i,2]*1000000),4) }
J_rev_pp=data.frame(jio$Circle)
for(i in 1:22) {
  rev_pp[i,3]=signif((revenue[i,3]*10000000)/(jio_sub[i,2]*1000000),4) }
V_rev_pp=data.frame(voda$Circle)
for(i in 1:22) {
  rev_pp[i,4]=signif((revenue[i,4]*10000000)/(voda_sub[i,2]*1000000),4) }
MB_rev_pp=data.frame(mb$Circle)
for(i in 1:22) {
  rev_pp[i,5]=signif((revenue[i,5]*10000000)/(mb_sub[i,2]*1000000),4) }
colnames(rev_pp)=c("Circle","Airtel","Jio","Voda","MTNL/BSNL")
rev_pp


#_____________________________________________________________________________________________________________________________________________________
#Additional Revenue to be generated if reaches the Threshold
#______________________________________________________________________________________________________________________________________________________________

# Airtel
Airtel_R_final=data.frame(airtel$Circle)
for (i in 1:22){  Airtel_R_final[i,2]=((1-Airtel_final[i,2])*usat[i,2]*rev_pp[i,2])/10000000 }

colnames(Airtel_R_final)=c("circle","revenue_inCRORES")
Airtel_R_final


# Jio
Jio_R_final=data.frame(jio$Circle)
for (i in 1:22){  Jio_R_final[i,2]=((1-Jio_final[i,2])*usat[i,2]*rev_pp[i,3])/10000000 }

colnames(Jio_R_final)=c("circle","revenue_inCRORES")
Jio_R_final


# Voda
Vi_R_final=data.frame(voda$Circle)
for (i in 1:22){ Vi_R_final[i,2]=((1-Voda_final[i,2])*usat[i,2]*rev_pp[i,4])/10000000 }

colnames(Vi_R_final)=c("circle","revenue_inCRORES")
Vi_R_final


# Mtnl/Bsnl
Mb_R_final=data.frame(mb$Circle)
for (i in 1:22){ Mb_R_final[i,2]=((1-MB_final[i,2])*usat[i,2]*rev_pp[i,5])/10000000 }

colnames(Mb_R_final)=c("circle","revenue_inCRORES")
Mb_R_final


#__________________________________________________________________________________________________________________________________________
#Rank List
#___________________________________________________________________________________________________________________________________________________

#AIRTEL
a=data.frame()
b=data.frame()
c=data.frame()
d=data.frame()
e=data.frame()

for(i in 1:22){
                if(Airtel_final$Current_Positioning[i]>0.75 & Airtel_final$Growth_Rate[i]<0){
                              a[i,1]=Airtel_R_final[i,1]
                              a[i,2]=Airtel_R_final[i,2]}
  
                else if(Airtel_final$Current_Positioning[i]>0.45 & Airtel_final$Current_Positioning[i]<0.75 & Airtel_final$Growth_Rate[i]<0){
                              b[i,1]=Airtel_R_final[i,1]
                              b[i,2]=Airtel_R_final[i,2]}

                else if(Airtel_final$Current_Positioning[i]<0.45 & Airtel_final$Growth_Rate[i]>0){
                              c[i,1]=Airtel_R_final[i,1]
                              c[i,2]=Airtel_R_final[i,2]}
  
                else if(Airtel_final$Current_Positioning[i]>0.45 & Airtel_final$Current_Positioning[i]<0.75 & Airtel_final$Growth_Rate[i]>0){
                              d[i,1]=Airtel_R_final[i,1]
                              d[i,2]=Airtel_R_final[i,2]}
  
                else if(Airtel_final$Current_Positioning[i]>0.75 & Airtel_final$Growth_Rate[i]>0){
                              e[i,1]=Airtel_R_final[i,1]
                              e[i,2]=Airtel_R_final[i,2]}

  }
a=na.omit(a)
b=na.omit(b)
c=na.omit(c)
d=na.omit(d)
e=na.omit(e)

a=a[order(-a$V2),]
b=b[order(-b$V2),]
c=c[order(-c$V2),]
d=d[order(-d$V2),]
e=e[order(-e$V2),]

Airtel_Rank= rbind(a,b,c,d,e)
Airtel_Rank=cbind(Airtel_Rank$V1)
Airtel_Rank



#JIO
a=data.frame()
b=data.frame()
c=data.frame()
d=data.frame()
e=data.frame()

for(i in 1:22){
  if(Jio_final$Current_Positioning[i]>0.75 & Jio_final$Growth_Rate[i]<0){
    a[i,1]=Jio_R_final[i,1]
    a[i,2]=Jio_R_final[i,2]}
  
  else if(Jio_final$Current_Positioning[i]>0.45 & Jio_final$Current_Positioning[i]<0.75 & Jio_final$Growth_Rate[i]<0){
    b[i,1]=Jio_R_final[i,1]
    b[i,2]=Jio_R_final[i,2]}
  
  else if(Jio_final$Current_Positioning[i]<0.45 & Jio_final$Growth_Rate[i]>0){
    c[i,1]=Jio_R_final[i,1]
    c[i,2]=Jio_R_final[i,2]}
  
  else if(Jio_final$Current_Positioning[i]>0.45 & Jio_final$Current_Positioning[i]<0.75 & Jio_final$Growth_Rate[i]>0){
    d[i,1]=Jio_R_final[i,1]
    d[i,2]=Jio_R_final[i,2]}
  
  else if(Jio_final$Current_Positioning[i]>0.75 & Jio_final$Growth_Rate[i]>0){
    e[i,1]=Jio_R_final[i,1]
    e[i,2]=Jio_R_final[i,2]}
  
}
a=na.omit(a)
b=na.omit(b)
c=na.omit(c)
d=na.omit(d)
e=na.omit(e)

a=a[order(-a$V2),]
b=b[order(-b$V2),]
c=c[order(-c$V2),]
d=d[order(-d$V2),]
e=e[order(-e$V2),]

Jio_Rank= rbind(a,b,c,d,e)
Jio_Rank=cbind(Jio_Rank$V1)
Jio_Rank



#Voda
a=data.frame()
b=data.frame()
c=data.frame()
d=data.frame()
e=data.frame()

for(i in 1:22){
  if(Voda_final$`Current Positioning`[i]>0.75 & Voda_final$`Growth Rate`[i]<0){
    a[i,1]=Vi_R_final[i,1]
    a[i,2]=Vi_R_final[i,2]}
  
  else if(Voda_final$`Current Positioning`[i]>0.45 & Voda_final$`Current Positioning`[i]<0.75 & Voda_final$`Growth Rate`[i]<0){
    b[i,1]=Vi_R_final[i,1]
    b[i,2]=Vi_R_final[i,2]}
  
  else if(Voda_final$`Current Positioning`[i]<0.45 & Voda_final$`Growth Rate`[i]>0){
    c[i,1]=Vi_R_final[i,1]
    c[i,2]=Vi_R_final[i,2]}
  
  else if(Voda_final$`Current Positioning`[i]>0.45 & Voda_final$`Current Positioning`[i]<0.75 & Voda_final$`Growth Rate`[i]>0){
    d[i,1]=Vi_R_final[i,1]
    d[i,2]=Vi_R_final[i,2]}
  
  else if(Voda_final$`Current Positioning`[i]>0.75 & Voda_final$`Growth Rate`[i]>0){
    e[i,1]=Vi_R_final[i,1]
    e[i,2]=Vi_R_final[i,2]}
  
}
a=na.omit(a)
b=na.omit(b)
c=na.omit(c)
d=na.omit(d)
e=na.omit(e)

a=a[order(-a$V2),]
b=b[order(-b$V2),]
c=c[order(-c$V2),]
d=d[order(-d$V2),]
e=e[order(-e$V2),]

Voda_Rank= rbind(a,b,c,d,e)
Voda_Rank=cbind(Voda_Rank$V1)
Voda_Rank



#Mtnl/Bsnl
a=data.frame()
b=data.frame()
c=data.frame()
d=data.frame()
e=data.frame()

for(i in 1:22){
  if(MB_final$`Current Positioning`[i]>0.75 & MB_final$`Growth Rate`[i]<0){
    a[i,1]=Mb_R_final[i,1]
    a[i,2]=Mb_R_final[i,2]}
  
  else if(MB_final$`Current Positioning`[i]>0.45 & MB_final$`Current Positioning`[i]<0.75 & MB_final$`Growth Rate`[i]<0){
    b[i,1]=Mb_R_final[i,1]
    b[i,2]=Mb_R_final[i,2]}
  
  else if(MB_final$`Current Positioning`[i]<0.45 & MB_final$`Growth Rate`[i]>0){
    c[i,1]=Mb_R_final[i,1]
    c[i,2]=Mb_R_final[i,2]}
  
  else if(MB_final$`Current Positioning`[i]>0.45 & MB_final$`Current Positioning`[i]<0.75 & MB_final$`Growth Rate`[i]>0){
    d[i,1]=Mb_R_final[i,1]
    d[i,2]=Mb_R_final[i,2]}
  
  else if(MB_final$`Current Positioning`[i]>0.75 & MB_final$`Growth Rate`[i]>0){
    e[i,1]=Mb_R_final[i,1]
    e[i,2]=Mb_R_final[i,2]}
  
}
a=na.omit(a)
b=na.omit(b)
c=na.omit(c)
d=na.omit(d)
e=na.omit(e)

a=a[order(-a$V2),]
b=b[order(-b$V2),]
c=c[order(-c$V2),]
d=d[order(-d$V2),]
e=e[order(-e$V2),]

MB_Rank= rbind(a,b,c,d,e)
MB_Rank=cbind(MB_Rank$V1)
MB_Rank
