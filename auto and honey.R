###Automobile
rm(list=ls())
au=read.csv("Automobile.csv")
str(au)
summary(au)
head(au,5)
sapply(au, function(x) sum(is.na(au)))
library(tidyverse)
au%>%ggplot(aes(normalized_losses))+geom_histogram(aes(y=..density..),binwidth=15,colour="black", fill="white")+geom_density(alpha=.2, fill="#FF6666")
au%>%ggplot(aes(engine_size,horsepower))+geom_point(color="blue")
pairs(au[,c(2,17,22)],pch=19,col="blue")
library(lattice)
stripplot(horsepower~fuel_type,data=au,jitter.data=T,pch=19,xlab="Fuel_Type")
install.packages("ggbeeswarm")
library(ggbeeswarm)
au%>%ggplot(aes(fuel_type,horsepower))+geom_quasirandom(aes(color=fuel_type))
au%>%ggplot(aes(number_of_doors,horsepower,fill=number_of_doors))+geom_boxplot()
ggplot(au,aes(x=number_of_doors,y=horsepower))+geom_bar(stat='identity')+scale_y_continuous(limits = c(0,500))
ggplot(au,aes(number_of_cylinders))+geom_bar(position ="dodge",aes(fill=number_of_doors))
au%>%group_by(fuel_system)%>%summarise(sd = sd(horsepower, na.rm = TRUE),len = mean(horsepower))->a1
a1
ggplot(a1,aes(x=reorder(fuel_system,len),len))+geom_line(aes(group=1))+geom_errorbar( aes(ymin =len-sd, ymax = len+sd),width = 0.2)+geom_point(size=2)
au%>%ggplot(aes(body_style))+geom_bar()
library(RColorBrewer)
ggplot(au,aes(x=fuel_type,y=horsepower))+geom_bar(stat='identity',aes(fill=number_of_doors))+scale_color_brewer()+scale_y_continuous(limits = c(0,500))+facet_grid(.~number_of_doors)+scale_fill_brewer(palette = "Blues")
ggplot(au,aes(x=fuel_type,y=horsepower))+geom_bar(stat='identity',aes(fill=body_style),position = "dodge")+scale_color_brewer()+scale_y_continuous(limits = c(0,500))+facet_grid(.~number_of_doors)+scale_fill_brewer(palette = "Set1")
ggplot(au,aes(x=fuel_type,y=horsepower))+geom_quasirandom(aes(color=body_style))+facet_grid(.~number_of_doors)+scale_fill_brewer(palette = "Set1")

#####Honey Production
ho=read.csv("honeyproduction.csv")
str(ho)
summary(ho)
sapply(ho, function(x) sum(is.na(ho)))
ho[ho$prodvalue==max(ho$prodvalue),]
pairs(ho[,c(2:7)],pch=19,col="blue")
library(corrplot)
corrplot(cor(ho[,c(2:7)]),type="upper",method="number")

ho%>%group_by(year)%>%summarise(max = max(totalprod,na.rm = TRUE))%>%ggplot(aes(year,max))+geom_line(aes(group=1))+geom_point(size=2)+scale_x_continuous(breaks = c(1998:2012))+ylab("totalprod")
ho%>%group_by(state,year)%>%summarize(max=max(totalprod))%>%ggplot(aes(year,max))+geom_line()+facet_wrap(.~state)+ylab("totaprod")

ho%>%ggplot(aes(numcol,prodvalue))+geom_point()+geom_smooth(method=lm)

ho%>%ggplot(aes(factor(year),totalprod,fill=year))+geom_boxplot(show.legend = F)

ho%>%group_by(year)%>%summarise(max = max(prodvalue,na.rm = TRUE))%>%ggplot(aes(year,max))+geom_line(aes(group=1))+geom_point(size=2)+scale_x_continuous(breaks = c(1998:2012))+ylab("prodvalue")

ho%>%ggplot(aes(totalprod,prodvalue))+geom_point()+geom_smooth(method=lm)

