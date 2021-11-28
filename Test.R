install.packages("dplyr")
install.packages("tidyverse")
install.packages("devtools")
devtools::install_github("andrew-griffen/griffen")
library(griffen)
library("dplyr")
library(ggplot2)
library(tidyverse)
x<-cps
x
educ_values<-cps%>% select(educ_years)%>%distinct()
for(i in educ_values){print(i)}
#select()->select variable   
#filter()->select items with a special variable
test<-cps%>%filter(year==1999|year==1984)
test2<-cps%>%filter(year %in% c(1999,1984))

#pull()->turn tibble to vector
test<-cps%>%filter(year==1999)
mean_wage<-mean(test$wage,na.rm=TRUE)
mean_wage
#Another way to put it
test2<-cps%>%filter(year==1999)%>%select(wage)%>%pull()
mean_test2<-mean(test2,na.rm=TRUE)
mean_test2

#summarise () returns a tibble
x<-summarise(cps,mean_wage=mean(wage,na.rm=TRUE))
x1<-cps%>%select(wage,year)%>%group_by(year)

#what's group_by and summarise ?for what???????????
x<-cps%>%select(wage,year)%>%group_by(year)%>%summarise(mean_wage=mean(wage,na.rm=TRUE))

Educ1<-cps%>%select(wage,educ_years)%>%group_by(educ_years)%>%summarise(mean_wage=mean(wage,na.rm=TRUE))

Educ2<-cps%>%select(wage,educ_years,year)%>%group_by(educ_years,year)%>%summarise(mean_wage=mean(wage,na.rm=TRUE))
SD_wages<-cps%>%select(wage,year)%>%group_by(year)%>%summarise(sd_wage=sd(wage,na.rm=TRUE))

TESR1<-cps%>%select(wage,year)
TESR<-cps%>%select(wage,year)%>%group_by(year)%>%summarise(mean_mean=mean(wage,na.rm=TRUE))
CPS<-cps

#Find the average wage by education,year and Standard deviation
Average_WAGE_By_Edu<-cps%>%select(wage,education_category)%>%group_by(education_category)%>%summarise(Wage_mean=mean(wage,na.rm=TRUE))
Average_WAGE_By_EduYear<-cps%>%select(wage,educ_years)%>%group_by(educ_years)%>%summarise(Wage_mean=mean(wage,na.rm=TRUE))
Sd_WAGE_By_Edu<-cps%>%select(wage,year)%>%group_by(year)%>%summarise(Wage_sd=sd(wage,na.rm=TRUE))

#basic dot plot from ggplot2
qplot(x=year,y=Wage_sd,data=Sd_WAGE_By_Edu)

#slice to cut out several rows from the dataset
?slice_sample
m<-cps%>%slice_sample(prop=0.01)
m

#rename to change variables names
n<-cps
n<-cps%>%rename(area=region)

#mutate add a new variable(including changing variable in instruction)
m<-cps%>%mutate(Log_wage=log(wage))
?mutate

#put mean to 3 variables()
?across
m<-cps%>%group_by(year)%>%summarise(across(c(black,white,married),mean,na.rm=TRUE))
m<-cps%>%group_by(year)%>%summarise(across(where(is.character),mean,na.rm=TRUE))
cps%>%select(where(is.character))

cps%>%select(where(is.character))%>%mutate(across(everything(),toupper))

x<-factor(c('a','b','c','a','b','b'))
x
x<-factor(c('b','c','x','a'))
x<-c('a','b')
x<-factor(x)
X<-factor(c('b','c','x','a'),levels=c("x","b","c","a"))

?join
x<-tibble(key=c(1,2,3),val_x=c("x1","x2","x3"))
y<-tibble(key=c(1,2,4),val_y=c("y1","y2","y3"))

#inner_join match the same and merge them
inner_join(x,y)

#left_join keep all the x(the 1st parameter)<->opposite to right_join
left_join(x,y)
right_join(x,y)
full_join(x,y)
semi_join(x,y)
anti_join(x,y)

#move variable to the left of the tibble
mtcars
mtcars%>%left(carb)

#ifelse  (test, yes, no) if test passes return 1st  if else 2nd
tbl4%>%pivot_longer(-id,names_to="round",values_to="contribution")%>%
  mutate(round=ifelse(round=="contribution_round1",1L,2L))

#str_replace_all (a,b,c) a:variable name   b:the thing to be replaced  c:the new content
tbl4%>%pivot_longer(-id,names_to="round",values_to="contribution")%>%
  mutate(round=str_replace_all(round,"contribution_round",""))%>%
  mutate(round=as.integer(round))

#names_prefix cut out part of the name of the string
tbl5%>%pivot_longer(cols=starts_with("contribution"),
                    names_to="round",values_to="contribution",
                    names_prefix = "contribution_round")

#Create a new dataset and input sum and frac
#ungroup the opposite of group_by
help(relig_income)
test<-relig_income%>%pivot_longer(-religion,names_to="income",values_to="count")%>%
  group_by(religion)%>%summarise(sum_income=sum(count))%>%
  mutate(fraction_income=sum_income/sum(sum_income))%>%
  arrange(-fraction_income)%>%ungroup
test%>%select(sum_income)%>%sum()
?ungroup

#3 ways to pivot
WB<-world_bank_pop%>%pivot_longer(-country&-indicator,names_to="Year",values_to = "Population")
#The code used by Griffen
world_bank_pop%>%pivot_longer(cols=as.character(2000:2017))
world_bank_pop%>%pivot_longer(cols='2000':'2017')

#use . to separate 1 col to many
WB1<-WB%>%separate(indicator,c(NA,"area","variable"))

WB<-WB%>%select(Year,Population)%>%
  mutate(Year=as.numeric(Year),Population=as.numeric(Population))

#separate variable(Grow+TOTL)out as new columns
WB2<-WB1%>%pivot_wider(names_from = variable,values_from = Population)

WB2<-WB2%>%rename(Total_P=TOTL,Grow_Rate=GROW)



#2021/11/28
install.packages(gapminder)
library(tidyverse)
library(dplyr)
library(gapminder)
p<-ggplot(data=filter(gapminder,country=="China"))+
  geom_point(
    aes(x=year,y=lifeExp,size=pop),
    show.legend=FALSE )
print(p)
