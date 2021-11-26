

#homework
library("dplyr")
library(tidyr)
library(griffen)
library("devtools")
library("rmarkdown")
library("magrittr")

#1
new_cps<-cps[,c("education_category","age","wage")]
new_cps<-filter(new_cps,!is.na(wage))
new_cps<-filter(new_cps,!is.na(age))
new_cps<-filter(new_cps,!is.na(education_category))
Mincer_regression<-
  lm(log(wage)~education_category + age + age^2, data=new_cps)

plot(Mincer_regression)
Mincer_regression$coefficients[3]

render("Homework.Rmd")

#2
MyFunction<-function(x){
  
  new_xs<-x[,c("education_category","age","wage")]
  new_x<-filter(new_x,!is.na(wage))
  new_x<-filter(new_x,!is.na(age))
  new_x<-filter(new_x,!is.na(education_category))
  RRegression<-
    lm(log(wage)~education_category + age + age^2, data=new_cps)
  
  return(RRegression$coefficients[3])
  
}



knitr::stitch('Test.R')
browseURL('Test.pdf')

#get dataset from library to Rstudio
x<-cps

#select   get variable from a dataset
y<-select(x,year)

#distinct    just get different numbers under a variable category
#arrange      sort it/  reverse it by adding a - in front of the variable name
z<-cps%>%select(year)%>%distinct()%>%arrange(-year)

#pull  get vector(tibble?)
Z<-cps%>%select(year)%>%distinct()%>%arrange(-year)%>%pull()
z

#no pull-> build a dataset rather than a vector
educ_years<-cps%>%select(educ_years)%>%distinct()%>%arrange(educ_years)
educ_years
for(i in educ_years){
  print(i)
}

#filter pick out by a specific variable   *got a new dataset(tibble)
filter<-filter(cps,year==1999)
filter<-cps%>%filter(year==1999)



#2021/11/12
install.packages("gapminder")
install.packages("wesanderson")
install.packages("Hmisc")
install.packages("ggthemes")
library(gapminder)
library(wesanderson)
library(Hmisc)
library(ggthemes)
library(ggplot2)
p<-ggplot(gapminder,aes(x=gdpPercap,y=lifeExp,color=continent))+geom_point()
scale_colour_mannual(values = wes_palette("Darjeeling2"))+
labs(x="GDP per capita",y="Life expectancy")
?wes_anderson
#Color Hunt : A website to get color for graph
#https://wesandersonpalettes.tumblr.com/

x<-gapminder
gapminder%>%fiter(country=="China")%>%ggplot(aes(x=year,y=lifeExp,label=pop))

#pivot_longer delete a variable and turn variable into 2 variables with specified variable names
z<-filter%>%pivot_longer(hours_lastweek,names_to="name",values_to="values") 
tbl1%>%pivot_longer(-id)
#contain:to convert variables whose name contains something
tbl1%>%pivot_longer(cols=contains("r"))
#start:to convert variables whose name starts by something
tbl1%>%pivot_longer(cols=starts_with("i"))
tbl2%>%pivot_longer(cols=a:c)
#-id means to keep the variable id and turn all other variable names and values into a <name and value>
tbl3%>%left(id)%>%pivot_longer(-id)

