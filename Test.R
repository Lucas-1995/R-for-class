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


library(dplyr)
library(griffen)
library(tidyverse)

Mi<-lm(log(wage)~educ_years+age+(age2=(age)^2), data=cps)
print(Mi)
EducationCo<-Mi$coefficients["educ_years"]
print(EducationCo)


#2021/11/30


library(dplyr)
library(griffen)
library(tidyverse)

MyFunction<-function(x){
  Mi<-lm(log(wage)~educ_years+age+(age2=(age)^2), data=x)
  EducationCo<-Mi$coefficients["educ_years"]
  return(EducationCo)
}

MyFunction(cps)

States<-cps%>%distinct(state)


for(y in States){
  M<-cps%>%filter(state==y)
  States<-States%>%mutate(Co=MyFunction(M))
}


sort(Edu)
Cps<-cps



# install.packages("googlesheets")
# library(googlesheets)
# gs_auth(new_user = TRUE)
install.packages("gsheet")
library(gsheet)
w<-gsheet2tbl("https://docs.google.com/spreadsheets/d/1AVfgO1hO51MVdrLoD-Xk49LouJ3wKULZWiz7An0dxNg/edit#gid=0",sheetid=2)
?gsheet2tbl

install.packages("ggtext")
install.packages("showtext")
library(tidyverse)
library(ggrepel)
library(ggtext)

library(showtext)
font_add_google("Lato")
showtext_auto()


install.packages("hrbrthemes")
install.packages("kableExtra")
install.packages("babynames")
install.packages("streamgraph")
install.packages("viridis")
install.packages("DT")
install.packages("plotly")
# Libraries
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
options(knitr.table.format = "html")
library(babynames)
library(streamgraph)
library(viridis)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Load dataset from github
data <- babynames %>% 
  filter(name %in% c("Mary","Emma", "Ida", "Ashley", "Amanda", "Jessica",    "Patricia", "Linda", "Deborah",   "Dorothy", "Betty", "Helen")) %>%
  filter(sex=="F")

# Plot
w %>%
  filter( !X1 == "福島県" )%>%
  filter( !X1 == "岩手県" )%>%
  filter( !X1 == "宮城県" )%>%
  pivot_longer(-X1,names_to = "Year", values_to = "Mortality")%>%
  ggplot( aes(x=Year, y=Mortality, group=X1, color=X1)) +
  geom_line() +
  #scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("Disaster-related indirect mortality in municipalities of 3 Tohoku prefectures") +
  theme_ipsum()



#TEST
library(purrr)
#sqrt is the square root of x
?sqrt
sqrt(6)
x<-c(1,6,9)

#length gets the number of items in the vector
for(i in length(x)){
  x[i]<-sqrt(x[i])
}
print(x)

#map creates a new bigger tibble
#map(a list , a function)
test<-map(x,sqrt)
#map_dbl makes a normal size of tibble(?)
test2<-map_dbl(x,sqrt)



#tibble, list and vector are different data formats in R
y <- list(name="abc", age=36, company="XYZ") 


x<-c(9,16,25)
doubler<-function(y){
　2*y
}
doubler(x)
map_dbl(x,doubler)
map(x,doubler)
mtcars
#difference among map, map_dbl and map_df?
#map returns list, map_dbl returns double vector, map_df returns data frame
x<-map(mtcars,mean)
x<-map_dbl(mtcars,mean)
x<-map_df(mtcars,mean)
m<- lm (mpg ~ cyl,mtcars)

#str is a way to compactly display the structure of as arbitrary R object
str(m)

#a way to have access to coefficients of  a regression's result and get a specific coefficient
m$coefficients["cyl"]

#tidy() is a function to get a tidy table off the result of the regression
library(broom)
lm(mpg ~ cyl, mtcars)%>%broom::tidy()

#dataframe in dataframe
library(gapminder)
library(tidyr)
library(dplyr)
gapminder_nested<-gapminder%>%group_by(country)%>%nest()

#WTF is df?
#df is a function?
?df
time_trend<-function(df){
  lm(lifeExp ~ year,df)
}

# I think the advantage of R here is  x
gapminder_models <- gapminder_nested %>% mutate(model = map(data,time_trend))
m<-data    
n<-df
gapminder_models <-  gapminder_models %>% select(-model,-data)
gapminder_coef<-gapminder_models %>% unnest(cols=c("tidy_model"))
?tidy_model




#2021/12/18
library(dplyr)
library(tidyverse)
library(gapminder)
library(broom)

gapminder_nested <-
  gapminder %>%
  group_by(country) %>%
  nest()

#df is a specific parameter for function making
time_trend <- function(df){
  lm(lifeExp ~ year,df)
}
x<-data

time_trend(gapminder)

gapminder_models <-
  gapminder_nested %>%
  mutate(model = map(data,time_trend))

#
gapminder_models <-
  gapminder_models %>%
  mutate(tidy_model = map(model,tidy))

#tidy() can use regression function as a parameter and return a tibble
lm(mpg ~ cyl,mtcars) %>% tidy()

gapminder_models <-
  gapminder_models %>%
  select(-model,-data)

#tear down the variable"tidy_model"
gapminder_models <- gapminder_models %>%
  unnest("tidy_model")

#filter picks out all the rows under the variable term that equals to year
#select picks out columns of the specific variable(here are country and estimate)
#ungroup ?
gapminder_coef <-
  gapminder_models  %>%
  filter(term=="year") %>%
  select(country,estimate) %>%
  arrange(-estimate) %>%
  ungroup

#left_join deals with 2 dataframes and use the left one as the standard
#join:add columns from y to x, matching rows based on the keys
gapminder_coef <-
  gapminder_coef %>%
  left_join(distinct(select(gapminder,country,continent)))
?left_join

gapminder_coef

p <- ggplot(gapminder_coef,
            aes(x = country , y = estimate, color=country))
p <- p + geom_point(show.legend = FALSE)
#country_colors is a color scheme from gapminder 
p <- p + scale_colour_manual(values = country_colors)
#divide the graph into several one by continent
p <- p + facet_wrap(. ~ continent)
print(p)

#theme and axis.text.x=element_text(angle=90) is a function to reverse the direction of text
p <- ggplot(gapminder_coef,
            aes(x = country , y = estimate, color=country))
p <- p + geom_point(show.legend = FALSE)
p <- p + scale_colour_manual(values = country_colors)
p <- p + theme(axis.text.x = element_text(angle = 90))
print(p)

#str is structure
str(gapminder_coef)

# 
# gapminder_coef$country <- factor(gapminder_coef$country,levels=gapminder_coef$country)
gapminder_coef <-
  gapminder_coef %>%
  mutate(country = factor(country))

gapminder_coef <-
  gapminder_coef %>%
  mutate(country1 = fct_reorder(country,-estimate))

p <- ggplot(gapminder_coef,
            aes(x = country , y = estimate, color=country))
p <- p + geom_point(show.legend = FALSE)
p <- p + scale_colour_manual(values = country_colors)
p <- p + coord_flip()
# p <- p + theme(axis.text.y = element_text(size=7))
#p <- p + theme(axis.text.x = element_text(angle = 90))
print(p)

#2021/12/19 Graphics in R
#gganimate
#extrafont includs many fonts we can use for graph
install.packages("extrafont")
library(ggthemes)
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
m<-fonts()            
windowsFonts()
p <- ggplot(data = gapminder, aes(x = log(gdpPercap) ,
                                  y = lifeExp,color=country, size=pop))+
    geom_point(alpha = 0.7, show.legend = FALSE)+
    scale_colour_manual(values = country_colors)+
    facet_wrap(. ~ continent,ncol=5)+
    theme_few()+
  #the way to change text's characteristics, size, etc.
    theme(axis.title.y = element_text(face="bold",size=11),
          axis.title.x = element_text(face="bold",size=11),
          text=element_text(family="Serif"))
print(p)

install.packages("gganimate")
install.packages("gifski")
library(gganimate)
library(gifski)
library(gapminder)
library(Rcpp)
p <- ggplot(data = gapminder, aes(x = log(gdpPercap) ,
                                  y = lifeExp,color=country, size=pop))
p <- p + geom_point(alpha = 0.7, show.legend = FALSE)
p <- p + scale_colour_manual(values = country_colors)
p <- p + facet_wrap(. ~ continent,ncol=5)
p <- p + labs(title = 'Year: {frame_time}', x = 'log GDP per capita', y = 'Life expectancy')
p <- p + transition_time(year) + ease_aes('linear')
animate(p, duration = 7,
        renderer = gifski_renderer("./Media/rosling.gif"),
        height = 350, width = 600, units = "px")


#2021/12/20 lecture 8
library(griffen)
library(dplyr)
library(tidyverse)
library(ggthemes)
#keep age and year and mean wage by them
Cps<-cps%>%select(wage, age, year)%>%group_by(age,year)%>%
           summarise(average_wage=mean(wage,na.rm=TRUE))%>%
           arrange(year)
p<-ggplot(data=Cps,aes(x=age, y=average_wage)) +
  geom_point(show.legend = FALSE) +
  scale_colour_manual()+ #?is this one necessary?
  labs(title = 'Year:{frame_time}', x='Age of Americans', y ='Average wage')+
  theme_few()+
  transition_time(as.integer(year)) + ease_aes('linear')
animate(p, duration=10,
        renderer = gifski_renderer("D:/Rtest/cps challenge.gif"),
        height = 350, width = 600, units ="px")
  

#profe's code
cps_wage <- cps %>%
  group_by(year,age) %>%
  summarise(mean_wage = mean(wage,na.rm=TRUE), .groups = "drop")
p <- ggplot(data = cps_wage, aes(x = age , y = mean_wage))
p <- p + geom_point()
p <- p + labs(title = 'Year: {frame_time}', x = 'Age', y = 'Average Wage')
p <- p + transition_time(year) + ease_aes('linear')
animate(p, duration = 10,
        renderer = gifski_renderer("./Media/cps2.gif"),
        height = 350, width = 600, units = "px")




#group by education
cps_wage_educ <-
  cps %>%
  group_by(year,age,education_category) %>%
  summarise(mean_wage = mean(wage,na.rm=TRUE), .groups = "drop")
#to change the variable education_category into factor
#just use factor()
cps_wage_educ <-
  cps_wage_educ %>%
  mutate(education_category = factor(education_category))
#fct_relevel reorders factor levels by hand
#Before:college,highschool,somecollege (by alpgabet)
#New:college, somecollege,highschool
cps_wage_educ <-
  cps_wage_educ %>%
  mutate(education_category =
           fct_relevel(education_category,c("college","somecollege","highschool")))
str(cps_wage_educ)

library(wesanderson)
#animate for more possibilities 
#3 labels ->3 small graphs
educ_labels <- c(college = "College",
                 somecollege = "Some College",
                 highschool = "High School")
#Besides x and y, color is used to recognize education situations
p <- ggplot(data = cps_wage_educ,
            aes(x = age , y = mean_wage, color = education_category))
#scatter points
p <- p + geom_point()
#make graghs by education_category by facet_wrap
p <- p + facet_wrap(. ~ education_category,
                    labeller = labeller(education_category = educ_labels))
#use a palette from the package wesanderson
p <- p + scale_colour_manual(values = wes_palette("Darjeeling1"))
#defer legend
p <- p + theme(legend.position = "none")
p <- p + theme_few()
p <- p + labs(title = 'Year: {frame_time}', x = 'Age', y = 'Average Wage')
p <- p + transition_time(as.integer(year)) + ease_aes('linear')
animate(p, duration = 10, renderer = gifski_renderer("D:/Rtest/cps Anotherchallenge.gif"),
        height = 350, width = 600, units = "px")

#2021/12/26
#Lecture 9 :optimization, modelr, BART
library(tidyverse)
library(modelr)

#optim(initial parameter guess ,function to be minimized, ...)
#par	The best set of parameters found.
#value	The value of fn corresponding to par.
#counts A two-element integer vector giving the number of calls to fn and gr respectively. This excludes those calls needed to compute the Hessian, if requested, and any calls to fn to compute a finite-difference approximation to the gradient.
#how to use optim for minimizing functions
test_optim <- function(b){(b[1]-2)^4 + 3*b[2]^2}
test_optim(c(0,0))
optim(c(0,0),test_optim)

#more advanced (minimizing)
#3 independent variables
test_optim <- function(b,a){(b[1]-a)^2 + 3*b[2]^2}
test_optim(c(0,0),a=7)
#we need to insert the argument"a" in the end
optim(c(0,0),test_optim,a=7)
# If we change the additional argument, what optim returns changes either
optim(c(0,0),test_optim,a=4)

#maximizing
test_optim <- function(b){-b[1]^4 - 3*b[2]^2}
optim(c(1,1),test_optim)
#control=list(fnscale=-1) is the point to make it looking for a maximum
optim(c(1,1),test_optim,control=list(fnscale=-1))


library(devtools)
library(griffen)
f <- y ~ x1
form_df
model_matrix(form_df , f)
#Another way to code
form_df %>% model_matrix(f)
#Matrix is a new data structure
form_df %>% model_matrix(f) %>% as.matrix()
#same same same
model_matrix(form_df , y ~ x1)
model_matrix(form_df , ~ x1)
model_matrix(form_df , ~ x1 + x2)
#same same different ways to use model_matrix
model_matrix(form_df , ~ x1*x2)
model_matrix(form_df , ~ x1 + x2 + x1*x2)
model_matrix(form_df , ~ x1:x2)
test<-model_matrix(form_df , ~ x1:x2 - 1)
#Y~X..... means after ~ there are the names of the variables
model_matrix(form_df , ~ D)
form_df %>% str()
# This is how to get sth out of the tibble
form_df$y[2]
p$data[2]

#fct_relevel :Reorder factor levels 
#In matrix it must be number 
#So here shows how factor(chr)was turned into dbl(1 or 0)
#!!IMPORTANT:if it's the 1st factor , it'll be turned into 1;if it's not, 0.
m3<-form_df %>%
  mutate(D = fct_relevel(D,"treated")) %>%
  model_matrix(~ D)

# the difference between these is that *put the result and all the variables
#  :just put the result of D multiply x1
# here are 2 columns of the results in :'s case
model_matrix(form_df , ~ D*x1)
model_matrix(form_df , ~ D:x1)

#if input is a dataframe df should be used
#b is a formula object
Fk<-function(df,b){
  x <- df%>%model_matrix(b)%>%as.matrix()
  y <- df%>%select(all.vars(b)[1])%>%pull()
  #x'=t()  transpose a (x) matrix
  #x^-1=solve()   invert a (x) matrix 
  z <- solve(t(x) %*% x) %*% t(x) %*% y
  return(z)
}
#why it is the same as lm
Fk(mtcars,mpg ~ cyl + disp + hp)
lm(mpg ~ cyl + disp + hp, data = mtcars)
# M1<-matrix(1:3,ncol=3,nrow=3)
# M2<-matrix(3:1)
# a <- matrix(c(0,1,2,3,4,5,6,7,9),3,3) #3y + 6z =  1
# b <- matrix(c(1,0,-2))                #x + 4y + 7z =  0
# solve(a)
# T1<-solve(A)

#2021/12/27
Vector1<-pre_bart%>%drop_na()
v<-Vector1$mode
post_bart 

#2022/01/22
library(griffen)
library(sjPlot)
library(tidyverse)
pre<-pre_bart
pre$mode<-factor(pre$mode)



# filter training data with those mode equal to bus
train <- train %>% filter(!mode == "bus")
test <- test %>% filter(!mode == "bus")
# training


# post<-post_bart
Logit<-glm(d ~ price + time + wage + mode, data = train,family = "binomial")
# tab_model(Logit,p.style = "stars",show.std = TRUE)

fitted.results<-predict(Logit, newdata=subset(test,select=2:5),type='response')

summary(fitted.results)
fitted.results<-ifelse(fitted.results>0.5,1,0)
misClasificError<-mean(fitted.results != test$d)
print(paste('Accuracy', 1-misClasificError))
"Accuracy 0.842696629213483"

# confint(Logit)
NewData<-with(train, data.frame(id=id,mode=mode,price=price,time=time,wage=wage))
NewData$d<-predict(Logit,newdata=NewData, type="response")

# params vector 
params <-Logit$coefficients
str(pre)
Pre<-pre
# Prediction<-function(df){
#   # split train and test data, [1:8000,] [8001,12000] train = pre[1:8000,]
#   train <- df[1:9000,]
#   test <- df[9001:12000,]
#   
#   # post<-post_bart
#   Logit<-glm(d ~ price + time + wage + mode, data = train,family = "binomial")
#   Coe <-Logit$coefficients
#   
#   d=Coe[1]+Coe[2]*price+Coe[3]*time+Coe[4]*wage+Coe[5]*mode
#   return(d)
# }

#IMPORTANT HOMEWORK3
Logit<-glm(d ~ price + time + wage, data = pre_bart,family = "binomial")
Coe <-Logit$coefficients

Result_bart<-pre_bart
Result_bart$mode<-factor(Result_bart$mode)

Result_bart <- Pre_bart %>% 
  mutate(v =exp((Coe[1]+Coe[2]*price+Coe[3]*time+Coe[4]*wage)))

#2022/01/28
install.packages("tidyverse")
install.packages("devtools")
library(devtools)
library(tidyverse)
install_github("andrew-griffen/griffen") 
library(griffen)
library(modelr)
Sys.setenv(LANG = "en")

#IMPORTANT HOMEWORK3
#Prepare training dataframe to feed the logit model
pre_bart_estimation<-pre_bart%>%na.omit()

#to use logit model to get coefficients
Logit<-glm(d ~ price + time + wage, data = pre_bart_estimation,family = "binomial")

#Multiply matrix of original data and co to induce v
#To change list to matrix
b <- as.matrix(Logit$coefficients)

#to contain NA globally
options(na.action='na.pass')
#Ta get another matrix as test dataframe
m <- post_bart %>% model_matrix(d ~ price + time + wage) %>% as.matrix()
#To set "contain NA" back
current.na.action <- options('na.action')

#to multiply matrix , they have to be adjusted
v <- t(b) %*% t(m)
V<-as.vector(v)

#INSERT many variables
#Change the dataframe to change what to be used as predictors
#Q1how to compare?
Predict_bart <- post_bart %>% mutate(ev=exp(V)) %>%  
  group_by(id) %>%  mutate(sum_v=sum(ev))%>% 
  mutate(p=ev/sum_v) %>% mutate(lnp=log(p)) %>% 
  select(!"sum_v"&!"ev") %>% ungroup() 

#Actual Share
  car_share1 <- Predict_bart %>% filter(mode=="car") 
  Car1<-sum(car_share1$d)/4000
  metro_share1 <- Predict_bart %>% filter(mode=="metro") 
  Metro1<-sum(metro_share1$d)/4000
  bus_share1 <- Predict_bart %>% filter(mode=="bus") 
  Bus1<-sum(bus_share1$d)/4000

#Predicted Share
  car_share2 <- Predict_bart %>% filter(mode=="car") 
  Car2<-sum(car_share2$p)/4000
  metro_share2 <- Predict_bart %>% filter(mode=="metro") 
  Metro2<-sum(metro_share2$p)/4000
  bus_share2 <- Predict_bart %>% filter(mode=="bus") 
  Bus2<-sum(bus_share2$p)/4000
  
#COMPARISON of actual share and predicted share
  Mode <- c("Car", "Metro", "Bus")
  Actual<-c(Car1,Metro1,Bus1)
  Predicted<-c(Car2,Metro2,Bus2)
  Com<-tibble(Mode,Actual,Predicted)
  
# #Log-likelihood function
# #optim here is for looking for a best situation that as many as people are benefited
# #Q2why error?
# LLF<-function(df){
#   d<-df%>%pull(d)
#   lnp<-df%>%pull(lnp)
#   logl<-sum(d*lnp)
#   return(logl)
# }
# optim(c(0,0), LLF, post_bart)


