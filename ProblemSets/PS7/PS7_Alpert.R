library(mice)
library(modelsummary)
library(tidyverse)

df<-read_csv("https://raw.githubusercontent.com/tyleransom/DScourseS22/master/ProblemSets/PS7/wages.csv")
df<-df %>% filter(!(is.na(hgc)|is.na(tenure)))

datasummary_skim(df,histogram=F,output = "markdown") %>% print
df <- df %>% mutate(logwage.meanimp=logwage, logwage.meanimp = replace(logwage.meanimp,is.na(logwage),mean(logwage,na.rm=T)))
df <- df %>% mutate(predwage=predict(est1,newdata = df))
df <- df %>% mutate(logwage.mar=logwage, logwage.mar = replace(logwage.mar,is.na(logwage),predwage[is.na(logwage)]))
est1 <- lm(logwage ~ hgc+tenure+age, data = df)
est2 <- lm(logwage.meanimp ~ hgc+tenure+age, data = df)
est3 <- lm(logwage.mar ~ hgc+tenure+age, data = df )
modelsummary(list(est1,est2,est3),output="latex") %>% print

