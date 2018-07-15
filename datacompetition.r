
rm(list=ls(all=TRUE))


install.packages(c("plm","mvtnorm","sandwich","lmtest",
                   "foreign","arm","rms", "readxl", "Hmisc", 
                   "haven", "car", "compactr", "multcomp", "splines", "stargazer", "strucchange","corrplot","rpart","standardize"))

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tree")
install.packages("rpart.plot")
install.packages("e1071")
install.packages("rminer")
install.packages("ROCR")
install.packages("plotROC")
install.packages("pROC")

update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')
install.packages('knitr', repos = c('http://rforge.net', 'http://cran.rstudio.org'),
                 type = 'source')
library(knitr)
library(magrittr)
library(dplyr)
library("ggplot2")
library(tree)
library(e1071)
library(rminer)

library(mvtnorm)
library(sandwich) #to help get Heteroskedasticity robust standard errors
library(lmtest) #lmtest provides a large collection of diagnostic tests
library(foreign)
library(arm) #to help estimated interaction terms
library(rms) #for OLS regressions
library(readxl) #to read excel files
library(Hmisc) #for adding variable descriptions
library(haven) #need for uploading stata files
library(car) #companion to applied regression
library(compactr)
library(multcomp) #we need this to run the Generalized least squares.  This will help
#performing statistical tests using Robust Standard errors. 
library(plm) #Helps to use the Wald test for joint restrictions on coefficients
library(splines) #for if you need to perform a spline regressions
library(stargazer) #to make nice tables in Latex
library(strucchange) #to perfrom the chow test
library(corrplot)
library("rpart")
library(rpart.plot)
library(standardize)
library(rpart)
library(rpart.plot)
library(e1071)

library(ROCR)
library(pROC)

cps_comp <- read.csv("TrainingEkta.csv")

cds_test <- read.csv("TestDataset.csv")
id <- cps_comp$Id
education <- cps_comp$EducationLevel



age <- cps_comp$Age
ageRange  <- cps_comp$AgeRange
empstatus <- cps_comp$EmploymentStatus
gen <- cps_comp$Gender
child <- cps_comp$Children
weeklyEarnings <- cps_comp$WeeklyEarnings
year <- cps_comp$Year
whw <- cps_comp$WeeklyHoursWorked
sleeping <- cps_comp$Sleeping
grooming <- cps_comp$Grooming
housework <- cps_comp$Housework
fooddrinkprep <- cps_comp$FoodDrinkPrep
houronchild <- cps_comp$HoursspentonChildren
jobsearching <- cps_comp$JobSearching
shop <- cps_comp$Shopping
eatanddrink <- cps_comp$EatingandDrinking
socializeandrelax <- cps_comp$SocializingRelaxing
#television <- cps_comp$Television
golfing <- cps_comp$Golfing
running <- cps_comp$Running
volunteer <- cps_comp$Volunteering
total <- cps_comp$Total
totalinmin <- cps_comp$TotalinMins

empstatus2 <- cps_comp$EmploymentStatus2


summary(weeklyEarnings)


cps <- data.frame(Edu, age, empstatus,gen, child, weeklyEarnings, year,whw, sleeping, grooming,housework,fooddrinkprep,houronchild, jobsearch,shop,eatanddrink,socialize, golfing,running,volunteer,total)







cps_fe <- subset(cps, empstatus!="Not in labor force") 

summary(cps_fe)



summary(d1)

install.packages("randomForest")
library(randomForest)
library(caret)
library(plotROC)


#Correlationmatrix**********************************


demo_dataframe <- data.frame(sleeping,grooming,housework,fooddrink,caringforchildren,playingwithchildren,jobsearching,shop,eatanddrink,socializeandrelax,television,golfing,running,volunteer,totalmin,dailyworkmins)


n <- cor(demo_dataframe)
corrplot(n, method = "number")










age+jobsearching+shop+eatanddrink+golfing+running+volunteer+total+sleeping+grooming+housework+fooddrinkprep+houronchild++weeklyEarnings+whw+socializeandrelax+empstatus2




demo_dataframe <- data.frame(age,jobsearching,shop,eatanddrink,golfing,running,volunteer,total,sleeping,grooming,housework,fooddrinkprep,houronchild,weeklyEarnings,whw,socializeandrelax,empstatus2)




#Random Forest***************************************************************
EmpStat <- randomForest(empstatus~shop+eatanddrink+golfing+running+volunteer+sleeping+grooming+housework+fooddrinkprep+houronchild, cps_comp,ntree=800, mtry=8)

importance(EmpStat)

varImpPlot(EmpStat,color="blue")

print(EmpStat)

predicted <- predict(EmpStat,cds_test, type='class')

predicted




predictioprobs <- predict(EmpStat,cds_test,type='prob')

predictioprobs







auc <- auc(cds_test$EmploymentStatus,predictioprobs[,2])


print(fit)


















summary(predicted)

submit <- data.frame(EmploymentStatus=cds_test$EmploymentStatus,EmploymentStatus=predicted)

summary(submit)
summary(empstatus)

print(submit)

write.csv(submit,file="TestDataset.csv", row.names=FALSE)






#ROC CURVE
pred1=predict(fit,type = "prob")


perf = prediction(pred1[,2], cds_test$EmploymentStatus)

# 1. Area under curve
auc = performance(perf, "auc")
auc

# 2. True Positive and Negative Rate
pred3 = performance(perf, "tpr","fpr")

# 3. Plot the ROC curve
plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")



















cps <- data.frame(Edu, age, empstatus,gen, child, weeklyEarnings, year,whw, sleeping, grooming,housework,fooddrinkprep,houronchild, jobsearch,shop,eatanddrink,socialize, golfing,running,volunteer,total)


cps_fe <- subset(cps, empstatus!="Not in labor force") 

tree_model = tree(empstatus~., cps_fe) 

plot(tree_model)

text(tree_model, pretty=0)




















dev.off()


cps_comp %>% filter(!is.na(EducationLevel)) %>% group_by(EducationLevel) %>% filter(WeeklyHoursWorked<40) %>% filter(WeeklyEarning<485) %>% summarise(n=sum(HoursspentonChildren), k=length(HoursspentonChildren), avg=n/k ) %>% arrange(-avg) %>% ungroup() -> dc








ggplot(data = dc, aes(x=reorder(dc$EducationLevel,k), y=avg))+  
  geom_bar(stat="identity", fill="magenta", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Working Hours< 40 and Weekly Earnings<485", y = "Average Time in Minutes")










"""













""" >40
cps_comp %>% filter(!is.na(EducationLevel)) %>% group_by(EducationLevel) %>% filter(WeeklyHoursWorked>40) %>% summarise(n=sum(HoursspentonChildren), k=length(HoursspentonChildren), avg=n/k ) %>% arrange(-avg) %>% ungroup() -> dc




ggplot(data = dc, aes(x=reorder(dc$EducationLevel,k), y=avg))+  
  geom_bar(stat="identity", fill="magenta", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Avg time on Baby care whours> 40 with EDU Levels", x ="Educational Level", y = "Time in Minutes")

"""









"""Avg time on Baby care whours<40 with EDU Levels

cps_comp %>% filter(!is.na(EducationLevel)) %>% group_by(EducationLevel) %>% filter(WeeklyHoursWorked>40) %>% summarise(n=sum(HoursspentonChildren), k=length(HoursspentonChildren), avg=n/k ) %>% arrange(-avg) %>% ungroup() -> dc


ggplot(data = dc, aes(x=reorder(dc$EducationLevel,k), y=avg))+  
  geom_bar(stat="identity", fill="magenta", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Avg time on Baby care whours<40 with EDU Levels", x ="Educational Level", y = "Hours spent on Children in Minutes")

"""












"""  Average Time spent on Baby care as per education

cps_comp %>% filter(!is.na(EducationLevel)) %>% group_by(EducationLevel) %>% summarise(n=sum(HoursspentonChildren), k=length(HoursspentonChildren), avg=n/k ) %>% arrange(-avg) %>% ungroup() -> dc


ggplot(data = dc, aes(x=reorder(dc$EducationLevel,k), y=avg))+  
  geom_bar(stat="identity", fill="magenta", colour="black") +
  coord_flip() + theme_bw(base_size = 10)  +
  labs(title="Average Time spent on Baby care as per Education", x ="Educational Level", y = "Time spent in Minutes")
"""




ggplot(data = dc, aes(x=dc$EducationLevel, y=dc$HoursspentonChildren, color= dc$EducationLevel))



cps <- data.frame(id, Edu, age, ageRange, empstatus,gen, child, weeklyEarnings, year,whw, sleeping, grooming,housework,fooddrinkprep,houronchild, jobsearch,shop,eatanddrink,socialize, golfing,running,volunteer,total, totalinmins)


f_ed <- subset(cps, Edu=="High School") #edu
f_g <- subset(f_ed, gender=="Female")#gender
f_chld <- subset(f_g, children>0)#children
f_empstat <- subset(f_chld, Empstat=="Employed")#empstat

lm_1 <- lm(age~ sleeping+grooming+housework+fooddrink+caringforchildren+playingwithchildren+jobsearching+shop+eatanddrink+socializeandrelax+television+golfing+running+volunteer+dailyworkmins, data=f_empstat)

summary(lm_1)


demo_dataframe <- data.frame(sleeping,grooming,housework,fooddrink,caringforchildren,playingwithchildren,jobsearching,shop,eatanddrink,socializeandrelax,television,golfing,running,volunteer,totalmin,dailyworkmins)





demo_dataframe <- data.frame(age,jobsearching,shop,eatanddrink,golfing,running,volunteer,total,sleeping,grooming,housework,fooddrinkprep,houronchild,weeklyEarnings,whw,socializeandrelax,empstatus2)




n <- cor(demo_dataframe)
corrplot(n, method = "number")


###################################################################
dev.off()

x <- data.frame(weeklyhw,sleeping,grooming,housework,fooddrink,caringforchildren,playingwithchildren,jobsearching,shop,eatanddrink,socializeandrelax,television,golfing,running,volunteer,totalmin,dailyworkmins)
y <- log10(x$caringforchildren)

s1 <- y
t1 <- playingwithchildren
cor_housetel <- cor(s1,tt1)
cor_housetel






mini_reg3 <- lm(totalmin~socializeandrelax)
summary(mini_reg3)


mini_reg4 <- lm(totalmin~socializeandrelax+television)
summary(mini_reg4)












mini_reg <- lm(totalmin~caringforchildren)
summary(mini_reg)


mini_reg2 <- lm(totalmin~caringforchildren+playingwithchildren)
summary(mini_reg2)


cor(caringforchildren,playingwithchildren)


mini_reg <- lm(caringforchildren~playingwithchildren)
summary(mini_reg)




#Regressing age on eatanddrink
"""
lm_demo1 <- lm(age ~ eatanddrink, data=f_empstat)

summary(lm_demo1)
cor_ageshop <- cor(housework,television)
cor_ageshop




