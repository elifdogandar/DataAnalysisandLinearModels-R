########### Loading the data
data1 <-  read.table(file="pharynx.txt",header=TRUE,sep=""
    ,col.names=c("case","inst","sex","tx","grade","age","cond",
                 "site","t_stage","n_stage","entry_dt","status","time"))
data<- data1
#since missing values are coded as 9  in cond and grade variables,I replaced them with NA's
data$grade <- ifelse(data$grade==9,NA,data$grade)
data$cond <- ifelse(data$cond==9,NA,data$cond)
datanew <- data[-c(1,11)]   #getting rid of the case and entry_dt variables 
############ Using Amelia package to fill NA's in data
install.packages("Amelia")
library(Amelia)
missmap(data)   #map of the missing values show that there doesnt seem to be a certain pattern in missing values
#and we can assume that they are random
#boundaries of the variables to be used in amelia function
bds <-  matrix(c(1, 1, 6,2,1,2,3,1,2,4,1,3,5,20,90,6,1,4,7,1,4,8,1,4,9,0,3,10,0,3,11,11,1823), nrow = 11, ncol = 3,byrow = TRUE)
set.seed(32)
a.out <- amelia(datanew,bounds=bds,empri = .01*nrow(datanew), m = 5, noms = c("inst","sex","tx","site","status"), 
                ords = c("grade","age","cond","t_stage","n_stage","time"),p2s = 0)
 plot(a.out, which.vars = 1:4)
 plot(a.out, which.vars = 5:8)
 plot(a.out, which.vars = 9:11)
 #observed and imputed values seem to have fairly similar distributions
 overimpute(a.out, var = "age") #if we treat observed values of age as missing and try to impute them only 5 percent of them 
 #would be predicted poorly
 overimpute(a.out, var = "time") #same for time variable
#5 different amelia imputations
out1 <- a.out$imputations[[1]]
out2 <- a.out$imputations[[2]]
out3 <- a.out$imputations[[3]]
out4 <- a.out$imputations[[4]]
out5 <- a.out$imputations[[5]]
#A function to compute mode of a vector
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#replacing factor variables with mode of 5 different imputations and continuos variables with median
datanew2<- datanew
for(i in 1:195){
datanew2$inst[i] <- getmode(c(out1$inst[i],out2$inst[i],out3$inst[i],out4$inst[i],out5$inst[i]))
datanew2$sex[i] <- getmode(c(out1$sex[i],out2$sex[i],out3$sex[i],out4$sex[i],out5$sex[i]))
datanew2$tx[i] <- getmode(c(out1$tx[i],out2$tx[i],out3$tx[i],out4$tx[i],out5$tx[i]))
datanew2$grade[i] <- getmode(c(out1$grade[i],out2$grade[i],out3$grade[i],out4$grade[i],out5$grade[i]))
datanew2$cond[i] <- getmode(c(out1$cond[i],out2$cond[i],out3$cond[i],out4$cond[i],out5$cond[i]))
datanew2$site[i] <- getmode(c(out1$site[i],out2$site[i],out3$site[i],out4$site[i],out5$site[i]))
datanew2$t_stage[i] <- getmode(c(out1$t_stage[i],out2$t_stage[i],out3$t_stage[i],out4$t_stage[i],out5$t_stage[i]))
datanew2$n_stage[i] <- getmode(c(out1$n_stage[i],out2$n_stage[i],out3$n_stage[i],out4$n_stage[i],out5$n_stage[i]))
datanew2$status[i] <- getmode(c(out1$status[i],out2$status[i],out3$status[i],out4$tstatus[i],out5$status[i]))
datanew2$age[i] <- median(c(out1$age[i],out2$age[i],out3$age[i],out4$age[i],out5$age[i]))
datanew2$time[i] <- median(c(out1$time[i],out2$time[i],out3$time[i],out4$time[i],out5$time[i]))
}
datafinal<- datanew2
#standardizing continuous regressors
scaled.age <- scale(datafinal$age)
datafinal$age <- scaled.age

###### Explaratory Data analysis
cormat <- cor(datafinal) # only high correlation is between status and time -0.65, which makes sense, time for alive is higher than dead.
datafinal$inst <- as.factor(datafinal$inst)
datafinal$sex <- as.factor(datafinal$sex)
datafinal$tx <- as.factor(datafinal$tx)
datafinal$site <- as.factor(datafinal$site)
datafinal$status <- as.factor(datafinal$status)
install.packages("ggplot2")
library(ggplot2)
attach(datafinal)

ggplot()+aes(x=tx,fill=factor(sex))+geom_bar()   #counts of treatment by sex
#barplot of a table which shows the percentage of men and women in instutition variable
table1 <- table(sex,inst)
barplot(prop.table(table1,2),col=c("red","yellow"),ylab="percentage of male ",xlab="institutions by sex") 

#barplot for condition
plot(as.factor(cond),xlab="condition") #observations with condition 4 seems to be too less,
#it wont say much and cause problems most probably
table(cond)

ggplot(datafinal, aes(x=factor(inst),y=time))+ geom_boxplot() #boxplot of variable time by institution
qplot(time, data=datafinal, fill=factor(sex), binwidth=150)  #histogram of time by sex

ggplot(datafinal, aes(x=factor(tx),y=time))+ geom_boxplot() #boxplot of time by treatment

#relation between t_stage and status
table3 <- table(status,t_stage)
prop.table(table3,2)
barplot(prop.table(table3,2),ylab="percentage of alive",xlab="t_stage",col=c("red","yellow"))
#scatter plot of age and time
ggplot(datafinal, aes(x =age , y = time)) +  geom_point()+ geom_smooth(se=FALSE) 
#relation between age time and status
ggplot(datafinal, aes(x =age , y = time, col = factor(status))) +  geom_point()+geom_smooth(se=FALSE) 
#relation between condition, time and status
ggplot(datafinal, aes(x =time , y = cond, col = factor(status))) +  geom_point()+geom_smooth(se=FALSE) 

#setting grade,cond, t_stage and n_stage variables as factors because the meaning of one 
#unit change is not fixed
datafinal$grade <- as.factor(datafinal$grade)  
datafinal$cond <- as.factor(datafinal$cond) 
datafinal$t_stage <- as.factor(datafinal$t_stage) 
datafinal$n_stage <-as.factor(datafinal$n_stage)  

######## Model building
model1 <- lm(time~.,datafinal)  # building a linear model
summary(model1)  #r^2 of the model is 0.5491 and model is significant 
par(mfrow=c(2,2))
plot(model1)    #from plots there doesnt seem to be a big problem

#normality check
shapiro.test(time)   #p value is almost zero which means dependent variable is not normally distributed
installed.packages("MASS")
library(MASS)
boxcox(model1) #boxcox suggest transformation of time : time^(1/3)
timenew <- datafinal$time^(1/3)
shapiro.test(timenew)   #after transformation pvalue is 0.085, normality problem is solved
datafinal$time <- timenew

model2 <- lm(time ~. ,datafinal)   # new linear model with transformed time variable
summary(model2)
par(mfrow=c(2,2))
plot(model2)   #this model has r^2 0.5719
null <- lm(time~1,datafinal)

# Creating folds for 5-fold cross validation
install.packages("caret")
library(caret)
set.seed(77)
partitions <- createFolds(datafinal$time,k=5)

test1 <- datafinal[partitions$Fold1,]
test2 <- datafinal[partitions$Fold2,]
test3 <- datafinal[partitions$Fold3,]
test4 <- datafinal[partitions$Fold4,]
test5 <- datafinal[partitions$Fold5,]

training1 <-datafinal[-partitions$Fold1,] 
training2 <-datafinal[-partitions$Fold2,] 
training3 <-datafinal[-partitions$Fold3,] 
training4 <-datafinal[-partitions$Fold4,] 
training5 <-datafinal[-partitions$Fold5,] 

#sse of model2
model2r <- c(0,0,0,0,0)
model2r1 <- lm(time ~. ,training1)
model2r1pred <- predict(model2r1,test1)
model2r[1] <- sum((test1$time-model2r1pred)^2)
model2r2 <- lm(time ~. ,training2)
model2r2pred <- predict(model2r2,test2)
model2r[2] <- sum((test2$time-model2r2pred)^2)
model2r3 <- lm(time ~. ,training3)
model2r3pred <- predict(model2r3,test3)
model2r[3] <- sum((test3$time-model2r3pred)^2)
model2r4 <- lm(time ~. ,training4)
model2r4pred <- predict(model2r4,test4)
model2r[4] <- sum((test4$time-model2r4pred)^2)
model2r5 <- lm(time ~. ,training5)
model2r5pred <- predict(model2r5,test5)
model2r[5] <- sum((test5$time-model2r5pred)^2)

#it gives an error of "factor cond has new levels 4", appereantly variable condition having 
#value 4 only once is causing problem. ill remove that observation 
table(datafinal$cond)
which(datafinal$cond==4)
datafinal <- datafinal[-176,]

#repeating the procedure with the new datafinal
model2 <- lm(time ~. ,datafinal)
summary(model2) # r^2 of this model is still 0.5719
par(mfrow=c(2,2))
plot(model2)
null <- lm(time~1,datafinal)

#5 fold cross validation
installed.packages("caret")
library(caret)
set.seed(77)
partitions <- createFolds(datafinal$time,k=5)

test1 <- datafinal[partitions$Fold1,]
test2 <- datafinal[partitions$Fold2,]
test3 <- datafinal[partitions$Fold3,]
test4 <- datafinal[partitions$Fold4,]
test5 <- datafinal[partitions$Fold5,]

training1 <-datafinal[-partitions$Fold1,] 
training2 <-datafinal[-partitions$Fold2,] 
training3 <-datafinal[-partitions$Fold3,] 
training4 <-datafinal[-partitions$Fold4,] 
training5 <-datafinal[-partitions$Fold5,] 

#sse of model2
model2r <- c(0,0,0,0,0)
model2r1 <- lm(time ~. ,training1)
model2r1pred <- predict(model2r1,test1)
model2r[1] <- sum((test1$time-model2r1pred)^2)
model2r2 <- lm(time ~. ,training2)
model2r2pred <- predict(model2r2,test2)
model2r[2] <- sum((test2$time-model2r2pred)^2)
model2r3 <- lm(time ~. ,training3)
model2r3pred <- predict(model2r3,test3)
model2r[3] <- sum((test3$time-model2r3pred)^2)
model2r4 <- lm(time ~. ,training4)
model2r4pred <- predict(model2r4,test4)
model2r[4] <- sum((test4$time-model2r4pred)^2)
model2r5 <- lm(time ~. ,training5)
model2r5pred <- predict(model2r5,test5)
model2r[5] <- sum((test5$time-model2r5pred)^2)

model2sse <- sum(model2r)/5   # sse of model2 is 80.11

#variable selection
stepwise <- step(null, list(lower=formula(null),upper=formula(model2)),direction="both",trace=0)
formula(stepwise)
forward <- step(null, list(lower=formula(null),upper=formula(model2)),direction="forward",trace=0)
formula(forward)
backward <- step(model2)
formula(backward)
#stepwise and forward are giving the same formula so ill just compare one of them namely stepwise with model2 and backward
#sse for backward
backwardr <- c(0,0,0,0,0)
backwardr1 <- lm(time~inst + sex + tx + grade + age + cond +site+n_stage+ status,training1)
backwardr1pred <- predict(backwardr1,test1)
backwardr[1] <- sum((test1$time-backwardr1pred)^2)
backwardr2 <- lm(time~inst + sex + tx + grade + age + cond +site+n_stage+  status,training2)
backwardr2pred <- predict(backwardr2,test2)
backwardr[2] <- sum((test2$time-backwardr2pred)^2)
backwardr3 <- lm(time~inst + sex + tx + grade + age + cond +site+n_stage+  status,training3)
backwardr3pred <- predict(backwardr3,test3)
backwardr[3] <- sum((test3$time-backwardr3pred)^2)
backwardr4 <- lm(time~inst + sex + tx + grade + age + cond +site+n_stage+  status,training4)
backwardr4pred <- predict(backwardr4,test4)
backwardr[4] <- sum((test4$time-backwardr4pred)^2)
backwardr5 <- lm(time~inst + sex + tx + grade + age + cond +site+n_stage+  status,training5)
backwardr5pred <- predict(backwardr5,test5)
backwardr[5] <- sum((test5$time-backwardr5pred)^2)

backwardsse <- sum(backwardr)/5    # sse of backward is 76.85

#sse of stepwise
stepwiser <- c(0,0,0,0,0)
stepwiser1 <- lm(time~status + cond + n_stage + tx + age + grade + inst + sex ,training1)
stepwiser1pred <- predict(stepwiser1,test1)
stepwiser[1] <- sum((test1$time-stepwiser1pred)^2)
stepwiser2 <- lm(time~status + cond + n_stage + tx + age + grade + inst + sex ,training2)
stepwiser2pred <- predict(stepwiser2,test2)
stepwiser[2] <- sum((test2$time-stepwiser2pred)^2)
stepwiser3 <- lm(time~status + cond + n_stage + tx + age + grade + inst + sex ,training3)
stepwiser3pred <- predict(stepwiser3,test3)
stepwiser[3] <- sum((test3$time-stepwiser3pred)^2)
stepwiser4 <- lm(time~status + cond + n_stage + tx + age + grade + inst + sex ,training4)
stepwiser4pred <- predict(stepwiser4,test4)
stepwiser[4] <- sum((test4$time-stepwiser4pred)^2)
stepwiser5 <- lm(time~status + cond + n_stage + tx + age + grade + inst + sex ,training5)
stepwiser5pred <- predict(stepwiser5,test5)
stepwiser[5] <- sum((test5$time-stepwiser5pred)^2)

stepwisesse <- sum(stepwiser)/5    # sse of stepwise 77.48
# backward has both smallest number or variables and smallest sse so we will use backward 
#as our final model till now

model3 <- backward
model3sse<- backwardsse

#uncorraleted errors
install.packages("lmtest")
library(lmtest)
dwtest(model3) #p-value is 0.4982, there is no autocorrelation

#heteroskedasticity check
par(mfrow=c(2,2))
plot(model3)    # from plots it looks like variance is constant
installed.packages("car")
library(car)
ncvTest(model3)    #pvalue is 0.18, so we have constant variance 

#checking vif's
vif(model3)   #non of the vifs is bigger than 10, so we dont have a multicollinearity problem

#checking influential points
summary(influence.measures(model3))
#19,59,89 and 186 looks like influential, ill delete them
datafinalout <- datafinal[-c(19,59,89,186),]
model4<- lm(time ~. ,datafinalout)
nullout <-lm(time~.,datafinalout)
stepwise2 <-step(nullout, list(lower=formula(nullout),upper=formula(model4)),direction="both",trace=0)
formula(stepwise2)
backward2 <-step(model4)
formula(backward2)
forward2 <-step(nullout, list(lower=formula(nullout),upper=formula(model4)),direction="forward",trace=0)
formula(forward2) # stepwise2 and forward2 has the same formula. ill pick stepwise2 to compare with 
#backward2 and model4

#5fold cross validation for datafinalout
set.seed(75)
partitions <- createFolds(datafinalout$time,k=5)

test1 <- datafinalout[partitions$Fold1,]
test2 <- datafinalout[partitions$Fold2,]
test3 <- datafinalout[partitions$Fold3,]
test4 <- datafinalout[partitions$Fold4,]
test5 <- datafinalout[partitions$Fold5,]

training1 <-datafinalout[-partitions$Fold1,] 
training2 <-datafinalout[-partitions$Fold2,] 
training3 <-datafinalout[-partitions$Fold3,] 
training4 <-datafinalout[-partitions$Fold4,] 
training5 <-datafinalout[-partitions$Fold5,] 

#sse for stepwise2
stepwise2r <- c(0,0,0,0,0)
stepwise2r1 <- lm(time~inst + sex + tx + grade + age + cond + site + t_stage +
                    n_stage + status ,training1)
stepwise2r1pred <- predict(stepwise2r1,test1)
stepwise2r[1] <- sum((test1$time-stepwise2r1pred)^2)
stepwise2r2 <- lm(time~inst + sex + tx + grade + age + cond + site + t_stage +
                    n_stage + status ,training2)
stepwise2r2pred <- predict(stepwise2r2,test2)
stepwise2r[2] <- sum((test2$time-stepwise2r2pred)^2)
stepwise2r3 <- lm(time~inst + sex + tx + grade + age + cond + site + t_stage +
                    n_stage + status ,training3)
stepwise2r3pred <- predict(stepwise2r3,test3)
stepwise2r[3] <- sum((test3$time-stepwise2r3pred)^2)
stepwise2r4 <- lm(time~inst + sex + tx + grade + age + cond + site + t_stage +
                    n_stage + status ,training4)
stepwise2r4pred <- predict(stepwise2r4,test4)
stepwise2r[4] <- sum((test4$time-stepwise2r4pred)^2)
stepwise2r5 <- lm(time~inst + sex + tx + grade + age + cond + site + t_stage +
                    n_stage + status ,training5)
stepwise2r5pred <- predict(stepwise2r5,test5)
stepwise2r[5] <- sum((test5$time-stepwise2r5pred)^2)

stepwise2sse <- sum(stepwise2r)/5   #sse of stepwise2 is 73.25

#sse for backward2
backward2r <- c(0,0,0,0,0)
backward2r1 <- lm(time~inst + sex + tx + grade + age + cond + status,training1)
backward2r1pred <- predict(backward2r1,test1)
backward2r[1] <- sum((test1$time-backward2r1pred)^2)
backward2r2 <- lm(time~inst + sex + tx + grade + age + cond + status,training2)
backward2r2pred <- predict(backward2r2,test2)
backward2r[2] <- sum((test2$time-backward2r2pred)^2)
backward2r3 <- lm(time~inst + sex + tx + grade + age + cond + status,training3)
backward2r3pred <- predict(backward2r3,test3)
backward2r[3] <- sum((test3$time-backward2r3pred)^2)
backward2r4 <- lm(time~inst + sex + tx + grade + age + cond + status,training4)
backward2r4pred <- predict(backward2r4,test4)
backward2r[4] <- sum((test4$time-backward2r4pred)^2)
backward2r5 <- lm(time~inst + sex + tx + grade + age + cond + status,training5)
backward2r5pred <- predict(backward2r5,test5)
backward2r[5] <- sum((test5$time-backward2r5pred)^2)

backward2sse <- sum(backward2r)/5     #sse of backward2 is 80.93
#sse for model4
model4r <- c(0,0,0,0,0)
model4r1 <- lm(time ~. ,training1)
model4r1pred <- predict(model4r1,test1)
model4r[1] <- sum((test1$time-model4r1pred)^2)
model4r2 <- lm(time ~. ,training2)
model4r2pred <- predict(model4r2,test2)
model4r[2] <- sum((test2$time-model4r2pred)^2)
model4r3 <- lm(time ~. ,training3)
model4r3pred <- predict(model4r3,test3)
model4r[3] <- sum((test3$time-model4r3pred)^2)
model4r4 <- lm(time ~. ,training4)
model4r4pred <- predict(model4r4,test4)
model4r[4] <- sum((test4$time-model4r4pred)^2)
model4r5 <- lm(time ~. ,training5)
model4r5pred <- predict(model4r5,test5)
model4r[5] <- sum((test5$time-model4r5pred)^2)

model4sse <- sum(model4r)/5   #sse for model4 is 73.25

#among model4,stepwise2 and backward2, backward2 has the best sse and the least variable number. 

#while comparing model 3 and backward2, sse of the model without outliers is less than
#the one with outliers. Also R^2 improved a little. So we will use backward2 instead of model3
#so backward2 is our final model till now.


########## Interaction effects

interaction.plot(t_stage,sex,time) #add t_stage*sex
interaction.plot(tx,sex,time)      #add tx*sex
interaction.plot(grade,sex,time)   #add grade*sex
interaction.plot(cond,sex,time)    #no need
interaction.plot(status,sex,time)  #no need
interaction.plot(grade,tx,time)    #add grade*tx
interaction.plot(cond,tx,time)     #no need
interaction.plot(t_stage,tx,time)  #add t_stage*tx
interaction.plot(status,tx,time)    #no need
interaction.plot(cond,grade,time)    #add grade*cond
interaction.plot(t_stage,grade,time) #add grade*t_stage
interaction.plot(status,grade,time)    #no need
interaction.plot(t_stage,cond,time)    #add t_stage*cond
interaction.plot(cond,status,time)    #add status*cond
interaction.plot(t_stage,status,time)    #add status*t_stage

#after adding possible interaction effects
model5<- lm(time ~inst + sex + tx + grade + age + cond + n_stage + status+t_stage*sex+tx*sex+grade*sex+grade*tx+t_stage*tx+grade*cond+grade*t_stage+t_stage*cond+status*cond+status*t_stage ,datafinalout)
#it resulted NA coefficients
model6 <-lm(time ~status + cond + n_stage + tx + age + grade + inst + sex+tx*sex+grade*sex+grade*tx+status*cond,datafinalout)
null6 <- lm(time~1,datafinalout)
stepwise6 <-step(null6, list(lower=formula(null6),upper=formula(model6)),direction="both",trace=0)
formula(stepwise6)
backward6 <-step(model6)
formula(backward6)
forward6 <-step(null6, list(lower=formula(null6),upper=formula(model6)),direction="forward",trace=0)
formula(forward6) 

#all of these models give the same formula. Lets find sse of forward6 and compare with model6 and backward2.
#sse for model6
model6r <- c(0,0,0,0,0)
model6r1 <- lm(time ~. ,training1)
model6r1pred <- predict(model6r1,test1)
model6r[1] <- sum((test1$time-model6r1pred)^2)
model6r2 <- lm(time ~. ,training2)
model6r2pred <- predict(model6r2,test2)
model6r[2] <- sum((test2$time-model6r2pred)^2)
model6r3 <- lm(time ~. ,training3)
model6r3pred <- predict(model6r3,test3)
model6r[3] <- sum((test3$time-model6r3pred)^2)
model6r4 <- lm(time ~. ,training4)
model6r4pred <- predict(model6r4,test4)
model6r[4] <- sum((test4$time-model6r4pred)^2)
model6r5 <- lm(time ~. ,training5)
model6r5pred <- predict(model6r5,test5)
model6r[5] <- sum((test5$time-model6r5pred)^2)

model6sse <- sum(model6r)/5   #sse for model6 is 72.29

#sse for forward6
forward6r <- c(0,0,0,0,0)
forward6r1 <- lm(time ~. ,training1)
forward6r1pred <- predict(forward6r1,test1)
forward6r[1] <- sum((test1$time-forward6r1pred)^2)
forward6r2 <- lm(time ~. ,training2)
forward6r2pred <- predict(forward6r2,test2)
forward6r[2] <- sum((test2$time-forward6r2pred)^2)
forward6r3 <- lm(time ~. ,training3)
forward6r3pred <- predict(forward6r3,test3)
forward6r[3] <- sum((test3$time-forward6r3pred)^2)
forward6r4 <- lm(time ~. ,training4)
forward6r4pred <- predict(forward6r4,test4)
forward6r[4] <- sum((test4$time-forward6r4pred)^2)
forward6r5 <- lm(time ~. ,training5)
forward6r5pred <- predict(forward6r5,test5)
forward6r[5] <- sum((test5$time-forward6r5pred)^2)

forward6sse <- sum(forward6r)/5   #sse for forward6 is 72.29


#among these forward6 has the best R^2 value, but sse of it is slightly more than the model without interaction.
#Therefore there is no need to add the interaction term to the model. 

#so backward2 is the final model we use till now.

##################### Analysis with the new variable newdate
xx <-data1$time[data1$status==0]
nna<- xx[!is.na(xx)]
plot(1:length(nna),nna, xlab="time",ylab="time for status 0")   # entry date variable has an increasing order except the last observation, 
#so when we plot non na values of this variable we will see the effect of it on time for status 0, 
#there is clear negative linear relation

yy <-data1$time[data1$status==1]
nnay<- yy[!is.na(yy)]
plot(1:length(nnay),nnay,xlab="time",ylab="time for status 1") #same for status 1, nut this time there is no clear relation

#transformation of the variable entry_date
#(data$entry_dt%%100) gives the year of the entry
#(data$entry_dt%/%100) give the day of the year
newdate <-(365*((data$entry_dt%%100)-68)) +(data$entry_dt%/%100) #newdate gives the date as days 
#taking first day of 1968 as reference point
newdata <- data
newdata$entry_dt <- newdate   #transforming entry_dt
newdata<- newdata[,-1]        # deleting unnecessary variable case

#filling na s with amelia
bds2 <-  matrix(c(1, 1, 6,2,1,2,3,1,2,4,1,3,5,20,90,6,1,4,7,1,4,8,1,4,9,0,3,10,24,1708,11,0,1,12,11,1823), nrow = 12, ncol = 3,byrow = TRUE)
set.seed(48)
a.out2 <- amelia(newdata,bounds=bds2,empri = .01*nrow(newdata), m = 5, noms = c("inst","sex","tx","site","status"), ords = c("grade","age","cond","t_stage","n_stage","time","entry_dt"),p2s = 0)

# table(a.out$imputations[[3]]$grade)
plot(a.out2, which.vars = 1:12)
overimpute(a.out2, var = "age")
overimpute(a.out2, var = "time")
overimpute(a.out2, var = "entry_dt")
write.amelia(a.out2,separate = TRUE,file.stem = "amelia2")

out12 <- a.out2$imputations[[1]]
out22 <- a.out2$imputations[[2]]
out32 <- a.out2$imputations[[3]]
out42 <- a.out2$imputations[[4]]
out52 <- a.out2$imputations[[5]]
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#replacing factors with mode and continuos variables with median
newdata2<- newdata
for(i in 1:195){
  newdata2$inst[i] <- getmode(c(out12$inst[i],out22$inst[i],out32$inst[i],out42$inst[i],out52$inst[i]))
  newdata2$sex[i] <- getmode(c(out12$sex[i],out22$sex[i],out32$sex[i],out42$sex[i],out52$sex[i]))
  newdata2$tx[i] <- getmode(c(out12$tx[i],out22$tx[i],out32$tx[i],out42$tx[i],out52$tx[i]))
  newdata2$grade[i] <- getmode(c(out12$grade[i],out22$grade[i],out32$grade[i],out42$grade[i],out52$grade[i]))
  newdata2$cond[i] <- getmode(c(out12$cond[i],out22$cond[i],out32$cond[i],out42$cond[i],out52$cond[i]))
  newdata2$site[i] <- getmode(c(out12$site[i],out22$site[i],out32$site[i],out42$site[i],out52$site[i]))
  newdata2$t_stage[i] <- getmode(c(out12$t_stage[i],out22$t_stage[i],out32$t_stage[i],out42$t_stage[i],out52$t_stage[i]))
  newdata2$n_stage[i] <- getmode(c(out12$n_stage[i],out22$n_stage[i],out32$n_stage[i],out42$n_stage[i],out52$n_stage[i]))
  newdata2$status[i] <- getmode(c(out12$status[i],out22$status[i],out32$status[i],out42$tstatus[i],out52$status[i]))
  newdata2$age[i] <- median(c(out12$age[i],out22$age[i],out32$age[i],out42$age[i],out52$age[i]))
  newdata2$time[i] <- median(c(out12$time[i],out22$time[i],out32$time[i],out42$time[i],out52$time[i]))
  newdata2$entry_dt[i] <- median(c(out12$entry_dt[i],out22$entry_dt[i],out32$entry_dt[i],out42$entry_dt[i],out52$entry_dt[i]))
  
  }

datafinal2<- newdata2
scaled.age <- scale(datafinal2$age)
datafinal2$age <- scaled.age
scaled.entry_dt <- scale(datafinal2$entry_dt)
datafinal2$entry_dt <- scaled.entry_dt

cormat2 <- cor(datafinal2)
datafinal2$inst <- as.factor(datafinal2$inst)
datafinal2$sex <- as.factor(datafinal2$sex)
datafinal2$tx <- as.factor(datafinal2$tx)
datafinal2$site <- as.factor(datafinal2$site)
datafinal2$status <- as.factor(datafinal2$status)
datafinal2$grade <- as.factor(datafinal2$grade)  
datafinal2$cond <- as.factor(datafinal2$cond) 
datafinal2$t_stage <- as.factor(datafinal2$t_stage) 
datafinal2$n_stage <-as.factor(datafinal2$n_stage)
#getting rid of the problematic observation 176
datafinal2 <- datafinal2[-176,]

#model building
model1_2 <- lm(time~.+status*entry_dt,datafinal2)
summary(model1_2)          # r^2 of this model is 0.71
plot(model1_2)


#normality check
shapiro.test(time)   # dependent variable is not normal, pvalue is approximately zero
boxcox(model1_2) #boxcox suggest transformation of time : time^(1/3)
timenew2 <- datafinal2$time^(1/3)
shapiro.test(timenew2)  # transformed dependent variable is approximately normal p value=0.07
datafinal2$time <- timenew2


model2_2 <- lm(time ~.+status*entry_dt ,datafinal2)
summary(model2_2)      # r^2 for this model is 0.64
plot(model2_2)

nulln <- lm(time~1,datafinal2)

#5fold cross validation for datafinal2
set.seed(130)
partitions <- createFolds(datafinal2$time,k=5)

test1 <- datafinal2[partitions$Fold1,]
test2 <- datafinal2[partitions$Fold2,]
test3 <- datafinal2[partitions$Fold3,]
test4 <- datafinal2[partitions$Fold4,]
test5 <- datafinal2[partitions$Fold5,]

training1 <-datafinal2[-partitions$Fold1,] 
training2 <-datafinal2[-partitions$Fold2,] 
training3 <-datafinal2[-partitions$Fold3,] 
training4 <-datafinal2[-partitions$Fold4,] 
training5 <-datafinal2[-partitions$Fold5,] 




#variable selection
stepwisen <- step(nulln, list(lower=formula(nulln),upper=formula(model2_2)),direction="both",trace=0)
formula(stepwisen)
forwardn <- step(nulln, list(lower=formula(nulln),upper=formula(model2_2)),direction="forward",trace=0)
formula(forwardn)
backwardn <- step(model2_2)
formula(backwardn)
#stepwisen and forwardn are giving the same results, so ill pick one of them stepwisen and compare stepwisen, backwardn and model2_2

#sse for stepwisen
stepwisenr <- c(0,0,0,0,0)
stepwisenr1 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt +status + entry_dt*status ,training1)
stepwisenr1pred <- predict(stepwisenr1,test1)
stepwisenr[1] <- sum((test1$time-stepwisenr1pred)^2)
stepwisenr2 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt +status + entry_dt*status ,training2)
stepwisenr2pred <- predict(stepwisenr2,test2)
stepwisenr[2] <- sum((test2$time-stepwisenr2pred)^2)
stepwisenr3 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt +status + entry_dt*status ,training3)
stepwisenr3pred <- predict(stepwisenr3,test3)
stepwisenr[3] <- sum((test3$time-stepwisenr3pred)^2)
stepwisenr4 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt +status + entry_dt*status  ,training4)
stepwisenr4pred <- predict(stepwisenr4,test4)
stepwisenr[4] <- sum((test4$time-stepwisenr4pred)^2)
stepwisenr5 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt +status + entry_dt*status  ,training5)
stepwisenr5pred <- predict(stepwisenr5,test5)
stepwisenr[5] <- sum((test5$time-stepwisenr5pred)^2)

stepwisensse <- sum(stepwisenr)/5      # sse of stepwisen is 66.53

#sse for backwardn
backwardnr <- c(0,0,0,0,0)
backwardnr1 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt + 
                    status + entry_dt*status,training1)
backwardnr1pred <- predict(backwardnr1,test1)
backwardnr[1] <- sum((test1$time-backwardnr1pred)^2)
backwardnr2 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt + 
                    status + entry_dt*status,training2)
backwardnr2pred <- predict(backwardnr2,test2)
backwardnr[2] <- sum((test2$time-backwardnr2pred)^2)
backwardnr3 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt + 
                    status + entry_dt*status,training3)
backwardnr3pred <- predict(backwardnr3,test3)
backwardnr[3] <- sum((test3$time-backwardnr3pred)^2)
backwardnr4 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt + 
                    status + entry_dt*status,training4)
backwardnr4pred <- predict(backwardnr4,test4)
backwardnr[4] <- sum((test4$time-backwardnr4pred)^2)
backwardnr5 <- lm(time~sex + tx + grade + age + cond + site + n_stage + entry_dt + 
                    status + entry_dt*status,training5)
backwardnr5pred <- predict(backwardnr5,test5)
backwardnr[5] <- sum((test5$time-backwardnr5pred)^2)

backwardnsse <- sum(backwardnr)/5    # sse of backwardn is 66.53
#sse for model2_2
model2_2r <- c(0,0,0,0,0)
model2_2r1 <- lm(time ~.+status*entry_dt  ,training1)
model2_2r1pred <- predict(model2_2r1,test1)
model2_2r[1] <- sum((test1$time-model2_2r1pred)^2)
model2_2r2 <- lm(time ~.+status*entry_dt  ,training2)
model2_2r2pred <- predict(model2_2r2,test2)
model2_2r[2] <- sum((test2$time-model2_2r2pred)^2)
model2_2r3 <- lm(time ~.+status*entry_dt  ,training3)
model2_2r3pred <- predict(model2_2r3,test3)
model2_2r[3] <- sum((test3$time-model2_2r3pred)^2)
model2_2r4 <- lm(time ~.+status*entry_dt  ,training4)
model2_2r4pred <- predict(model2_2r4,test4)
model2_2r[4] <- sum((test4$time-model2_2r4pred)^2)
model2_2r5 <- lm(time ~.+status*entry_dt  ,training5)
model2_2r5pred <- predict(model2_2r5,test5)
model2_2r[5] <- sum((test5$time-model2_2r5pred)^2)

model2_2sse <- sum(model4r)/5    #sse of model2_2  73.25
#among these 3 stepwisen gives the least error and has the least number of variables  
#so ill pick that model as my final model with the new variable, and call it as model3_2

model3_2 <- stepwisen
model3_2sse<- stepwisensse


#uncorraleted errors
installed.packages("lmtest")
library(lmtest)
dwtest(model3_2) #there is no autocorrelation

#heteroskedasticity check
par(mfrow=c(2,2))
plot(model3_2)
installed.packages("car")
library(car)
ncvTest(model3_2)  # p value is 0.04, less than 0.05 but still acceptable

#checking vif's
vif(model3_2)    #no collinearity problem


#checking influential points
summary(influence.measures(model3_2))
#14,19,22,31,89 and 186 looks like influential, ill delete them
datafinal2out <- datafinal2[-c(14,19,22,31,89,186),]
model4_2<- lm(time ~.+entry_dt*status ,datafinal2out)
null4_2 <- lm(time ~1 ,datafinal2out)

stepwise2_2 <-step(null4_2, list(lower=formula(null4_2),upper=formula(model4_2)),direction="both",trace=0)
formula(stepwise2_2)
backward2_2 <-step(model4_2)
formula(backward2_2)
forward2_2 <-step(null4_2, list(lower=formula(null4_2),upper=formula(model4_2)),direction="forward",trace=0)
formula(forward2_2)
#all stepwise models are same. Adjusted r squares are similar. so no need for a more complicated model.variable numbers are 7 and  11.
#SO our final model is model3_2

model3_2sse
model3sse
#sse of model 3_2 is better so our final model will be model3_2