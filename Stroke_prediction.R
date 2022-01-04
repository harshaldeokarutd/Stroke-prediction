rm(list=ls())
#install.packages("writexl")
library("writexl")
library("readxl")
library(gains)
library(caret)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)
library(smotefamily)
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

setwd("D:/UTD BUAN Course/BA with R/Assignments/Term Project/Dataset/")
sd_df <- read_excel("Stroke_data.xlsx")
summary(sd_df)
summary(sd_df$gender)
sd_df[sd_df == 'Other'] <- 'Female'
unique(sd_df$gender)


# BMI column needs to be treated
summary(sd_df['bmi'])
unique(sd_df['bmi'])
sd_df[sd_df == 'N/A'] <- NA
summary(sd_df)
which(is.na(sd_df['bmi']))
length(which(is.na(sd_df['bmi'])))
# Replace n/a with mean of bmi
check<-split(sd_df, sd_df$gender)
df_m <- check$Male
df_f <- check$Female
df_m$bmi <- as.integer(df_m$bmi)
df_f$bmi <- as.integer(df_f$bmi)

df_m$bmi[is.na(df_m$bmi)]<-mean(df_m$bmi,na.rm=TRUE)
df_f$bmi[is.na(df_f$bmi)]<-mean(df_f$bmi,na.rm=TRUE)

df <- rbind(df_m,df_f)
data_proportion_gender<- df %>%
  group_by(gender)%>%
  summarise(prop=sum(stroke==1)/length(gender))
g1<-ggplot(data_proportion_gender,aes(x=gender,y=prop,fill=gender))+geom_col()
g1

#comparing the proportions within the different married type who got stroke
data_proportion_married<- df%>%
  group_by(ever_married)%>%
  summarise(prop=sum(stroke==1)/length(ever_married))

g2<-ggplot(data_proportion_married,aes(x=ever_married,y=prop,fill=ever_married))+geom_col()
g2

#Comparing the proportions of people who have hypertension who got stroke
data_proportion_hypertension<- df%>%
  group_by(hypertension)%>%
  summarise(prop=sum(stroke==1)/length(hypertension))



g3<-ggplot(data_proportion_hypertension,aes(x=hypertension,y=prop,fill=hypertension))+geom_col()
g3


data_proportion_heart_disease<- df%>%
  group_by(heart_disease)%>%
  summarise(prop=sum(stroke==1)/length(heart_disease))

data_proportion_heart_disease$heart_disease <- as.factor(data_proportion_heart_disease$heart_disease)

g4<-ggplot(data_proportion_heart_disease,aes(x=heart_disease,y=prop,fill=heart_disease))+geom_col()
g4


#Plotting together in the form of a grid
grid.arrange(grobs=list(g1,g2,g3,g4,g5,g6),ncol=3, top = "Proportion of Strokes for Each Factor")


#Comparing the boxplot for different factors with age
b1<-df %>%
  ggplot(aes(x=as.factor(gender),y=age,color=as.factor(stroke)))+geom_boxplot()
b1 + labs(x="Gender",y="Age")
b2<-df %>%
  ggplot(aes(x=as.factor(hypertension),y=age,color=as.factor(stroke)))+geom_boxplot()
b2 + labs(x="Hypertension",y="Age")
b3<-df %>%
  ggplot(aes(x=as.factor(ever_married),y=age,color=as.factor(stroke)))+geom_boxplot()
b3 + labs(x="Ever Married",y="Age")
b4<-df %>%
  ggplot(aes(x=as.factor(heart_disease),y=age,color=as.factor(stroke)))+geom_boxplot()
b4 + labs(x="Heart Disease",y="Age")
b5<-df %>%
  ggplot(aes(x=as.factor(smoking_status),y=age,color=as.factor(stroke)))+geom_boxplot()
b5 + labs(x="Smoking Status",y="Age")
b6<-df %>%
  ggplot(aes(x=as.factor(work_type),y=age,color=as.factor(stroke)))+geom_boxplot()
b6 + labs(x="Work Type",y="Age")
b7<-df %>%
  ggplot(aes(x=as.factor(Residence_type),y=age,color=as.factor(stroke)))+geom_boxplot()
b7 + labs(x="Residence Type",y="Age")
#plotting the density plot and the histogram for the continuous parameters
stroke_df1 <- as.factor(df)


#Plotting together in the form of a grid
grid.arrange(grobs=list(g1,g2,g3,g4),ncol=3, top = "Proportion of Strokes for Each Factor")
grid.arrange(grobs=list(b1,b2,b3,b4,b5,b6,b7),ncol=3, top = "Proportion of Strokes for Each Factor")

df$bmi<- round(df$bmi,digits=2)
unique(df$smoking_status)
length(which(df$smoking_status == 'Unknown'))
summary(df$age)
boxplot(df$age,main="Age Box Plot")
boxplot(df$bmi,main="BMI Box Plot")


summary(df$bmi)
IQR(df$bmi)
length(which(df$bmi > (32+1.5*9)))
length(which(df$bmi > (50)))
df[which(df$bmi > 50),]
df %>% filter(smoking_status == 'smokes',work_type == 'children')
df %>% filter(smoking_status == 'formerly smoked',work_type == 'children')

###################### Check code as on 1215 hours 11302021 #######################
summary(df)
child_df <- df[which(df$work_type == 'children'),]
mean(child_df$age)
boxplot(child_df$age)
summary(child_df$age)
length(which(child_df$smoking_status == 'formerly smoked'))
# To be decided
#df$age[which(df$age < 6)] <- mean(df$age)
length(which(child_df$smoking_status == "Unknown"))
barchart(x=df$work_type,df$stroke,main ="Work Type",xlab="Frequency")
########################  Check code ends ###########################################
# Replace the outliers with 50.
# IQR suggests 45.5 but there are 65 values between 45 and 50. Scientifically, the BMI is capped at 50 
# which is termed as morbidly obese 
# there are high levels of bmi but they are uncommon and would affect the model prediction.
df$bmi[df$bmi > 50] <- 50
boxplot(df$avg_glucose_level)
# Factors would be created in the logistic regression. Copy the dataset for running decision tree and 
# random forest
main_df <- df


############################################ Logistic Regression #######################################
# Convert the categorical variables to numerical. Factor the string
unique(df$gender)
df$gender <- factor(df$gender, labels = c(0, 1), 
                    levels = c("Male", "Female"))
unique(df$ever_married)
df$ever_married <- factor(df$ever_married, labels = c(0, 1), 
                          levels = c("Yes", "No"))
unique(df$work_type)
df$work_type <- factor(df$work_type, labels = c(0, 1, 2,3,4), 
                       levels = c("Private", "Govt_job","Self-employed","children","Never_worked"))
unique(df$Residence_type)
df$Residence_type <- factor(df$Residence_type, labels = c(0, 1), 
                            levels = c("Urban", "Rural"))
unique(df$smoking_status)
df$smoking_status <- factor(df$smoking_status, labels = c(0, 1, 2,3), 
                            levels = c("smokes", "formerly smoked","never smoked","Unknown"))
df$gender <- as.numeric(df$gender)
df$ever_married <- as.numeric(df$ever_married)
df$work_type <- as.numeric(df$work_type)
df$Residence_type <- as.numeric(df$Residence_type)
df$smoking_status <- as.numeric(df$smoking_status)

cor<-cor(df,method="pearson")
corrplot(cor,method="circle")
df[c(3, 9,10)] <- scale(df[c(3,9,10)])
sapply(df,class)
df <- subset(df,select = -(id))
set.seed(23)
numberOfRows <- nrow(df)
train.index <- sample(numberOfRows, numberOfRows*0.7)  
train.df <- df[train.index,]
valid.df <- df[-train.index,]

log_res <- glm(stroke ~., data = train.df, family = "binomial") 
options(scipen=999)
summary(log_res)
confusionMatrix(table(predict(log_res, newdata = valid.df, 
                              type="response") >= 0.35, valid.df$stroke == 1))
log_res_pred <- predict(log_res, valid.df, type = "response")
length(log_res_pred)
log_res_df<- data.frame(log_res_pred)
pred_valid<- cbind(valid.df,log_res_df)
gain <- gains(valid.df$stroke, log_res_pred, groups=10)
plot(c(0,gain$cume.pct.of.total*sum(valid.df$stroke))~c(0,gain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l",xlab="Cases",main="Gain Chart")
lines(c(0,sum(valid.df$stroke))~c(0, dim(valid.df)[1]), lty=2,xlab="Cases",main="Gain Chart")
pred_values<- data.frame(actual = valid.df$stroke, predicted = log_res_pred)
varImp(log_res, scale = FALSE)
################### Important Charts for logistic depending on var importance #########################
counts <- table(main_df$smoking_status,main_df$age)
par(mar = c(5,5,4,7))

barplot(counts, main="Smoking Status vs Stroke",
        xlab="Smoking", ylab="Stroke",col=c("darkblue","red"),legend.text = rownames(counts),xpd=T,
        args.legend = list(x = "right", bty='n', inset=c(-0.20,0), xpd = TRUE),beside=T,cex.names=0.70)

counts <- table(main_df$stroke,main_df$age)
par(mar = c(5,5,4,7))
barplot(counts, main="Stroke vs Age",
        xlab="Age", ylab="Stroke",col=c("darkblue","red"),legend.text = rownames(counts),xpd=T,
        args.legend = list(x = "right", bty='n', inset=c(-0.20,0), xpd = TRUE))

############################################ Decision Tree ############################################
set.seed(23)
dec_df <- main_df
dec_df <- subset(dec_df,select = -(id))
numberOfRows <- nrow(dec_df)
train.index <- sample(numberOfRows, numberOfRows*0.6)  
train.df <- dec_df[train.index,]
valid.df <- dec_df[-train.index,]
dt <- rpart(stroke ~ ., data = train.df, method = "class",cp=0.00001,maxdepth=5, minsplit =5)

prp(dt, type = 1, extra = 1, split.font = 1, varlen = -10)
printcp(dt)

ct.pred <- predict(dt, valid.df, type = "class")
confusionMatrix(ct.pred, as.factor(valid.df$stroke))
plotcp(dt)
#################################################### Random Forest #########################################
set.seed(1)
rf <- randomForest(as.factor(stroke) ~ ., data = train.df, 
                   ntree = 5000, mtry = 10, nodesize = 1, importance = TRUE, sampsize = 3000) 
varImpPlot(rf, type = 1)

#create a confusion matrix
valid.df$stroke <- factor(valid.df$stroke)
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$stroke)
