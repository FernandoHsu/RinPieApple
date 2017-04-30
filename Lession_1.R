### Author : Wei, Hsu
### date : 04,30,2017
############################################################
### Resource:
### http://stat545.com/topics.html
### https://www.kaggle.com/c/titanic/data
############################################################

###3 DataStructre in R #### 
###df, vetor, factor
x <- sin(2*pi)
x;print(x)
class(x)

y <- rep(x,10)+rnorm(10)
plot(y,type = "l")

iris
class(iris)
summary(iris)
str(iris)

### Titanic: Who would be saved ? ####
### GetData: https://www.kaggle.com/c/titanic/data
### DataDescription: https://www.kaggle.com/c/titani
setwd("C:\\Users\\USER\\Google 雲端硬碟\\大三\\R\\Kaggle")
train <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv")
test <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv")
str(train)
head(train)


### DataVidualization_OneVar. : Numeric ####
#desc
summary(train$Age)
table(train$Age)
#summary is a O.O.P function !
summary(train$Survived)
summary(as.factor(train$Survived))#table(train$Survived)

EDA <- function(x){
  par(mfrow=c(2,2))
  hist(x,prob=T)
  plot(x)
  boxplot(x,horizontal = T)
  qqnorm(x);qqline(x)
  par(mfrow=c(1,1))
}
EDA(train$Age)
EDA(log2(train$Age))#meaningful or not ?

EDB <- function(x){
  par(mfrow=c(2,2))
  barplot(x,prob=T)
  plot(x)
  boxplot(x,horizontal = T)
  qqnorm(x);qqline(x)
  par(mfrow=c(1,1))
}

###Your turn:Fare, Survied, Ticket
EDA(train$Fare)#outlier
EDA(train$Survived)#logical var.
EDA(train$Ticket)#factor

### DataVidualization_OneVar. : Categorical ####
### Factor in R ####
### Ref: handing strings in R
### http://stat545.com/block022_regular-expression.html

#which var. is a factor ? Name,Sex,Ticket,Cabin,Embarked
str(train)
#which do you want to look? Embarked & Cabin
#Let's see the distr. of vector
table(train$Embarked)
barplot(table(train$Embarked))
table(train$Cabin)
#Could we direct transform?
c(687+length(levels(train$Cabin))-1,length(train$Cabin))
#Let's transform the data to "A,B,C...."
vec <- as.character(train$Cabin)
table(vec)

#Transform strings into A
#gsub("A.","A",vec)

grep('A',vec,value = T)
vec[grepl('A',vec)] <-'A'
#Use loops to do that
for(i in LETTERS[1:7])  vec[grepl(i,vec)] <- i
table(vec)
#Nmae empty vector 'N' 
vec[nchar(vec)==0] <- "N"
table(vec)
barplot(table(vec))
train$Cabin_new <- vec


### DataVidualization_-Two-Var. ####
# What's my question: Who will be saved in Titanic ?
# Response var. ?  Survied ! (Factor)
# Explanatory var. ?  
# (Factor)Sex,Pclass ;(Numeric) Age,Fare 


# what's different in the following prob.?
#   P(Survivied|Sex) != P(Sex|Survied)
#   (hint: what's your response variable)
# Since Survived & Unsaved are disjoint, the following conditon holds:
#   P(Survied=1|male)+P(Survived=0|male)=1
#   P(Survied=1|female)+P(Survived=0|female)=1


### Factor-Factor ####
attach(train)
table(Survived,Sex)
#table(row,col,conditon_on)
#P(A|B):table(A,given_B,conditon_on_B)

#P(Sex|Survied)
#P(male|Survied=0)+P(female|Survivied=0)=1
prop.table(table(Survived,Sex),margin = 1) #conditon_on:rows
mosaicplot(table(Survived,Sex),main="Sex by Survied",shade = T)
barplot(table(Sex,Survived),main = "Sex by Survived")
#P(Survived|Sex)
#P(Survivied=1|female)+P(Survived=0|female)=1
prop.table(table(Survived,Sex),margin = 2) #conditon_on:col
mosaicplot(Sex~Survived,main="Survived by Sex",shade = T)
barplot(table(Survived,Sex),main = "Survived by Sex")
#what's your observation?
#Conditon on female is more likely to be saved

### Your term 
### find P(Survived|Sex) in the following way?
#prop.table(table(Sex,Survived),margin = 1)

### How about affect of social class?
#P(Survied|Pclass)
mosaicplot(Survived~Pclass,main="Survived by SocialClass")
prop.table(table(Survived,Pclass),margin = 2)#condition_on:col
#what did you observe?
#High class're more likely to be saved.

### How about conditonal on both Sex & Social class?
#P(Survied|Sex & Pclass)
mosaicplot(Survived~Sex+Pclass,main = "Survived by Sex & Pclass")
#table(row,col,oth,margin=row):condition_on_ row & oth
#oth is automacally contionally.
prop.table(table(Sex,Survived,Pclass),margin=1)

### your tern 
#table(a,b,c,margin = 2):conditon_on ( b & C )
#c is automacally contionally.
#find P(Survived|Sex & Pclass) in the above way?
#prop.table(table(Survived,Sex,Pclass),margin = 2)

#what's your conclusion?
#P(Survied=1|female & Pclass=1)= 0.289
#P(Survied=1|female & Pclass=2)= 0.222
#P(Survied=1|female & Pclass=3)= 0.229

#If Sex is different, saving female?
#If Pclass is different, saving gentalmen&lady?

### Factor-Numeric ####
#boxplot(y~x)
boxplot(Age~Survived)
boxplot(Age~Survived)

barplot(table(Survived,Age))
### Numeric-Numeric ####
#scatter plot
plot(Age,Fare)
#set the ope.
plot(Fare~Age,ylim=c(0,300),main="Fare~Age",ylab="Fare")
#sometimes log() form could be useful
plot(log2(Fare)~Age)
#regression line(remember to conduct together!!!)
plot(Age,Fare);abline(lm(Fare~Age))
detach(train)  


### Model with LPM Models ####
### Using training data to fit a model
attach(train)
fit.1 <- lm(Survived~Age+Fare+Sex+Pclass,data=train)
summary(fit.1)
pre <- predict(fit.1,train)
pre <- ifelse(pre>.5,1,0)
confuse.matrix <- table(Answer=Survived,Pred=pre)
accuracy <- sum(diag(confuse.matrix))/sum(confuse.matrix)
accuracy
detach(train)

### Using testing dataset to predict
pre.test <- predict(fit.1,test)
pre.test <- ifelse(pre.test>.5,1,0)
pre.test[is.na(pre.test)] <- 0
pre.final <- cbind(PassengerID=test$PassengerId,Survived=pre.test)

### Goto test your prediction:https://www.kaggle.com/c/titanic#
write.csv(pre.final,"pre.final.scv",col.names = TRUE)
