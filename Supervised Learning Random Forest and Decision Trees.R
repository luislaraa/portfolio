rm(list = ls())
library(readr)
library(ranger)
exams <- read_csv("Documents/exams.csv")
View(exams)
attach(exams)

#EDA
hist(`math score`)
max(`math score`)
hist(`reading score`)
max(`reading score`)
hist(`writing score`)
max(`writing score`)

if(!require('corrplot')) install.packages('corrplot');library('corrplot')
male <- ifelse(exams$gender=="male", 1,0)
exams$male <- male 
exams$gender <- NULL
prep <- ifelse(exams$`test preparation course`=="completed",1,0)
exams$prep <- prep
exams$`test preparation course`=NULL
exams$ethnicity_a <- ifelse(exams$`race/ethnicity`=="group A",1,0)
exams$ethnicity_b <- ifelse(exams$`race/ethnicity`=="group B",1,0)
exams$ethnicity_c <- ifelse(exams$`race/ethnicity`=="group C",1,0)
exams$ethnicity_d <- ifelse(exams$`race/ethnicity`=="group D",1,0)
exams$ethnicity_e <- ifelse(exams$`race/ethnicity`=="group E",1,0)
exams$`race/ethnicity` <- NULL
unique(exams$`parental level of education`)
exams$hs <- ifelse(exams$`parental level of education`=="high school",1,0)
exams$shs <- ifelse(exams$`parental level of education`=="some high school",1,0)
exams$sc <- ifelse(exams$`parental level of education`=="some college",1,0)
exams$ad <- ifelse(exams$`parental level of education`=="associate's degree",1,0)
exams$bd <- ifelse(exams$`parental level of education`=="bachelor's degree",1,0)
exams$md <- ifelse(exams$`parental level of education`=="master's degree",1,0)
exams$`parental level of education` <- NULL
unique(exams$lunch)
exams$paidlunch <- ifelse(exams$lunch=="standard",1,0)
exams$paidlunch <- ifelse(exams$lunch=="free/reduced",1,0)
exams$lunch <- NULL

M <- cor(exams)
corrplot::corrplot(M)
#After performing the correlation matrix, we can see that there are interesting relationships, such as the paid lunch negative relationship with the test scores. 

sp <- sample(nrow(exams), 0.7*nrow(exams))
tr <- exams[sp,]
te <- exams[-sp,]

#First we make a model for the qualification of the math exams. We quickly notice that both the parental level of "bd" and "md" do not have much relevance.
m1 <- lm(`math score`~. -`reading score` -`writing score`,tr)
summary(m1)
mathlm <- lm(`math score`~. -`reading score`-`writing score`-bd-md,tr)
summary(mathlm)
readinglm <- m2 <- lm(`reading score`~. -`math score`-`writing score`-bd-ad-md,tr)
summary(readinglm)
writinglm <- lm(`writing score`~. -`reading score`-`math score`-bd-ethnicity_d-ethnicity_e-ad,tr)
summary(writinglm)
#Here we have three linear models corresponding to our objective variables.
library(ggplot2)
ggplot(tr, aes(x=`math score`, y=`reading score`, col=ethnicity_a)) +
  geom_point() + geom_smooth(method='lm')
#Here we can see the math score and reading score plotting, where also the ethnicity a is filled. We can see that in average more dark dots appear on the top results as the test grades increase.

Mregr <- function(pred, real){
  RMSE = sqrt(mean((pred - real)^2))
  R2 = 1-sum((pred - real)^2)/sum((real - mean(real))^2)
  MAE = mean(abs(pred - real))
  reg <- data.frame(RMSE,R2,MAE)
  print(reg, row.names=FALSE)
}
Mregr(predict(mathlm, te), te$`math score`)
Mregr(predict(readinglm, te), te$`reading score`)
Mregr(predict(writinglm, te), te$`writing score`)

# With the data that we have, it can be seen that the information appears to not be enough to explain the data accurately.

# Ahora intentaremos utilizar los decision trees
if(!require('partykit')) install.packages('partykit');library('partykit')

mpk1 = ctree(`math score` ~ . -`reading score` -`writing score` , data=tr)
plot(mpk1)
mpk1
ppk1 = predict(mpk1, te)
table(predicho=ppk1, real=te$`math score`)
head(data.frame(PRED=ppk1, REAL=te$`math score`), 15)
mpk2 = ctree(`reading score` ~ . -`math score` -`writing score` , data=tr)
plot(mpk2)
mpk2
ppk2 = predict(mpk2, te)
table(predicho=ppk2, real=te$`math score`)
head(data.frame(PRED=ppk2, REAL=te$`reading score`), 15)
mpk3 = ctree(`writing score` ~ . -`math score` -`reading score` , data=tr)
plot(mpk3)
mpk3
ppk3 = predict(mpk3, te)
table(predicho=ppk3, real=te$`math score`)
head(data.frame(PRED=ppk3, REAL=te$`writing score`), 15)


# Now we utilize the rpart package.
if(!require('rpart')) install.packages('rpart');library('rpart')
if(!require('rpart.plot')) install.packages('rpart.plot');library('rpart.plot')

mrp1 <- rpart(`math score` ~ . -`reading score` - `writing score`, data=tr)
rpart.plot(mrp1)
mrp1


# Predicting the model
prp <- predict(mrp1, te)
table(predicho=prp, real=te$`math score`)

#Now, we will try random forests.
m = ranger(`math score` ~ . -`reading score` - `writing score`, exams, importance = "impurity")
p1 = predict(m, te)
table(p1$predictions, te$`math score`)
m$variable.importance
dfn <- as.data.frame(m$variable.importance)
dfn$w <- row.names(dfn)

names(dfn) <- c("w","v")

library(ggplot2)

ggplot(dfn, aes(x=reorder(v,w), y=w,fill=w))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Information Value Summary")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
m$confusion.matrix
as.data.frame(m$num.trees)
#As we can see with this model, paid lunch, ethnicity_e, and male are the most important variables for the math grades. This model ended up having 500 trees.
