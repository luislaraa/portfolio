# A Kaplan-Meier plot is a non-parametric statistic, used to estimate the survival function from the lifetime data. What Are Nonparametric Statistics?
# Nonparametric statistics refers to a statistical method in which the data are not assumed to come from prescribed models that are determined by a small number of parameters; examples of such models include the normal distribution model and the linear regression model. Nonparametric statistics sometimes uses data that is ordinal, meaning it does not rely on numbers, but rather on a ranking or order of sorts. For example, a survey conveying consumer preferences ranging from like to dislike would be considered ordinal data.
rm(list = ls())
library("survival")
install.packages("survminer")
library("survminer")
library("dplyr")
attach(kmcurveexample)
library(readxl)
churn <- read_excel("Documents/churn.xlsx")
View(churn)
attach(churn)
summary(churn)
summary(tenure)
plot(tenure, TotalCharges)
plot(TotalCharges, MonthlyCharges)
# Firstly, we can put the following information into the curve. In order to plot the survival analysis on whether the employee is a partner or not, it is needed to make partner and churn binary. Thus we make the new variables.
partner_binary <- ifelse(churn$Partner=="No",0,1)
churn$partner_binary <- partner_binary
churn_binary <- ifelse(churn$Churn=="No",1,2)
churn$churn_binary <- churn_binary
kmcurvepartner <- survfit(Surv(tenure, churn_binary)~partner_binary, data = churn)
ggsurvplot(kmcurvepartner)

ggsurvplot(kmcurvepartner, xlim=c(0,75), break.x.by = 50, ylab="", xlab="", pval = TRUE, risk.table = TRUE, risk.table.title="", legend.labs=c("Non-Partner", "Partner"), legend.title = "", surv.scale = "percent", palette=c("blue", "red"), title="Example: Overall Survival", risk.table.height=.20)

#For example, let us plot a survival analysis for gender over tenure.
male_binary <- ifelse(churn$gender=="Female",0,1)
churn$male_binary <- male_binary
kmcurvegender <- survfit(Surv(tenure, churn_binary)~male_binary, data = churn)
ggsurvplot(kmcurvegender)
max(tenure)
ggsurvplot(kmcurvegender, xlim=c(0,75), break.x.by = 4, ylab="", xlab="", pval = TRUE, risk.table = TRUE, risk.table.title="", legend.labs=c("Female", "Male"), legend.title = "", surv.scale = "percent", palette=c("blue", "red"), title="Example: Overall Survival", risk.table.height=.20)

# here we can see the summary of survival, between 0 and 72, every 4 months
summary(kmcurvegender, times=seq(0,72,4))
# Since a comparison between genders were made, with the kaplan-meier curve we can see that this variable is not really redundant on the analysis of survival in the company.

# Now, we will check the survival for senior citizen in the company

kmcurvesenior <- survfit(Surv(tenure, churn_binary)~SeniorCitizen, data = churn)
ggsurvplot(kmcurvesenior)
ggsurvplot(kmcurvesenior, xlim=c(0,75), break.x.by = 4, ylab="", xlab="", pval = TRUE, risk.table = TRUE, risk.table.title="", legend.labs=c("Non-senior", "Senior"), legend.title = "", surv.scale = "percent", palette=c("blue", "red"), title="Example: Overall Survival", risk.table.height=.20)
summary(kmcurvesenior, time=50)
summary(kmcurvesenior)

# Now, for example, let´s do just a kaplan-meier curve between tenure and churn
kmcurveten <- survfit(Surv(tenure, churn_binary)~1, data = churn)

#
ggsurvplot(kmcurveten)
plot(kmcurveten ,mark.time = TRUE)
# This two plots show the same data.

# Then for example if we wanted to know the survival cases after a certain time period
summary(kmcurveten,time=50)


#Now for example if we wanted to check on top-half or low-half monthly charges, we would do this. 
mean(as.numeric(churn$MonthlyCharges))
lowMC_binary <- ifelse(churn$MonthlyCharges<64,"low","high")
churn$lowMC_binary=lowMC_binary

kmcurvelowmc <- survfit(Surv(tenure,churn_binary)~lowMC_binary,data=churn)
plot(kmcurvelowmc,mark.time = TRUE)
ggsurvplot(kmcurvelowmc)
summary(kmcurvelowmc, time = 50)

summary(kmcurveten)

#Important plot, kmcurve with the confidence interval.
plot(kmcurveten, conf.int = T, xlab="Time(in months)", ylab="%Survival", main="KM-Modl", mark.time = TRUE )


#To do the log-rank-test;
# Ho: survival in two groups is the same
# ha: survival in two groups is not
diferenciasMonthlycharges <- survdiff(Surv(tenure, churn_binary)~lowMC_binary)
diferenciasMonthlycharges
#Given that the p-value is very low, we can reject the null hypothesis and suppose that there are significant differences between the low and high monthly charges.