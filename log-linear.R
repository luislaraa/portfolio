rm(list=ls())

tabcon <- array(data = c(19,24,29,43, 29,42,27,30, 57,37,49,52, 63,68,53,42), 
                dim = c(2,2,2,2), 
                dimnames = list("Water Softness" = c("Soft","Hard"),
                                "Brand preference" = c("X","M"),
                                "User of M" = c("Si","No"), "Temperature" = c("High","Low"))) 
ftable(tabcon, row.vars = c("Water Softness","Brand preference", "User of M"))


addmargins(tabcon)

options(digits=2)
prop.table(tabcon, margin = c(1,3))
prop.table(tabcon, margin = c(1,2))
prop.table(tabcon, margin = c(1))

tabcon.df <- as.data.frame(as.table(tabcon))
tabcon.df[,-5] <- lapply(tabcon.df[,-5], relevel, ref = "No")
tabcon.df

# No interactions

mod1 <- glm(Freq ~ Water.Softness + Brand.preference + User.of.M + Temperature, data = tabcon.df, family = poisson)
summary(mod1)
pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = F)
cbind(mod1$data, fitted(mod1))

exp(coef(mod1)[3])

# 1st order interactions

mod2 <- glm(Freq ~ (Water.Softness + Brand.preference + User.of.M + Temperature)^2, 
            data = tabcon.df, family = poisson)
summary(mod2)
pchisq(deviance(mod2), df = df.residual(mod2), lower.tail = F)
#value of .27
cbind(mod2$data, fitted(mod2))

exp(coef(mod2)["Water.SoftnessHard:User.ofMNo"])

mod2$coefficients
exp(mod2$coefficients)

exp(confint(mod2, parm = c("TemperatureLow:Brand.preferenceM","Brand.preferenceM:User.of.MNo","Water.SoftnessHard:User.of.MNo")))

# Saturated model

mod3 <- glm(Freq ~ Water.Softness*Brand.preference*User.of.M*Temperature, 
            data = tabcon.df, family = poisson)
summary(mod3)
#no degrees of freedom, thus we cannot prove its functioning.
cbind(mod3$data, fitted(mod3))

mod4 <- glm(Freq ~ Water.Softness + Brand.preference + User.of.M + Temperature + (Water.Softness * Brand.preference) + (Brand.preference * User.of.M) + (Brand.preference * Temperature) + (User.of.M * Temperature) + (User.of.M * Brand.preference * Temperature), data = tabcon.df, family = poisson) 
summary(mod4)
pchisq(deviance(mod4), df = df.residual(mod4), lower.tail = F)
#value of  0.076
mod5 <- glm(Freq~ Brand.preference + User.of.M + Temperature + (Brand.preference * User.of.M) + (Brand.preference * Temperature), data = tabcon.df, family = poisson)
summary(mod5)
pchisq(deviance(mod5), df = df.residual(mod5), lower.tail = F)
#p-value of .10
mod6 <- glm(Freq~ Brand.preference + User.of.M + Temperature + (Brand.preference * User.of.M), data = tabcon.df, family = poisson)
summary(mod6)
pchisq(deviance(mod6), df = df.residual(mod6), lower.tail = F)
#pchisq of .16, which means model has not been improved.

# Model Comparison
anova(mod5, mod2)
pchisq(6.43, df = 5, lower.tail = F)
#value of .27, thus model 5 explains better than model 2
anova(mod1, mod2)
pchisq(19.8, df = 6, lower.tail = F)

anova(mod5, mod4)
pchisq(1.37, df = 4, lower.tail = F)
