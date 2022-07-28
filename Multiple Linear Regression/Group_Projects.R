X4_Stroke

X4_Stroke$Smoker = factor(X4_Stroke$Smoker,c("No","Yes"),labels = c(1,0))
X4_Stroke$Smoker

X4_Stroke

attach(X4_Stroke)

fit = lm(Risk~Age+Pressure+Smoker)
fit

std_res = scale(fit$residuals)

plot(fit$fitted.values, std_res, main = "Std_residuals Vs Fitted values",xlab = "Fitted Vales", ylab = "Std_residuals")

plot(fit$fitted.values,std_res)

anova(fit)

summary(fit)

library(MASS)
stepAIC(fit,direction = "both")



std_residuals = round(scale(fit$residuals),2)
std_residuals
outliers = c((std_residuals)<2 & (std_residuals)>-2)
outliers

Cook_statistics = round(4/(20-3),2)
Cook_statistics
Cook_distance = round(cooks.distance(fit),2)
Cook_distance
influential = c((cooks.distance(fit))<Cook_statistics)
influential

plot(Cook_distance,std_residuals)

library(car)
vif(fit)
