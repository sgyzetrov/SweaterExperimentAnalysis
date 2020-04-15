# ref: https://stackoverflow.com/questions/17154844/r-anova-data-25-design-with-replicates
rm(list = ls())
Length_Block2<-c(8.2, 8.4, 8.3, 8.4,8.1,8.2,8.2,8.2,8.4,8.3,8.3,8,8,8.3,8.1,8.6,8.5,8.3,8.5,8.3,8.3,8,8.3,8.5)
Width_Block2<-c(6.3,6,6.2,6.4,6.5,6.5,6.3,6.4,6.4,6.2,6.5,6.3,6.5,6.6,7,6.4,6.2,6.5,6.5,6.5,6.5,6.5,6.6,6.5)
response_Block2<-Length_Block2*Width_Block2

FactorA=c(rep(1,12),rep(-1,12))
FactorB=rep(c(rep(1,3),rep(-1,3)),4)
FactorC=rep(c(rep(1,6),rep(-1,6)),2)

response_data <- data.frame(response_Block2, FactorA, FactorB, FactorC)

res.lm<-lm(response_Block2~FactorA*FactorB*FactorC, data=response_data)
summary(res.lm)
# 
# library(daewr)
# # fullnormal(coef(res.lm)[-1], alpha = 0.025, refline = "TRUE")
# LGB( coef(res.lm)[-1], rpt = T) #Half normal plots

res.aov<-aov(response_Block2~FactorA*FactorB*FactorC,data=response_data)
summary(res.aov)
effects_values=res.aov$effects/2
# res.aov_reduced<-aov(response_Block2~FactorA*FactorB*FactorC-FactorA:FactorB-FactorA:FactorC-FactorB:FactorC-FactorA:FactorB:FactorC, data = response_data)
# summary(res.aov_reduced)
# 
y_residuals=res.aov$residuals
#Normality
qqnorm(y_residuals, ylim=c(min(y_residuals)-1,max(y_residuals)+1), main = "Normal Q-Q Plot for Residuals",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified", plot.it = TRUE, datax = FALSE)
qqline(y_residuals, datax = FALSE, distribution = qnorm)
#Test normality using Shapiro Wilks
shapiro.test(y_residuals)
#Check Variance
Fitted_values=res.aov$fitted.values
plot(Fitted_values,y_residuals,ylab="Residuals",xlab="Fitted Values")
abline(h=0)
# 
# y_residuals=res.aov_reduced$residuals
# #Normality
# qqnorm(y_residuals, ylim=c(min(y_residuals)-1,max(y_residuals)+1), main = "Normal Q-Q Plot for Residuals",
#        xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified", plot.it = TRUE, datax = FALSE)
# qqline(y_residuals, datax = FALSE, distribution = qnorm)
# #Test normality using Shapiro Wilks
# shapiro.test(y_residuals)
# #Check Variance
# Fitted_values=res.aov_reduced$fitted.values
# plot(Fitted_values,y_residuals,ylab="Residuals",xlab="Fitted Values")
# abline(h=0)