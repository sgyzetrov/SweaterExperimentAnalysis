# ref: https://stackoverflow.com/questions/17154844/r-anova-data-25-design-with-replicates
rm(list = ls())
Length_Block1<-c(8.6, 8.4, 8.7, 8,8.2,8.1,8.5,8.4,8.7,8.1,8.5,8,8,8,8.2,7.6,8.2,8.1,8,8.5,8.6,7.6,8.1,8.2)
Width_Block1<-c(6.5,6.3,6.7,6.3,6,6.2,6.4,6.6,6.7,6.5,6.6,6.5,6.3,6.3,6.5,6.3,6.3,6.3,6.4,6.6,6.2,6.4,6.6,6.4)
response_Block1<-Length_Block1*Width_Block1

Length_Block2<-c(8.2, 8.4, 8.3, 8.4,8.1,8.2,8.2,8.2,8.4,8.3,8.3,8,8,8.3,8.1,8.6,8.5,8.3,8.5,8.3,8.3,8,8.3,8.5)
Width_Block2<-c(6.3,6,6.2,6.4,6.5,6.5,6.3,6.4,6.4,6.2,6.5,6.3,6.5,6.6,7,6.4,6.2,6.5,6.5,6.5,6.5,6.5,6.6,6.5)
response_Block2<-Length_Block2*Width_Block2

response <- c(response_Block1, response_Block2)

FactorA=rep(c(rep(1,12),rep(-1,12)), 2)
FactorB=rep(rep(c(rep(1,3),rep(-1,3)),4), 2)
FactorC=rep(rep(c(rep(1,6),rep(-1,6)),2), 2)

response_data <- data.frame(response, FactorA, FactorB, FactorC)

response_data$Block <- as.factor(c(rep(1,24),rep(-1,24))) # as.factor important?
# response_data$Block <- c(rep(1,24),rep(-1,24)) # as.factor important?
# response_data$Block <- c(rep(1,24),rep(-1,24))

# res.aov_Block1<-aov(response~factor(FactorA)*factor(FactorB)*factor(FactorC)+Block,data=response_data)
# summary(res.aov_Block1)

res.lm<-lm(response~FactorA*FactorB*FactorC+Block, data=response_data)# ? -A:B:C necessary?
summary(res.lm)

res.lm1<-lm(response~FactorA*FactorB*FactorC*Block, data=response_data)# ? -A:B:C necessary?
summary(res.lm1)

# library(daewr)
# # fullnormal(coef(res.lm)[-1], alpha = 0.025, refline = "TRUE")
# LGB( coef(res.lm)[-1], rpt = T) #Half normal plots

res.aov1<-aov(response~FactorA*FactorB*FactorC+Block,data=response_data)# ? -A:B:C necessary?
summary(res.aov1)
# effects_values=res.aov$effects/2
res.aov<-aov(response~FactorA*FactorB*FactorC*Block,data=response_data)# ? -A:B:C necessary?
summary(res.aov)

y_residuals=res.aov$residuals
Fitted_values=res.aov$fitted.values
plot(Fitted_values,y_residuals,ylab="Residuals",xlab="Fitted Values")
abline(h=0)
#Normality
qqnorm(y_residuals, ylim=c(min(y_residuals)-1,max(y_residuals)+1), main = "Normal Q-Q Plot for Residuals",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified", plot.it = TRUE, datax = FALSE)
qqline(y_residuals, datax = FALSE, distribution = qnorm)
#Test normality using Shapiro Wilks
shapiro.test(y_residuals)


res.aov_reduced<-aov(response~FactorA*FactorB*FactorC*Block-FactorB:FactorC, data = response_data)
summary(res.aov_reduced)

y_residuals=res.aov_reduced$residuals
#Normality
qqnorm(y_residuals, ylim=c(min(y_residuals)-1,max(y_residuals)+1), main = "Normal Q-Q Plot for Residuals",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles- Modified", plot.it = TRUE, datax = FALSE)
qqline(y_residuals, datax = FALSE, distribution = qnorm)
#Test normality using Shapiro Wilks
shapiro.test(y_residuals)
#Check Variance
Fitted_values=res.aov_reduced$fitted.values
plot(Fitted_values,y_residuals,ylab="Residuals",xlab="Fitted Values")
abline(h=0)

index=which(Fitted_values == max(Fitted_values))
FactorA[index]
FactorB[index]
FactorC[index]
response_data$Block[index]
# A1 B1 C-1 Block-1 max