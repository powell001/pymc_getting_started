library(astsa)

png(file = "myplot.png")  # create PNG device
par(mfrow = c(3,1))
tsplot(cmort, ylab="Rate per 10,000", type="o", pch=19, col=6, nxm=2, main="Cardiovascular Mortality")
tsplot(tempr, ylab="\u00B0F", type="o", pch=19, col=4, nxm=2, main="Temperature")
tsplot(part, ylab="PPM", type="o", pch=19, col=2, nxm=2, main="Particulates")
dev.off() 


pairs(cbind(Mortality=cmort, Temperature=tempr, Particulates=part), col=4, lower.panel = astsa:::.panelcor)
temp = tempr - mean(tempr)  # center temperature
temp2 = temp^2
trend = time(cmort)
fit = lm(cmort~ trend + temp + temp2 + part, na.action=NULL)
summary(fit)  # regression results
summary(aov(fit))  # ANOVA table (compare to n<br/> Ext line)
summary(aov(lm(cmort~cbind(trend, temp, temp2, part)))) # Table 2.1
num = length(cmort)  # sample size
AIC(fit)/num - log(2*pi)  # AIC as in (2.15)
BIC(fit)/num - log(2*pi)  # BIC as in (2.17)
(AICc = log(sum(resid(fit)^2)/num) + (num+5)/(num-5-2)) # AICc
 # close PNG device


par(mfrow=2:1)
trend(chicken, lwd=2, results=TRUE) # graphic and results
trend(salmon, lwd=2)                # graphic only


summary(fit<-lm(chicken~time(chicken),na.action=NULL))
