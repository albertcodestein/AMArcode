library(dplyr)
Tropic <- read.csv("tropic.csv", header = TRUE)

#prices ending in 9

Tropic$end9 <- ifelse((Tropic$Price * 100) %% 10 == 9, 1, 0)

#dividing the weeks into quarters

Tropic$Quarter <- floor((Tropic$Week - 1) %% 52 / 13) + 1

#semi-log regression model

Tropic$Quarter <- relevel(as.factor(Tropic$Quarter), 4) #setting quarter 4 as the reference level
Tropic$Store <- relevel(as.factor(Tropic$Store), 15) #setting store 15 as the reference level

RegCoeff <- lm(log(Quant) ~ Price + as.factor(Quarter) + as.factor(Deal) + as.factor(Store) 
                + as.factor(end9) + Week, data = Tropic)

summary(RegCoeff)

#creating lag sales variable

Tropic2 <- Tropic %>% group_by(Store) %>% mutate(LagQuant = lag(Quant,2))

Tropic3 <- Tropic2 %>% group_by(Store) %>% mutate(LQ1 = lag(Quant))


RegCoeff2 <- lm(log(Quant) ~ Price + as.factor(Quarter) + as.factor(Deal) + as.factor(Store) 
               + as.factor(end9) + Week + log(LagQuant) + log(LQ1), data = Tropic3)

summary(RegCoeff2)

#diagnostics
library(car)
outlierTest(RegCoeff2)


    #adjust the model by removing the 2 outlier data points based on bonferroni p values
    y <- names(outlierTest(RegCoeff2)$bonf.p)
    Tropic4 <- Tropic3[!(rownames(Tropic3) %in% y),]
    
    
    #new adjusted model
    fit <- lm(log(Quant) ~ Price + as.factor(Quarter) + as.factor(Deal) + as.factor(Store) 
                    + as.factor(end9) + Week + log(LagQuant) + log(LQ1), data = Tropic4)
    
    summary(fit)
    
    
outlierTest(RegCoeff3)
qqPlot(RegCoeff3, main = "QQplot") #qq plot for studentized resid 
leveragePlots(RegCoeff3) # leverage plots

# Influential Observations
# added variable plots 
av.Plots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2)) 
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot 
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
    

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit)


# Evaluate Collinearity
vif(fit) # variance inflation factors 
sqrt(vif(fit)) > 2 # problem?


# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit) 
summary(gvmodel)