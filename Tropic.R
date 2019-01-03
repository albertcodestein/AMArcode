library(dplyr)
Tropic <- read.csv("tropic.csv", header = TRUE)

#prices ending in 9

Tropic$end9 <- ifelse((Tropic$Price * 100) %% 10 == 9, 1, 0)

#dividing the weeks into quarters

Tropic$Quarter <- floor((Tropic$Week - 1) %% 52 / 13) + 1

#library(car)

#semi-log regression model

Tropic$Quarter <- relevel(as.factor(Tropic$Quarter), 4)
Tropic$Store <- relevel(as.factor(Tropic$Store), 15)

RegCoeff <- lm(log(Quant) ~ Price + as.factor(Quarter) + as.factor(Deal) + as.factor(Store) 
               + Week + as.factor(end9), data = Tropic)

summary(RegCoeff)