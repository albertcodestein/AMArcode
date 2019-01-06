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

Tropic2 <- Tropic %>% group_by(Store) %>% mutate(LagQuant = lag(Quant)) %>% 
           select(-"LagQrt")

RegCoeff2 <- lm(log(Quant) ~ Price + as.factor(Quarter) + as.factor(Deal) + as.factor(Store) 
               + as.factor(end9) + Week + LagQuant, data = Tropic2)

summary(RegCoeff2)



