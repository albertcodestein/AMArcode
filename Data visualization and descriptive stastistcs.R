library(ggplot2)
library(corrplot)

#Histogram of sales

ggplot(data = Tropic, aes(Tropic$Quant)) + 
    geom_histogram(col = "black", fill = "white") + 
    labs("Histogram of Sales") + 
    labs(x = "Sales", y = "Frequency")

#Histogram of LogSales

ggplot(data = Tropic, aes(log(Tropic$Quant))) + 
    geom_histogram(col = "black", fill = "white") + 
    labs("Histogram of LogSales") + 
    labs(x = "LogSales", y = "Frequency")

#correlation between Sales and price

cormat <- cbind(Tropic$Quant,log(Tropic$Quant),Tropic$Price,log(Tropic$Price))
colnames(cormat) <- c("Sales", "LogSales", "Price", "LogPrice")

cormat <- cor(cormat) #correlation matrix

corrplot(cormat, method = "number", type = "lower", tl.col = "black", tl.srt = 45)

#plot of seasonality Sales vs Week

AvgSales <- Tropic %>% group_by(Week) %>% summarise(Quant = mean(Quant))

ggplot(data = AvgSales, aes(x = AvgSales$Week, y =  AvgSales$Quant)) +
    geom_line(color = "blue", size = 0.5) +
    labs("Plot of Average Weekly Sales") +
    labs(x = "Weeks", y = "Average Weekly Sales")