#load visualization package
library("ggplot2")

#load dataset
sales_data <- read.csv("spend-sales.csv")

##graphical analysis
#scatter plot
scatter.smooth(x=sales_data$Spend, y=sales_data$Sales)

#boxplot
par(mfrow=c(1, 2))
boxplot(sales_data$Spend, main="Spend", sub=paste("Outlier rows: ", boxplot.stats(sales_data$Spend)$out))
boxplot(sales_data$Sales, main="Sales", sub=paste("Outlier rows: ", boxplot.stats(sales_data$Sales)$out))

##build simple linear regression from scratch
#convert dataframe into X and Y matrices
X = as.matrix(sales_data$Spend)
Y = as.matrix(sales_data$Sales)

#calculate coefficient
beta1 <- function(x, y) {
  result <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))**2)
  return(result)
}

#calculate intercept
beta0 <- function(x, y) {
  b1 <-  beta1(x, y)
  result <- mean(y) - b1 * mean(x)
  return(result)
}

#predict using scratch model
pred_scratch <- function(x, y) {
  b1 <- beta1(x, y)
  b0 <- beta0(x, y)
  pred <- b0 + b1*x
  return(pred)
}

#plot model
y_hat <- pred_scratch(X, Y)
plot1 <- ggplot(sales_data) +
  geom_point(aes(x=Spend, y=Sales), color="#00AFBB", size=3, alpha=0.75) +
  geom_line(aes(x=Spend, y=y_hat), color="#00AFBB", size=1, alpha=0.4) +
  ggtitle("Simple Linear Regression (Scratch)") +
  theme(plot.title = element_text(hjust=0.5))
plot1

#check r2 and rmse scores
r2 <- function(actual, predicted) {
  SSE <- sum((predicted - actual)**2)
  SST <- sum((mean(actual) - actual)**2)
  return(1 - SSE/SST)
}
rmse <- function(actual, predicted) {
  MSE <- sum((predicted - actual)**2) / length(actual)
  return(sqrt(MSE))
}
r2_score <- r2(sales_data$Sales, y_hat)
r2_score
rmse_score <- rmse(sales_data$Sales, y_hat)
rmse_score

##linear regression model using lm function
linreg <- lm(Sales ~ Spend, data=sales_data)
summary(linreg)