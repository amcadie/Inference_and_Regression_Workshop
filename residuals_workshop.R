library(ggplot2)

set.seed(50)
x1 <- rnorm(20, 5, 1)
set.seed(30)
x2 <- x1^2 + rnorm(20)*0.1
y <- 0.8 * x1 + 0.2 * x2

df <- data.frame(pred1 = x1, pred2 = x2, response = y)

#plot y vs x1
g <- ggplot(aes(x = pred1, y = response), data = df) + geom_point(alpha = I(1/2), 
                                                                  size = 7)
g <- g + geom_point(color = "firebrick", alpha = I(1/2), size = 6)
g <- g + geom_smooth(method = "lm")
g

#generate linear model
fit <- lm(response ~ pred1, data = df)
summary(fit) #R-squared = 0.992

#create new data frame for residual vs predicted plot
df_fit <- data.frame(e = fit$resid, yhat = predict(fit))

g2 <- ggplot(aes(x = yhat, y = e), data = df_fit)
g2 <- g2 + geom_point(alpha = I(1/2), size = 7)
g2 <- g2 + geom_point(color = "firebrick", alpha = I(1/2), size = 6)
g2


