### --- 07_Regression Project

data(mtcars)

#[, 1]    mpg	Miles/(US) gallon
#[, 2]	cyl	Number of cylinders
#[, 3]	disp	Displacement (cu.in.)
#[, 4]	hp	Gross horsepower
#[, 5]	drat	Rear axle ratio
#[, 6]	wt	Weight (lb/1000)
#[, 7]	qsec	1/4 mile time
#[, 8]	vs	V/S
#[, 9]	am	Transmission (0 = automatic, 1 = manual)
#[,10]	gear	Number of forward gears
#[,11]	carb	Number of carburetors
data <- mtcars
data1 <- data # create a copy to work on
str(data1)
# - Transform numeric am value to factor with discription
data1$am <- as.character(data1$am)
data1$am <- ifelse (data1$am == "0",
    sub("0", "Manual", data1$am, fixed=FALSE),
    sub("1", "Automatic", data1$am, fixed=FALSE) )
data1$am <- as.factor(data1$am)
# - Look at Data by am
boxplot(mpg ~ am, data = data1,
        xlab = "Type of Transmission", ylab = "MPG - Miles Per Gallon",
        main = "mtcars data", varwidth = TRUE, col = "lightgray")
# - Fit first model & view summary
fitTrans <- lm(mpg ~ am, data = mtcars)
plot(mtcars$am, mtcars$mpg, pch=19)
points(mtcars$am, mtcars$mpg, pch=19, col= mtcars$am)

plot(fitTrans)
summary(fitTrans)

# - Fit all variables and view summary
fitAll <- lm(mpg ~ . , data=data)
plot(fitAll)
summary(fitAll)$coef
plot(mtcars$am, mtcars$mpg, pch=19)
points(mtcars$am, mtcars$mpg, pch=19, col= mtcars$am)
abline(c(fitTrans$coeff[1], fitTrans$coeff[2]), col="Black", lwd=3)
abline(c(fit2slope$coeff[1] + fit2slope$coeff[3], 
         fit2slope$coeff[2] + fit2slope$coeff[4]), col="red", lwd=3)

fit16 <- lm(mpg ~ wt-1, data = data1)
summary(fit16)
plot(fit16)

pairs(data, panel = panel.smooth, main = "mtcars data",  col = 3)
# select 2nd regressor to add to am. View pairs plot, fit and view summary
    # used lowest p-value and this makes sense
pairs(data1[c(1,6,9)], panel = panel.smooth, main = "mtcars data",  col = 3)
fit169 <- lm( mpg ~ wt * am, data = mtcars)
summary(fit169)
plot(fit169)
# select 3rd regressor like above == qsec - but this has p-Value > am
pairs(data1[c(1, 6, 7, 9)], panel = panel.smooth, main = "mtcars data",  col = 3)
fit1679 <- lm( mpg ~ am + qsec + wt, data = mtcars)
fit1679m <- lm( mpg ~ am * qsec * wt, data = mtcars)
summary(fit1679)

# 2 line different slope
par(mfrow = c(2,2))
fit2slope <- lm(mpg ~ wt + am + am * wt, data=data1)
summary(fit2slope)
plot(data1$wt, data1$mpg, pch=19)
points(data1$wt, data1$mpg, pch=19, col= data1$am)
abline(c(fit2slope$coeff[1], fit2slope$coeff[2]), col="Black", lwd=3)
abline(c(fit2slope$coeff[1] + fit2slope$coeff[3], 
         fit2slope$coeff[2] + fit2slope$coeff[4]), col="red", lwd=3)

plot(resid(lm(wt ~ am, data=data)), resid(lm(mpg ~ am, data=data)), 
     frame = FALSE, col = "black", bg = "lightblue", pch = 21, cex = 2)
abline(lm(I(resid(lm(wt ~ am, data=data))), I(resid(lm(mpg ~ am, data=data)))), lwd = 2)

# nested model testing
fitTrans <- lm(mpg ~ am -1, data = data)
fitAll <- lm(mpg ~ . -1 , data = data)
fit169 <- lm(mpg ~ wt * am - 1, data = data)
fit1679 <- lm(mpg ~ wt * qsec *am -1, data = data)
fit179 <- lm(mpg ~ qsec *am, data = data1)
anova(fitTrans, fitAll, fit169, fit1679, fit179)

plot(data1$qsec, data1$mpg, pch=19)
points(data1$qsec, data1$mpg, pch=19, col= data1$am)
abline(c(fit179$coeff[1], fit179$coeff[2]), col="Black", lwd=3)
abline(c(fit179$coeff[1] + fit179$coeff[3], 
         fit179$coeff[2] + fit179$coeff[4]), col="red", lwd=3)


plot(predict(fit179), resid(fit179), pch = 19)
plot(fit179)
library(car)
sqrt(vif(fitAll))
