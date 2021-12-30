#Clear Directory 
rm(list = ls())

###########################################################################
###############################
#Part 2
###############################
###########################################################################

# set your working directory
setwd(dir = "~/desktop/Fall2020/Stats525/IE Project")
# and read your data files using the "read.table" command
NBA_data = read.table(file = "NBA_data.txt")
names(NBA_data)<-c("Salary", "TwoP", "TwoPA", "TwoPP", "eFGP", "FT", "FTA", "FTP")
attach(NBA_data)

#Assigning all the variables
Y <- NBA_data$Salary
X1 <- NBA_data$TwoP
X2 <- NBA_data$TwoPA
X3 <- NBA_data$TwoPP
X4 <- NBA_data$eFGP
X5 <- NBA_data$FT
X6 <- NBA_data$FTA
X7 <- NBA_data$FTP

#Average Y
Y.bar <- mean(Y)

n <- length(Y)

#----------------------------------------------------
#    1    Average Two Point Shot Made per Game (TwoP)
#----------------------------------------------------

mod_1 <- lm(Y~X1)

mod_output_1 = summary(mod_1)
names(mod_output_1)
mod_output_1$coefficients
b0_1 <- mod_output_1$coefficients[1,1]
b1_1 <- mod_output_1$coefficients[2,1]
s_1 <- mod_output_1$sigma
e_1 <- mod_output_1$residuals 
#Estimator for Y
Y.hat_1 <- mod_1$fitted.values

R2_1 <- sum((Y.hat_1 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_1

#Scatterplot TwoP on Salary

plot(X1, Y, pch = 16,
     xlab = "Average Two Points Made Per Game", ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 10),
     main = "Average Two Points Made Per Game vs Salary")
abline(b0_1, b1_1, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVTwoP_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X1, 
     freq = FALSE,
     xlab = "Avergae Two Points Made Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Avergae Two Points Made Per Game")),
     ylim = c(0, 0.5))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVTwoP_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X1
plot(X1, e_1, pch = 16,
     xlab = "TwoP", 
     ylab = expression(paste("Residuals ", sep = "")),
     ylim = c(-20000000,30000000), 
     xlim = c(0,10),
     main = expression(paste("Residual Plot Against Two Points Made", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVTwoP_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#-----------------------------------------------------------------
#    2    Average Two Point Shots Attempted Per Game  (TwoPA)
#-----------------------------------------------------------------

mod_2 = lm(Y~X2)

mod_output_2 = summary(mod_2)
names(mod_output_2)
mod_output_2$coefficients
b0_2 = mod_output_2$coefficients[1,1]
b1_2 = mod_output_2$coefficients[2,1]
s_2 = mod_output_2$sigma
e_2 <- mod_output_2$residuals 
#Estimator for Y
Y.hat_2 = mod_2$fitted.values

R2_2 = sum((Y.hat_2 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_2

#Scatterplot TwoPA on Salary

plot(X2, Y, pch = 16,
     xlab = "Average Two Point Shots Attempted Per Game", 
     ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 20),
     main = "Average Two Point Shots Attempted Per Game vs Salary")
abline(b0_2, b1_2, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVTwoPA_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X2, 
     freq = FALSE,
     xlab = "Average Two Point Shots Attempted Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Average Two Point Shots Attempted Per Game")),
     ylim = c(0, 0.2))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVTwoPA_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X2
plot(X2, e_2, pch = 16,
     xlab = "TwoPA", 
     ylab = expression(paste("Residuals ", sep = "")),
     ylim = c(-20000000,30000000), 
     xlim = c(0,20),
     main = expression(paste("Residual Plot Against Two Points Attempted", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVTwoPA_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#-----------------------------------------------------------------
#    3    Average Two Point Shots Percentage Per Game  (TwoPP)
#-----------------------------------------------------------------

mod_3 = lm(Y~X3)

mod_output_3 = summary(mod_3)
names(mod_output_3)
mod_output_3$coefficients
b0_3 = mod_output_3$coefficients[1,1]
b1_3 = mod_output_3$coefficients[2,1]
s_3 = mod_output_3$sigma
e_3 <- mod_output_3$residuals 
#Estimator for Y
Y.hat_3 = mod_3$fitted.values

R2_3 = sum((Y.hat_3 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_3

#Scatterplot TwoPP on Salary

plot(X3, Y, pch = 16,
     xlab = "Average Two Point Shots Percentage Per Game", 
     ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 1),
     main = "Average Two Point Shots Percentage Per Game vs Salary")
abline(b0_3, b1_3, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVTwoPP_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X3, 
     freq = FALSE,
     xlab = "Average Two Point Shots Percentage Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Average Two Point Shots Percentage Per Game")),
     ylim = c(0, 8))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVTwoPP_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X3
plot(X3, e_3, pch = 16,
     xlab = "TwoPP", 
     ylab = expression(paste("Residuals ", sep = "")),
     ylim = c(-12000000,31000000), 
     xlim = c(0.3,0.8),
     main = expression(paste("Residual Plot Against Two Point Percentage", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVTwoPP_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#-----------------------------------------------------------------
#    4    Average Effective Field Goal Percentage Per Game  (eFGP)
#-----------------------------------------------------------------

mod_4 = lm(Y~X4)

mod_output_4 = summary(mod_4)
names(mod_output_4)
mod_output_4$coefficients
b0_4 = mod_output_4$coefficients[1,1]
b1_4 = mod_output_4$coefficients[2,1]
s_4 = mod_output_4$sigma
e_4 <- mod_output_4$residuals
#Estimator for Y
Y.hat_4 = mod_4$fitted.values

R2_4 = sum((Y.hat_4 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_4

#Scatterplot eFGP on Salary

plot(X4, Y, pch = 16,
     xlab = "Average Effective Field Goal Percentage Per Game", 
     ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 1),
     main = "Average Effective Field Goal Percentage Per Game vs Salary")
abline(b0_4, b1_4, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVeFGP_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X4, 
     freq = FALSE,
     xlab = "Average Effective Field Goal Percentage Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Average Effective Field Goal Percentage Per Game")),
     ylim = c(0, 8))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVeFGP_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X4
plot(X4, e_4, pch = 16,
     xlab = "eFGP", 
     ylab = expression(paste("Residuals ", sep = "")),
     ylim = c(-12000000,31000000), 
     xlim = c(0.1,0.7),
     main = expression(paste("Residual Plot Against Average Effective Field Goal Percentage", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVeFGP_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#-----------------------------------------------------------------
#    5    Average Free Throws Made Per Game  (FT)
#-----------------------------------------------------------------

mod_5 <- lm(Y~X5)

mod_output_5 <- summary(mod_5)
names(mod_output_5)
mod_output_5$coefficients
b0_5 <- mod_output_5$coefficients[1,1]
b1_5 <- mod_output_5$coefficients[2,1]
s_5 <- mod_output_5$sigma
e_5 <- mod_output_5$residuals
#Estimator for Y
Y.hat_5 <- mod_5$fitted.values

R2_5 = sum((Y.hat_5 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_5

#Scatterplot FP on Salary

plot(X5, Y, pch = 16,
     xlab = "Average Free Throws Made Per Game", 
     ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 10),
     main = "Average Free Throws Made Per Game vs Salary")
abline(b0_5, b1_5, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVFT_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X5, 
     freq = FALSE,
     xlab = "Average Free Throws Made Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Average Free Throws Made Per Game")),
     ylim = c(0, 0.5))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVFT_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X5
plot(X5, e_5, pch = 16,
     xlab = "FT", 
     ylab = expression(paste("Residuals ", sep = "")),
     ylim = c(-19000000,24000000), 
     xlim = c(0,10),
     main = expression(paste("Residual Plot Against Average Free Throws Made", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVFT_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#-----------------------------------------------------------------
#    6    Average Free Throws Attempted Per Game  (FTA)
#-----------------------------------------------------------------

mod_6 <- lm(Y~X6)

mod_output_6 <- summary(mod_6)
names(mod_output_6)
mod_output_6$coefficients
b0_6 <- mod_output_6$coefficients[1,1]
b1_6 <- mod_output_6$coefficients[2,1]
s_6 <- mod_output_6$sigma
e_6 <- mod_output_6$residuals
#Estimator for Y
Y.hat_6 <- mod_6$fitted.values

R2_6 = sum((Y.hat_6 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_6

#Scatterplot FPA on Salary

plot(X6, Y, pch = 16,
     xlab = "Average Free Throws Attempted Per Game", 
     ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 11),
     main = "Average Free Throws Attempted Per Game vs Salary")
abline(b0_6, b1_6, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVFTA_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X6, 
     freq = FALSE,
     xlab = "Average Free Throws Attempted Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Average Free Throws Attempted Per Game")),
     ylim = c(0, 0.4))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVFTA_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X6
plot(X6, e_6, pch = 16,
     xlab = "FTA", 
     ylab = expression(paste("Residuals ", sep = "")),
     ylim = c(-19000000,25000000), 
     xlim = c(0,11),
     main = expression(paste("Residual Plot Against Average Free Throws Attempted", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVFTA_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#-----------------------------------------------------------------
#    7    Average Free Throws Percentage Per Game  (FTP)
#-----------------------------------------------------------------

mod_7 <- lm(Y~X7)

mod_output_7 <- summary(mod_7)
names(mod_output_7)
mod_output_7$coefficients
b0_7 <- mod_output_7$coefficients[1,1]
b1_7 <- mod_output_7$coefficients[2,1]
s_7 <- mod_output_7$sigma
e_7 <- mod_output_7$residuals
#Estimator for Y
Y.hat_7 <- mod_7$fitted.values

R2_7 = sum((Y.hat_7 - Y.bar)^2)/sum((Y - Y.bar)^2)
R2_7

#Scatterplot FPA on Salary

plot(X7, Y, pch = 16,
     xlab = "Average Free Throws Percentage Per Game", 
     ylab = "Salary for the 2019-2020 Season",
     ylim = c(0, 41000000), 
     xlim = c(0, 1),
     main = "Average Free Throws Percentage Per Game vs Salary")
abline(b0_7, b1_7, col = "red")

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVFTP_Scatter.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Histogram

hist(X7, 
     freq = FALSE,
     xlab = "Average Free Throws Percentage Per Game",
     main = expression(paste("Histogram of the Sampling Distribution of Average Free Throws Percentage Per Game")),
     ylim = c(0, 4))

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/SalaryVFTP_Histogram.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

#Residual against X7
plot(X7, e_7, pch = 16,
     xlab = "FTP", 
     ylab = expression(paste("Residuals ", 10^2, sep = "")),
     ylim = c(-15000000,31000000), 
     xlim = c(0,1),
     main = expression(paste("Residual Plot Against Average Free Throw Percentage", sep = "")))
abline(h = 0, lty = 3)
#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/ResidualsVFTP_Plot.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

###################################################################

#Corelation
cor(X1, Y, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(X2, Y, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(X3, Y, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(X4, Y, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(X5, Y, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(X6, Y, use = "everything", method = c("pearson", "kendall", "spearman"))
cor(X7, Y, use = "everything", method = c("pearson", "kendall", "spearman"))

# Scatter Plot Matrix

par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X7, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X6, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X5, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X4, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X3, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X2, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(Y ~ X1, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X7 ~ X6, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X6 ~ X5, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X5 ~ X4, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X4 ~ X3, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X3 ~ X2, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X2 ~ X1, bty = "n", lwd = 8)
par(lwd = 3, cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
plot(X1 ~ X7, bty = "n", lwd = 8)

par(cex.axis = 1.5, cex.lab = 1.5, mar = c(5,5,2,2))
pairs(NBA_data[,c(1,2,3,4,5,6,7)], lwd = 3)

#Saving the plot
pdf(file="~/desktop/Fall2020/Stats525/IE Project/IE Project pt.2 Plots/CorelationMatrix.pdf")
#RE-input the code to create the plot. Then input....
dev.off()

###########################################################################
###############################
#Part 3
###############################
###########################################################################


install.packages("olsrr")
library(olsrr)

# set your working directory
setwd(dir = "~/desktop/Fall2020/Stats525/IE Project")
# and read your data files using the "read.table" command
NBA_data = read.table(file = "NBA_data.txt")
names(NBA_data)<-c("Salary", "TwoP", "TwoPA", "TwoPP", "eFGP", "FT", "FTA", "FTP")
attach(NBA_data)

#Assigning all the variables
Y <- NBA_data$Salary
X1 <- NBA_data$TwoP
X2 <- NBA_data$TwoPA
X3 <- NBA_data$TwoPP
X4 <- NBA_data$eFGP
X5 <- NBA_data$FT
X6 <- NBA_data$FTA
X7 <- NBA_data$FTP

#Average Y
Y.bar <- mean(Y)

n <- length(Y)

#multi-linear regression model 
mod <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7)

#e <- mod$residuals
#MSE <- sum(e^2)/(n - 8)

best_mod <- ols_step_best_subset(mod)
summary(best_mod)

# stepwise forward regression
ols_step_forward_p(mod)

# stepwise backward regression
ols_step_backward_p(mod, details = TRUE)

mod_2456 <- lm(Y ~ X2+X4+X5+X6)
best_mod_2456 <- ols_step_best_subset(mod_2456)
summary(best_mod_2456)

#e_2456 <- mod_2456$residuals
#MSE_2456 <- sum(e_2456^2)/(n - 8)
