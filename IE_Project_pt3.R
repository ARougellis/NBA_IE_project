
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