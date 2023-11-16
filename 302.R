
library(knitr)
library(dplyr)

##### Part A - Univariate Model #####

##### 0. Data Ingestion #####
path_folder = "D:\\Program Files\\Google Drive\\Academy\\9 - 2022 Fall\\STA302\\data\\"
file_name = "india_birth.csv"

df<- read.csv(paste0(path_folder, file_name), header = T)
df_uni<- data.frame(df$Institutional_Delivery, df$Infant_Mortality_Rate)
model <- lm(df.Infant_Mortality_Rate ~ df.Institutional_Delivery, data = df_uni)

##### 1. Data Cleaning #####

##### 1.1 Identifying influential points using Cooks Distance#####
D   <- cooks.distance(model)
lev <- which(D > 4/(nrow(df_uni)-2))

par(family = 'serif')
plot(df_uni$df.Institutional_Delivery, D, type = "p", 
     xlab = "Institutional Delivery", ylab = "Cook's Distances", 
     main = "Cook's Distance", col = ifelse(D > 4/(nrow(df_uni)-2), "red", "blue"))
text(df_uni$df.Institutional_Delivery[D > 4/(nrow(df_uni)-2)]+0.5, D[D > 4/(nrow(df_uni)-2)], 
     labels = which(D > 4/(nrow(df_uni)-2)) )

##### 1.2 Refit after removing influential points#####
df_uni <- df_uni[-c(79,82,83,112,141,211,265,266,272,274,281),]
model <- lm(df.Infant_Mortality_Rate ~ df.Institutional_Delivery, data = df_uni)

plot(df_uni$df.Institutional_Delivery, df_uni$df.Infant_Mortality_Rate, type = "p", pch = 20,
     xlab = "Institutional_Delivery", ylab = "Infant_Mortality_Rate", 
     main = "Institutional Delivery vs Infant Mortality Rate", col = "red")
abline(model, lwd = 2, col = "blue")

##### 2. Diagnostic Checking #####

##### 2.1 Normal Q-Q Plot (Normality Assumption) #####
r <- rstandard(model)
qqnorm(r)
qqline(r)

##### 2.2 Standardized Residue vs Predictor Plot (Linearity Assumption) #####
hii <- hatvalues(model)
r   <- rstandard(model)

par(family = 'serif')
plot(df_uni$df.Institutional_Delivery, r, type = "p", 
     xlab = "Institutional Delivery", ylab = "Standardized Residuals",
     main = "Standardized Residuals", col = "red", xlim = c(0, 100), ylim = c(-4, 4))
abline(h = 2, lty = 2)
abline(h = -2, lty = 2)

##### 2.3 Observe Patterns in Variance (Homoscedasticity Assumption) #####
hii   <- hatvalues(model)
r     <- rstandard(model)
r_mod <- sqrt(abs(r))

best_fit <- lm(r_mod ~ df_uni$df.Institutional_Delivery)

plot(df_uni$df.Institutional_Delivery, r_mod, type = "p", 
     xlab = "Institutional Delivery", ylab = "Standardized Residuals",
     main = "Standardized Residuals", col = "red", xlim = c(0, 100), ylim = c(-1, 3))
abline(best_fit, lwd = 2, col = "blue")

##### 3. Analysis Process #####

##### 3.1 Fitting Best Line #####
model %>%
  tidy() %>%
  kable(caption = "The summary from the simple linear regression model")

plot(df_uni$df.Institutional_Delivery, df_uni$df.Infant_Mortality_Rate, type = "p", pch = 20,
     xlab = "Institutional_Delivery", ylab = "Infant_Mortality_Rate",
     main = "Institutional Delivery vs Infant Mortality Rate", col = "red")
abline(model, lwd = 2, col = "blue")

##### 3.2 Confidence Intervals #####
df_uni <- df_uni[order(df_uni$df.Institutional_Delivery),]

yhat <- predict(model, newdata = df_uni, type = "response", interval = "none")
conf.inter <- predict(model, newdata = df_uni, type = "response", interval = "confidence", level = 0.95)
pred.inter <- predict(model, newdata = df_uni, type = "response", interval = "prediction", level = 0.95)

plot(df_uni$df.Institutional_Delivery, df_uni$df.Infant_Mortality_Rate, type = "p", xlab = "Order Size", ylab = "Time (Minutes)",
     main = "Institutional Delivery vs Infant Mortality Rate", col = "red", xlim = c(20, 100), ylim = c(20, 90))
abline(model, lwd = 2, col = "blue")
lines(df_uni$df.Institutional_Delivery, conf.inter[,2], lty = 2, col = "blue")
lines(df_uni$df.Institutional_Delivery, conf.inter[,3], lty = 2, col = "blue")
lines(df_uni$df.Institutional_Delivery, pred.inter[,2], lty = 2, col = "red")
lines(df_uni$df.Institutional_Delivery, pred.inter[,3], lty = 2, col = "red")

##### 3.3 ANOVA Test for H0 #####
#  Under H0 we have the following assumption
#  they are normally distributed (checked 2.1),
#  the errors are independent of each other (checked 2.2), 
#  they have constant variance and mean 0 (checked 2.3).

anova.1 <- anova(model)
anova.1$`F value`
# 0.5412858

qf(0.95, 1, 282)
# 3.874645

# 0.5412858 < 3.874645 We failed to Reject H0

##### Part B - Multivariate Model #####

##### 0. Data Ingestion #####
path_folder = "D:\\Program Files\\Google Drive\\Academy\\9 - 2022 Fall\\STA302\\data\\"
file_name = "india_birth.csv"

df <- read.csv(paste0(path_folder, file_name), header = T)
model <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Delivery_At_Home + Mothers_Who_Received_Any_Antenatal_Check_Up 
            + Mothers_Who_Had_Full_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)

##### 1. Check Multicollinearity #####

##### 1.1 Scatter Plot Matrix and VIF #####
pairs(~ Institutional_Delivery + Delivery_At_Home + Mothers_Who_Received_Any_Antenatal_Check_Up +
        Mothers_Who_Had_Full_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)

Xmat<- model.matrix(model)
XTX <- solve(t(Xmat)%*%Xmat)
rankMatrix(XTX)
H <- Xmat%*%XTX%*%t(Xmat)
rankMatrix(H)
summary(model)
vif(model)  ## The vif function is from the car package

# As seen Institutional Delivery and Delivery at Home are almost perferctly related.
# We remove the variable "Delivery At Home" as it provides the exact same information as "Instutitonal Delivery"
model <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up 
            + Mothers_Who_Had_Full_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)

##### 1.2 Remove variable and Repeat 1.1 #####
pairs(~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up +
        Mothers_Who_Had_Full_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)

Xmat<- model.matrix(model)
XTX <- solve(t(Xmat)%*%Xmat)
rankMatrix(XTX)
H <- Xmat%*%XTX%*%t(Xmat)
rankMatrix(H)
summary(model)
vif(model)  ## The vif function is from the car package

# VIF < 5 We accept the assumption that there isn't multidisciplinary

##### 2. Data Cleaning #####

##### 2.1 Cook's Distance, DFFITS, DFBETAS #####
D <- cooks.distance(model)
which(D > qf(0.5, 5, 284-5))

dfits <- dffits(model)
which(abs(dfits) > 2*sqrt(5/284)) 

dfb <- dfbetas(model)
which(abs(dfb[,2]) > 2/sqrt(284))

# Remove influential observations, refit model
df <- df[-c(3,9,77,79,80,83,86,87,121,127,137,141,155,211,261,265,266,272,273,274,281),]
model <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up 
            + Mothers_Who_Had_Full_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)

##### 3. Analysis Process #####

##### 3.1 Normal Q-Q plot and Standard Residue Plot #####
resid <- rstandard(model)
fitted <- predict(model)

par(family = 'serif', mfrow = c(1,2))
qqnorm(resid)
qqline(resid)

plot(resid ~ fitted, type = "p", 
     xlab = "Fitted Values", ylab = "Standardized Residual", 
     cex.lab = 1.2, col = "red")
lines(lowess(fitted, resid), col = "blue")

##### 3.2 Response vs Fitted values #####
par(family = 'serif')
plot(df$Infant_Mortality_Rate ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Infant Mortality Rate", cex.lab = 1.2,
     col = "red")
abline(lm(df$Infant_Mortality_Rate ~ fitted), lwd = 2, col = "blue")
lines(lowess(fitted, df$Infant_Mortality_Rate), col = "red")

#No Transformation Needed

##### 4. ANCOVA Test #####
# No Indicator variable dividing observations into two groups

##### 5. Model Selection #####
#The goal is to have an interpretable model, prediction accuracy is of secondary importance
#Potential choice: Forward Selection, Backward Elimination, AIC, BIC-based stepwise selection

##### 5.1 Manual Investigation #####
criteria <- function(model){
  n <- length(model$residuals)
  p <- length(model$coefficients) - 1
  RSS <- sum(model$residuals^2)
  R2 <- summary(model)$r.squared
  R2.adj <- summary(model)$adj.r.squared
  AIC <- n*log(RSS/n) + 2*p
  AICc <- AIC + (2*(p+2)*(p+3))/(n-p-1)
  BIC <- n*log(RSS/n) + (p+2)*log(n)
  res <- c(R2, R2.adj, AIC, AICc, BIC)
  names(res) <- c("R Squared", "Adjsuted R Squared", "AIC", "AICc", "BIC")
  return(res)
}

## The crteria ##
## model 1 ##
model.full <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up 
              + Mothers_Who_Had_Full_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)
crit1 <- criteria(model = model.full)
## model 2 ##
model.3.1 <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up + 
                  Mothers_Who_Had_Full_Antenatal_Check_Up, data = df)
crit2 <- criteria(model = model.3.1)
## model 3 ##
model.3.2 <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up + 
                  Mothers_Who_Underwent_Ultrasound, data = df)
crit3 <- criteria(model = model.3.2)
## model 4 ##
model.3.3 <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Had_Full_Antenatal_Check_Up + 
                   Mothers_Who_Underwent_Ultrasound, data = df)
crit4 <- criteria(model = model.3.3)
## model 5 ##
model.3.4 <- lm(Infant_Mortality_Rate ~ Mothers_Who_Received_Any_Antenatal_Check_Up + Mothers_Who_Had_Full_Antenatal_Check_Up
                 + Mothers_Who_Underwent_Ultrasound, data = df)
crit5 <- criteria(model = model.3.4)
## model 6 ##
model.2.1 <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Received_Any_Antenatal_Check_Up, data = df)
crit6 <- criteria(model = model.2.1)
## model 7 ##
model.2.2 <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Had_Full_Antenatal_Check_Up, data = df)
crit7 <- criteria(model = model.2.2)
## model 8 ##
model.2.3 <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + Mothers_Who_Underwent_Ultrasound, data = df)
crit8 <- criteria(model = model.2.3)

## Comapre the criteria for each model ##
rbind(crit1, crit2, crit3, crit4, crit5, crit6, crit7, crit8)
# Result is not promising, we proceed to the next step

##### 5.2 LASSO selection #####
df_select = data.frame(df$Infant_Mortality_Rate,
                       df$Institutional_Delivery,
                       df$Mothers_Who_Received_Any_Antenatal_Check_Up, 
                       df$Mothers_Who_Had_Full_Antenatal_Check_Up,
                       df$Mothers_Who_Underwent_Ultrasound)
model.select <- lm(df.Infant_Mortality_Rate ~ df.Institutional_Delivery + df.Mothers_Who_Received_Any_Antenatal_Check_Up 
             + df.Mothers_Who_Had_Full_Antenatal_Check_Up + df.Mothers_Who_Underwent_Ultrasound, data = df_select)

## choose lambda ##
set.seed(1006353848)
cv.out <- cv.glmnet(x = as.matrix(df_select[,2:5]), y = df_select$df.Infant_Mortality_Rate, standardize = T, alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.1se
best.lambda
co<-coef(cv.out, s = "lambda.1se")

thresh <- 0.00
# select variables #
inds<-which(abs(co) > thresh )
variables<-row.names(co)[inds]
sel.var.lasso<-variables[!(variables %in% '(Intercept)')]
sel.var.lasso

##### 5.3 Step wise Selection based on AIC & BIC #####

## Based on AIC ##
n <- nrow(df_select)
sel.var.aic <- step(model.select, trace = 0, k = 2, direction = "both") 
sel.var.aic<-attr(terms(sel.var.aic), "term.labels")   
sel.var.aic

## Based on BIC ##
n <- nrow(df_select)
sel.var.bic <- step(model.select, trace = 0, k = log(n), direction = "both") 
sel.var.bic<-attr(terms(sel.var.bic), "term.labels")   
sel.var.bic


##### 6. Model Validation #####
df_test  <- df_select[row.names(df) %in% 1:131, ]
df_valid <- df_select[row.names(df) %in% (134):nrow(df), ]

##### 6.1 Cross Validation and prediction performance of AIC based selection #####
ols.aic <- ols(df.Infant_Mortality_Rate ~ ., data = df_valid[,which(colnames(df_valid) %in% c(sel.var.aic, "df.Infant_Mortality_Rate"))], 
               x=T, y=T, model = T)
aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10) ## 10 fold cross validation   
plot(aic.cross, las = 1, xlab = "Predicted LPSA", main = "Cross-Validation calibration with AIC")

pred.aic <- predict(ols.aic, newdata = df_test[,which(colnames(df_valid) %in% c(sel.var.aic, "df.Infant_Mortality_Rate"))]) ## Test Error ##
pred.error.AIC <- mean((df_test$df.Infant_Mortality_Rate - pred.aic)^2) ## Prediction error ##

##### 6.2 Cross Validation and prediction performance of BIC based selection #####
ols.aic <- ols(df.Infant_Mortality_Rate ~ ., data = df_valid[,which(colnames(df_valid) %in% c(sel.var.aic, "df.Infant_Mortality_Rate"))], 
               x=T, y=T, model = T)
aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10) ## 10 fold cross validation   
plot(aic.cross, las = 1, xlab = "Predicted LPSA", main = "Cross-Validation calibration with AIC")

pred.aic <- predict(ols.aic, newdata = df_test[,which(colnames(df_valid) %in% c(sel.var.aic, "df.Infant_Mortality_Rate"))]) ## Test Error ##
pred.error.BIC <- mean((df_test$df.Infant_Mortality_Rate - pred.aic)^2) ## Prediction error ##

##### 6.3 Cross Validation and prediction performance of LASSO based selection #####
ols.lasso <- ols(df.Infant_Mortality_Rate ~ ., data = df_valid[,which(colnames(df_valid) %in% c(sel.var.lasso, "df.Infant_Mortality_Rate"))], 
                 x=T, y=T, model = T)

## 10 fold cross validation ##    
lasso.cross <- calibrate(ols.lasso, method = "crossvalidation", B = 10)
plot(lasso.cross, las = 1, xlab = "Predicted LPSA", main = "Cross-Validation calibration with LASSO")

pred.lasso <- predict(ols.lasso, newdata = df_test[,which(colnames(df_valid) %in% c(sel.var.lasso, "df.Infant_Mortality_Rate"))]) ## Test Error ##
pred.error.lasso <- mean((df_test$df.Infant_Mortality_Rate - pred.lasso)^2) ## Prediction error ##

print(c(pred.error.AIC, pred.error.BIC, pred.error.lasso))


##### 7. Analysis Process #####
# After model selection, we decide to use the model from BIC based selection.
# As LASSO and AIC include 4 predictors, BIC includes 3 predictor.
# The goal of this study is not to predcit, but to understand the relation.
# Hence, less variable is desirble.

model.final <- lm(Infant_Mortality_Rate ~ Institutional_Delivery + 
                    Mothers_Who_Received_Any_Antenatal_Check_Up + Mothers_Who_Underwent_Ultrasound, data = df)
summary(model.final)
anova(model.final)
qf(0.95, 1, 282)

# Again we failed to reject H0 for the predictor Institutional_Delivery, 
# meaning Institutional delivery has no effect on Infant Mortality Rate
# We successfully rejected H0 for the other two predictors
# However, based on the summary we conclude these two predictors has negligble effect on Infant Mortality Rate
