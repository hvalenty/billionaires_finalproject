# cleaning and MLR

library(tidyverse)
# set working directory
setwd('C:/Users/Valenty/VSCode/billionaires_finalproject')
# read in original data set
bill <- read.csv('data/Billionaires-Statistics.csv')
View(bill)
# normalize column names with lowercase
colnames(bill) <- tolower(colnames(bill))
# subset OG dataframe -- remove unwanted columns
bill_num <- bill[, c(2,5,22,25,26,27,28,29,30,31,32,33)] %>%
  na.omit()
View(bill_num)
# fix GDP column to be numeric
bill_num$gdp_country <- gsub( ',', '', bill_num$gdp_country) # four times 
bill_num$gdp_country <- gsub( '\\$', '', bill_num$gdp_country)
bill_num$gdp_country <- gsub( ' ', '', bill_num$gdp_country)
bill_num$gdp_country <- as.numeric(bill_num$gdp_country)
bill_num$gdp_country
# visualizations
cor_mat <- round(cor(bill_num),2)
ggcorrplot::ggcorrplot(cor_mat, lab=F, type='lower',
                       col=colorRampPalette(c("#ff8700","white","#0000FF"))(3))
# Highest correlation
## -1 age:birthyear
## -0.75 cpi_country:life_expectancy_country
## -0.65 total_tax_rate:population_country
## -0.60 gdp_country:tax_revenue_country

# Now the MLR
modbill <- lm(finalworth~., data=bill_num)
summary(modbill)  
# step AIC
aic <- MASS::stepAIC(modbill, direction='both', Trace=FALSE)
summary(aic)
# Step AIC recommends birthyear, GDP, population
library(car)
vif(aic)
# All well below 5 -- no multicollinearity
# make prediction with lm


library(glmnet)
# design matrix
X <- model.matrix(finalworth~0+., data=bill_num)
y <- bill_num$finalworth
# Ridge regression
ridgemod <- glmnet(x=X,y=y, alpha=0) # ridge
plot(ridgemod, label=T, xvar='lambda')
## ridge cross validation
ridgeglm <- cv.glmnet(x=X, y=y, alpha=0, nfolds=10)
ridgeglm$lambda.min
ridgeglm$lambda.1se
## prediction of response with ridge
predict(ridgemod, type='response', s=ridgeglm$lambda.min, newx=X[1:2,])
# compare with originals
X[1:2,]

# Lasso regression
lassomod <- glmnet(x=X,y=y, alpha=1) # lasso
plot(lassomod, label=T, xvar='lambda')
## lasso cross validation
lassoglm <- cv.glmnet(x=X, y=y, alpha=1, nfolds=10)
lassoglm$lambda.min
lassoglm$lambda.1se
## plot with lambda min and 1se
plot(lassomod, label=T, xvar='lambda')+
  abline(v=log(lassoglm$lambda.1se), col='green')+
  abline(v=log(lassoglm$lambda.min), col='red')
## prediction of response with lasso
predict(lassomod, type='response', s=lassoglm$lambda.min, newx=X[1:2,])

# use pca/pcr for final testing