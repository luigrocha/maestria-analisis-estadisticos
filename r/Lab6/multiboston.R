---
  title: 'Boston and Crime'
author: "Ash S"
output:
  html_document:
  
  number_sections: true
fig_caption: true
toc: true
fig_width: 7
fig_height: 4.5
theme: cosmo
highlight: tango
code_folding: hide
---
  
  # Introduction
  
  The Kernel will discuss model selection and regularization techniques with data containing information on Boston areas. The data will be used to determine the relationship between crime rate and several other variables in the Boston data set. 

First, the data will be looked at in an exploratory manner. Then, a manual model selection will be performed. Afterwards, an automatic model search using best subset selection, the lasso, and ridge regression will be done. The results from each will be discussed. Then a final model will be selected that performed well on the data set, and the reasoning behind the choice will be justified. 

# Data

This paper will be looking at the Boston data frame from the MASS library in R. The data frame contains housing, neighborhood, and location related information about Boston suburbs. The data frame has 506 rows and 14 columns/variables. With that being said, here is a brief description of the variables:
  
  <center>------------------------------ 
  Table 1: Variable Descriptions of the Data Set
</center>
  ```{r}
# Make Variable and Description Table
rm(list=ls()) # Clear the workspace
graphics.off() # Clear graphics

tableCounter = 1
figCounter = 0

library(knitr) # datatables
Variable = c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax","ptratio", "black", "lstat","mdev")
Description = c("per capita crime rate by town",
                "proportion of residential land zoned for lots over 25,000 sq.ft.",
                "proportion of non-retail business acres per town.",
                "Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).",
                "nitrogen oxides concentration (parts per 10 million).",
                "average number of rooms per dwelling.",
                "proportion of owner-occupied units built prior to 1940.",
                "weighted mean of distances to five Boston employment centers.", 
                "index of accessibility to radial highways.", 
                "full-value property-tax rate per $10,000.", 
                "pupil-teacher ratio by town.", 
                "1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.",
                "lower status of the population (percent).", 
                "median value of owner-occupied homes in $1000s.")
df = data.frame(Variable, Description)
kable(df, format = 'markdown')
```
<center>------------------------------
  </center>
  
  
  ```{r setup, warning=FALSE, results='hide', message=FALSE}
library(MASS) # Data source
library(GGally) # ggpairs plot
library(ggplot2) # Other plotting
library(leaps) # regsubsets
library(glmnet) # Ridge Regression and Lasso
library(plotmo) # Plotting glmnet
library(car) # vif among other things
library(knitr) 
library(htmltools) # Table Header Editing

data(Boston) # load data
# Convert Appropriate variables to categorical (factor)
Boston$chas = as.factor(Boston$chas) 
#Boston$rad = as.factor(Boston$rad) # fine as numeric according to professor comment in discussion module

# Read in and prep the data
if (sum(is.na(Boston)) >0) { # to remove any rows with missing values
  Boston = na.omit(Boston)
}

# Set global option for significant digits in output
options(digits=3)
```

# Analysis

As mentioned in the introduction, the Boston data will be explored. Then, a manual model search will be done to determine the relationship between crime rate (crim) and the other variables in the data set. Afterwards, a more automatic model search will be conducted using three separate methods: best subset, ridge regression, and lasso. These methods will be used to see which method can predict crim the best using a test mean square error. Once the model search is complete, a final model will be selected along with the justification for that model.

## Exploratory Analysis

Let us take a look at the summary of the Boston data frame that is produced by R.

<center>------------------------------
  </center>
  ```{r}
summary(Boston)
```
<center>------------------------------
  </center>
  
  Looking at chas, a categorical variable in the data set, a large majority (>90%) of the observations have 0, which indicates most of the areas do not tract bound the Charles River.

The variable we are trying to determine, crim, seems to be have a small range with a large amount of frequencies. One can see the minimum at 0, the median at 0.3, and the 3rd quartile at 3.7. With a max value of 89.0, this points to half of the distribution for crim are in between 0 and 0.3, while the next 25% of the values are in between 0.3 and 3.6, and then the last 25% of the values stretch from 3.6 to 89.0. Some of the other variables like zn and black exhibit similar behavior.

This can be more easily visualized in the following figure that also shows how the variables in the data are related to each other.

<center>------------------------------  
  `r figCounter =  figCounter + 1`
Figure `r figCounter`: First Visual Look at the Data set
</center>
  ```{r, warning=FALSE, results='hide',fig.height=10,fig.width=10}
p = ggpairs(Boston, aes(alpha=0.1), 
            diag = list(continuous = "barDiag"),
            lower = list(continuous = "smooth"))
suppressMessages(print(p))
```

<center>------------------------------
  </center>
  
  From Figure 1, one can see various aspects to the data. As we mentioned before, crim, zn, and black have a few values with a large number of frequencies at one end of its distribution that leads to the distribution being heavy on one side. 

Looking at the top row that shows the variable of interest, crim, and how it relates to other continuous variables via correlation coefficients, rad and tax have the two largest correlation coefficient magnitudes at 0.626 and 0.583 respectively. A majority of the other crim correlation coefficients fall in the range of 0.2 and 0.45 in magnitude, so rad and tax have relatively large values.

The correlation can also be seen visually in the bottom left portion of the figure with the scatter plots and trend lines. 

For the crim scatter plots, one can see a large number of these scatter plots have points that seem to hug the y-axis. This is partly due to the fact that a majority of the values for crim fall near 0 as was mentioned earlier.

Since a large majority of crim values fall near zero, the values that are greater than 10 will have much more influence. In addition to another reason that will be discussed in the analysis for fitting a full linear model, a log transformation of crim helps alleviate this situation and provide for easier modeling.

Below, we transform crim by applying the log function to all crim values and then remake a figure similar to Figure `r figCounter`.

<center>------------------------------  
  `r figCounter =  figCounter + 1`
Figure `r figCounter`: Visual Look at the Data set with log(crim)
</center>
  ```{r, warning=FALSE, results='hide',fig.height=10,fig.width=10}
BostonCrimLog = Boston
BostonCrimLog$crim = log(Boston$crim)
p = ggpairs(BostonCrimLog, aes(alpha=0.1), 
            diag = list(continuous = "barDiag"),
            lower = list(continuous = "smooth"))
suppressMessages(print(p))
```
<center>------------------------------  
  </center>
  
  Looking at the crim histogram, one can now see a distribution that is closer to a normal distribution. It looks like it might be a bi-modal distribution; however, a few points will not have undue influence now unlike before. Also, one can see all the correlation coefficients in the first row have increased in magnitude from about (0.2, 0.65) to (0.4, 0.85). This helps point to variables having a stronger relationship with crim (after crim is log transformed).

Rad and tax still having the largest correlation coefficients as was stated before. Taking a closer at tax and rad, one can see they are highly correlated (>0.90) to each other. This may show up as multicolinearity if both of these variables are included in the model. This will be looked at in more detail later in the analysis. 

From the scatter plot for rad and tax, one can see all the higher values of rad have similar higher values for tax. This results in a 'clump' of data seen in the top right part of the plot. This also helps explain a high correlation between the two variables.

With the data explored, let's look into how a model can be selected to determine the relationship between crime rate (crim) and the other variables in the data set.

## Multiple Linear Regression and Manual Variable Selection

In this section, we will explore the full model initially. Afterwards, variables that are deemed problematic or insignificant will be manually removed and a final model will be looked at. 

With my current knowledge in crime rates, I know that poorer areas tend to have higher crime rates. In this paper, this means that median house value (medv) and crim (crime rate) should have a (inverse) relationship and hopefully this proves to be true. I do not have enough knowledge to make a logical association or decision on the other variables.

### Initial (Full) Model

First, let's look at a model containing all the other variables in the model (aside from crim) to determine the per capita crime rate by town (crim).

We will begin by checking some of the linear assumptions via plots.

<center>------------------------------  
  `r figCounter =  figCounter + 1`
Figure `r figCounter`: Residual Plots for Full Linear Model
</center>
  ```{r}
par(mfrow=c(2,2))
plot(lm(crim ~ ., data=Boston))
```
<center>------------------------------  
  </center>
  
  One can see that large fitted values present issues by having large residuals. Also, the residual variance of the larger fitted values is larger than the lower fitted values. This violates the constant variance assumption and makes it hard to tell the pattern of the residual trend line near zero. 
One method to remedy this is to transform the variable being predicted. In this case, crim is the variable being predicted, so we will transform crim by applying a log function to all the crim values. This was mentioned earlier in the exploratory data analysis section. It should also be noted that the normality plot points to having a heavy tail, which a log transformation can also help remedy.

After applying a log transformation to crim, we get the following plots.

<center>------------------------------  
  `r figCounter =  figCounter + 1`
Figure `r figCounter`: Residual Plots for Full Linear Model (with log transformation of crim)
</center>
  ```{r}
par(mfrow=c(2,2))
plot(lm(log(crim) ~ ., data=Boston))
```
<center>------------------------------  
  </center>
  
  From Figure `r figCounter`, the constant variance assumption is no longer violated as the spread of the residuals remains relatively constant throughout the residuals vs fitted values plot. Also, linearity seems satisfied as the mean of the residuals in the same plot stay near zero as one moves left to right on the plot. From the top right plot, normality of the residuals is much better as almost all the points are close to the ideal normality line. In the bottom right plot, none of the points have a large Cook's distance, which helps indicate none of the points have unneeded influence in the model building.

Overall, the linear model assumptions of linearity, normality, and constant variance do not seem to be violated from these plots. 

From this point on, the crim variable will be log transformed.

We can go over some of the results of the full linear model in the following table

<center>------------------------------
`r tableCounter = tableCounter + 1`
Table `r tableCounter`: Results for Full Linear Model (with log transformation of crim)
</center>
```{r}
# Basic linear model (full data set)
Boston$crim = log(Boston$crim) # log transform crim.
crim.lm = lm(crim ~ ., data=Boston)
crim.lm.summary = summary(crim.lm)
crim.lm.coef = t(t(crim.lm.summary$coefficients)) # Save off results
#look at VIF to see if any multicolinearity exists
crim.lm.vif = t(t(vif(crim.lm)))
colnames(crim.lm.vif) = "VIF"
rownames(crim.lm.vif)[3] = "chas1" 

fullLMresults = merge(crim.lm.vif,crim.lm.coef, by = 0, all = TRUE)
rownames(fullLMresults) = fullLMresults$Row.names
fullLMresults = fullLMresults[-1]
kable(fullLMresults, format = 'markdown')
```
<center>------------------------------
</center>

Looking at the VIF values, one can see rad and tax both have VIF values of `r fullLMresults["rad","VIF"]` and `r fullLMresults["tax","VIF"]` respectively. These are greater than 5, which can indicate multicolinearity exists. 

To remedy this issue, we will remove one of the variables. 

The variable to remove will be determined by using the value with the higher p-value. Tax has a p-value of `r fullLMresults["tax","Pr(>|t|)"]`, which is greater than a 0.05 significance level. This tells us tax does not need to be included in the model especially considering its VIF value and how correlated it is with rad. Also, rad has a p-value of `r fullLMresults["rad","Pr(>|t|)"]` that indicates it should be included in the model.

The full linear model also has an R-squared value of `r crim.lm.summary$r.squared` and adjusted R-squared value of `r crim.lm.summary$adj.r.squared`. The next model will attempt to reduce multicolinearity by removing tax from the predictors.

### Multicolinearity Free Model

As was previously discussed, the variable tax will be removed from the model. The following table presents some of the results of the model, including VIF values.

<center>------------------------------
`r tableCounter = tableCounter + 1`
Table `r tableCounter`: Results for Model without Tax
</center>
```{r}
# Basic linear model (without tax)
crim.lmNoTax = lm(crim ~ . - tax, data=Boston)
crim.lmNoTax.summary = summary(crim.lmNoTax)
crim.lmNoTax.coef = t(t(crim.lmNoTax.summary$coefficients)) # Save off results
#look at VIF to see if any multicolinearity exists
crim.lmNoTax.vif = t(t(vif(crim.lmNoTax)))
colnames(crim.lmNoTax.vif) = "VIF"
rownames(crim.lmNoTax.vif)[3] = "chas1" 

mclFreeResults = merge(crim.lmNoTax.vif,crim.lmNoTax.coef, by = 0, all = TRUE)
rownames(mclFreeResults) = mclFreeResults$Row.names
mclFreeResults = mclFreeResults[-1]
kable(mclFreeResults, format = 'markdown')
```
<center>------------------------------
</center>

Looking at the VIF values, one can see that all values are under 5, although a few values are close to 5 like nox and dis. For now, we will take note and take a look at VIF values in the next models to be cautious. There are also quite a few variables with large p-values. This will be addressed in the next section.

This multicolinearity free linear model has an R-squared value of `r crim.lmNoTax.summary$r.squared` and adjusted R-squared value of `r crim.lmNoTax.summary$adj.r.squared`. 

### Removing Unneeded Variables

The next stop will be to investigate and potentially remove variables that do not need to be included, using a significance value of 0.05. Looking at Table `r tableCounter`, this will be the following variables: chas1, dis, medv, ptratio, rm. After removal of these variables, we will fit a model again to check to see if these variables are affecting the p-values of any of the remaining variables enough at a 0.05 significance value. The following table presents some of the results of the model, including VIF values.

<center>------------------------------
`r tableCounter = tableCounter + 1`
Table `r tableCounter`: Results for Model without tax, chas, dis, medv, ptratio, and rm
</center>
```{r}
# Basic linear model 
crim.lm.test = lm(crim ~ . - tax-chas-dis-rm-medv-ptratio, data=Boston)
crim.lm.final.summary = summary(crim.lm.test)
crim.lm.final.coef = t(t(crim.lm.final.summary$coefficients)) # Save off results
#look at VIF to see if any multicolinearity exists
crim.lm.final.vif = t(t(vif(crim.lm.test)))
colnames(crim.lm.final.vif) = "VIF"

manualFinalModelResults = merge(crim.lm.final.vif,crim.lm.final.coef, by = 0, all = TRUE)
rownames(manualFinalModelResults) = manualFinalModelResults$Row.names
manualFinalModelResults = manualFinalModelResults[-1] # get rid of row.names column
kable(manualFinalModelResults, format = 'markdown')
```
<center>------------------------------
</center>

Table `r tableCounter` shows indus now not being needed to be included in the model at a 0.05 significance level. We will remove indus and refit the model.

### Final Manual Method Model

After removing tax, chas, indus, dis, medv, ptratio, and rm, the final remaining predictors of age, black, lstat, nox, rad, and zn were used to determine crim. They were all deemed significant at a 0.05 significance level. Some of the results can be seen in the table below.

<center>------------------------------
`r tableCounter = tableCounter + 1`
Table `r tableCounter`: Results for Final Manual Method Model
</center>
```{r}
# Basic linear model (with all variables that must be included)
crim.lm.final = lm(crim ~ . - tax-chas-dis-rm-medv-ptratio-indus, data=Boston)
crim.lm.final.summary = summary(crim.lm.final)
crim.lm.final.coef = t(t(crim.lm.final.summary$coefficients)) # Save off results
#look at VIF to see if any multicolinearity exists
crim.lm.final.vif = t(t(vif(crim.lm.final)))
colnames(crim.lm.final.vif) = "VIF"
manualFinalModelResults = merge(crim.lm.final.vif,crim.lm.final.coef, by = 0, all = TRUE)
rownames(manualFinalModelResults) = manualFinalModelResults$Row.names
manualFinalModelResults = manualFinalModelResults[-1] # get rid of row.names column
kable(manualFinalModelResults, format = 'markdown')
```
<center>------------------------------
</center>

Table `r tableCounter` shows these variables need to be included at a 0.05 significance level. Looking at the VIF values, they all fall below 3 and have better values than previous models. The model reduction helped reduce VIF values. As mentioned earlier, my knowledge between poor areas having higher crime rates is not supported by the final model as it was not deemed significant enough to be needed in the model.

We will also check linear model assumptions as we did with the initial model.

<center>------------------------------  
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Residual Plots for Final Linear Model
</center>
```{r}
par(mfrow=c(2,2))
plot(crim.lm.final)
```
<center>------------------------------  
</center>

Figure `r figCounter` shows similar patterns as in the initial model (with the log transformation of crim), so it does not seem to violate the linear model assumptions of linearity, normality, and constant variance.

The following figure helps check the assumption of independent errors.

<center>------------------------------  
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Residual vs Index Plot
</center>
```{r}
par(mfrow=c(1,1))
plot(crim.lm.final.summary$residuals, main = 'Residuals vs Index', ylab='Residuals')
lines(crim.lm.final.summary$residuals)
```
<center>------------------------------  
</center>

Figure `r figCounter` shows no obvious serial pattern, so the independent error assumption does not seem to be violated. 

With that being said, this final manual method model has an R-squared value of `r crim.lm.final.summary$r.squared` and adjusted R-squared value of `r crim.lm.final.summary$adj.r.squared`. This is only a minor drop from the full linear model that had an R-squared value of `r crim.lm.summary$r.squared` and adjusted R-squared value of `r crim.lm.summary$adj.r.squared`; however, we decreased the number of predictors by over half over the full linear model. This makes it a more simple and easy to interpret the model.

## Automated Model Selection

This section will delve into three different (more automated) methods for model searching. The first will be best subset selection. Followed by ridge regression. Then, lasso will be used. For all model searches, a 10-fold cross-validation (CV) method will be used in some fashion. Recall, the crim variable in the Boston data set is now log transformed as was mentioned earlier. This was to help alleviate some assumption issues and crim distribution concerns.

### Best Subset Selection

To perform best subset selection, we fit a separate least squares regression for each possible combination of the p predictors. That is, we fit all p models that contain exactly one predictor, all models that contain exactly two predictors, and so forth. We then look at all of the resulting models, with the goal of identifying the one that is the 'best' at determining the relationship between crim and the other variables. 

'Best' in this paper will be based on using (10-fold) cross-validated prediction error. In other words, the model that minimizes the mean squared prediction error over the folds will be the 'best' model. A 'best' model will be chosen for each number of predictors up to the model with all the predictors included.

```{r}
# Create predict like function for regsubsets
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
} #predict.regsubsets

# Set up Folds for k-fold cross validation
k=10
set.seed(8)
folds=sample(1:k,nrow(Boston),replace=TRUE)
maxNumPred = 13
# Initialize error matrix 
cv.errors=matrix(NA,k,maxNumPred, dimnames=list(NULL, paste(1:maxNumPred))) 
# Perform k-fold cross validation and get errors
for(j in 1:k){
  best.fit=regsubsets(crim~.,data=Boston[folds!=j,],nvmax=maxNumPred+1)
  for(i in 1:maxNumPred){
    pred=predict(best.fit,Boston[folds==j,],id=i)
    cv.errors[j,i]=mean( (Boston$crim[folds==j]-pred)^2)
  }
}
# Get mean of errors across k-folds
mean.cv.errors=apply(cv.errors,2,mean)
# Find where minimum mean error is
bestSubsetNumVars = which.min(mean.cv.errors)
BS.cv.MSE = min(mean.cv.errors)
# Get best subset coefficients using all the data (based on the number of variables found when minimizing mean.cv.error)
reg.best = regsubsets(crim~.,data=Boston, nvmax = maxNumPred+1)
BS.cv.coef = coef(reg.best,bestSubsetNumVars)
# Save off results for future use and display
modelResults = data.frame(as.list(BS.cv.coef))
modelResults$'Test MSE' = BS.cv.MSE
row.names(modelResults) = 'BestSubset'
# Move 'Test MSE' to front
modelResults = modelResults[,c(which(colnames(modelResults)=="Test MSE"),which(colnames(modelResults)!="Test MSE"))]
modelResults = t(modelResults)
```

In the following figure, we can see how the average mean squared error (MSE) over the different folds changes with the different number of variables in the best subset model.

<center>------------------------------
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Best Subset Selection Errors vs Number of Variables</center>
```{r}
# Plot mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type="b", xlab= 'Number of Variables in Model', ylab='MSE over the folds')
abline(v=bestSubsetNumVars, lty = 2)
title('Best Subset Selection Model Errors vs Number of Variables')
```
<center>------------------------------
</center>

From Figure `r figCounter`, one can see that the best subset model with `r bestSubsetNumVars` variables had the lowest average/mean squared error (MSE) over the folds during k-fold CV with an MSE of `r BS.cv.MSE`. The table below gives more details on what values the lowest test MSE are along with what variables are in the model and their values.

<center>------------------------------
`r tableCounter =  tableCounter + 1`
Table `r tableCounter`: Results for Best Subset Selection
</center>
```{r}
# display results in table
kable(modelResults, format = 'markdown')
```

<center>------------------------------
</center>

From the Table `r tableCounter`, one can see the minimum test MSE was `r modelResults["Test MSE",]`, which aligns to about where the minimum point is in Figure `r figCounter`.

### Ridge Regression

From reference [1], ridge regression is very similar to least squares, except that the coefficients are estimated by minimizing a slightly different quantity. In particular, the ridge regression coefficient estimates, $\hat{\beta}^R_\lambda$, are the values that minimize:

$$\sum^n_{i=1} (y_i-\beta_0-\sum^p_{j=1}\beta_jx_{ij} )^2 +\lambda\sum^p_{j=1}\beta_j^2 = RSS + \lambda\sum^p_{j=1}\beta_j^2$$
where RSS is root sum squared and $\lambda\ge0$ is a tuning parameter, to be determined separately with k-fold cross validation. The 'best' model will have a $\lambda$ that will minimize the mean cross-validated error across the k-folds. 

The data will be split into a training set and testing set. The training set will be used to create a model and use 10-fold CV to determine the 'best' lambda. The 'best' lambda will be the one with the lowest average MSE. Once that is determined, the lambda value and model based on the training value will be used to predict values for the test data set to get a test mean square error.

```{r, warning=FALSE, results='hide'}
# Create our x matrix
x = model.matrix(crim~.,Boston)[,-1]
y = Boston$crim # Create our response vector (y)

set.seed(8)
# Get Training and Testing Data Set
train = sample(1:nrow(x), 3*nrow(x)/5) # wanted ~300 obs for 10-fold CV, so each fold has ~30 obs.
test = -(train)
y.test = y[test]

# Fit ridge regression on the training set
grid=10^seq(10, -2, length=100)
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)

# Perform k-fold cross validation on all the data to determine best lambda value
ridge.cv.out = cv.glmnet(x[train,], y[train], alpha=0) # this does 10-fold cv by default, appropriate with 500 obs
ridge.bestlam=ridge.cv.out$lambda.min

# MSE associated with value of lambda that results in smallest cv-error
ridge.pred = predict(ridge.mod, s=ridge.bestlam, newx=x[test,])
ridge.MSE = mean((ridge.pred - y.test)^2)

# Now refit ridge regression on full data set, using the value of lambda
# selected by cross-validation
ridge.modF = glmnet(x, y, alpha=0, lambda=grid)
ridge.coef=predict(ridge.modF,type="coefficients",s=ridge.bestlam)[1:maxNumPred,]
# Save off Results
ridgeModelResults = data.frame(as.list(ridge.coef))
ridgeModelResults$'Test MSE' = ridge.MSE
ridgeModelResults$BestLambda = ridge.bestlam
row.names(ridgeModelResults) = 'RidgeRegression'
# Rearrange variables 
ridgeModelResults = ridgeModelResults[,c(which(colnames(ridgeModelResults)=="BestLambda"), which(colnames(ridgeModelResults)!="BestLambda"))]
ridgeModelResults = ridgeModelResults[,c(which(colnames(ridgeModelResults)=="Test MSE"),which(colnames(ridgeModelResults)!="Test MSE"))]
ridgeModelResults = t(ridgeModelResults) # transpose for easier table display
```

In the following figure, one can see how the error changes as lambda is changed for the ridge regression model (that is based on the training data).

<center>------------------------------
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Ridge Regression Model Errors vs Lambda</center>
```{r}
plot(ridge.cv.out)
```
<center>------------------------------
</center>

From the figure, one can see the lowest MSE occurs at a log(Lambda) below 0 and has a MSE value of around 0.4. The figure also indicates this model has all 13 variables in it from the x axis on top of the plot. In fact, ridge regression always includes all 13 variables for the lambda values shown. Remember, this figure is based on the model produced by the training set. 

How the variable coefficient values change with lambda is given below in a visual manner. 

<center>------------------------------
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Ridge Regression Model Coefficients vs Lambda
</center>
```{r}
plot_glmnet(ridge.modF)
abline(v=log(ridge.bestlam), lty = 2)
```

<center>------------------------------
</center>

From the figure, the dotted dash vertical line indicates the 'best' lambda value that results in the lowest training MSE. This figure is based on all the data (that includes both the training and testing set). One can see that the nox coefficient has the largest magnitude in the 'best' model selected. This is also shown in the table below that also displays the actual values along with the best lambda and test MSE values. 

<center>------------------------------
`r tableCounter =  tableCounter + 1`
Table `r tableCounter`: Results for Ridge Regression
</center>
```{r}
# display results in table
kable(ridgeModelResults, format = 'markdown')
```
<center>------------------------------
</center>

From the Table `r tableCounter`, one can see the minimum test MSE was `r ridgeModelResults["Test MSE",]`. Table `r tableCounter` also shows all variables were chosen and shows their values. As seen in Figure `r figCounter`, nox has largest coefficient value magnitude. 

### Lasso Model

One downside to ridge regression is that it will usually generate a model involving all the predictors even if some are irrelevant. It can create a challenge in model interpretation. The lasso is a relatively recent alternative to ridge regression that overcomes this disadvantage. The lasso coefficient estimates, $\hat{\beta}^L_\lambda$, are the values that minimize:

$$\sum^n_{i=1} (y_i-\beta_0-\sum^p_{j=1}\beta_jx_{ij} )^2 +\lambda\sum^p_{j=1}|{\beta_j}| = RSS + \lambda\sum^p_{j=1}|\beta_j|$$
Similar to ridge regression, $\lambda\ge0$ is a tuning parameter that will be determined separately with k-fold cross validation. The 'best' model will have a $\lambda$ that will minimize the mean cross-validated error across the k-folds.

As with ridge regression, the data will be split into a training set and testing set. The training set will be used to create a model and use 10-fold CV to determine the 'best' lambda. The 'best' lambda will be the one with the lowest average MSE. Once that is determined, the lambda value and model based on the training value will be used to predict values for the test data set and get a test mean square error.

```{r, warning=FALSE, results='hide'}
set.seed(1)
# Fit lasso on the training set (same one used as ridge)
grid=10^seq(10, -2, length=100) # using same grid as before (optional: could use new grid)
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid, thresh=1e-12)

# Create lasso model, perform k-fold CV, and get best lambda and lowest MSE
lasso.cv.out= cv.glmnet(x[train,], y[train], alpha=1)
lasso.bestlam=lasso.cv.out$lambda.min

lasso.pred = predict(lasso.mod, s=lasso.bestlam, newx=x[test,])
lasso.MSE = mean((lasso.pred - y.test)^2) # test set MSE

# Now refit lasso on full data set, using the value of lambda
# selected by cross-validation
lasso.modF = glmnet(x, y, alpha=1, lambda=grid)
lasso.coef = predict(lasso.modF, type="coefficients", s=lasso.bestlam)[1:maxNumPred,]

# Get coefficients and save off results
lasso.coef = data.frame(as.list(lasso.coef))
lassoModelResults = data.frame(as.list(lasso.coef))
lassoModelResults$'Test MSE' = lasso.MSE
lassoModelResults$BestLambda = lasso.bestlam
row.names(lassoModelResults) = 'Lasso'
# Rearrange variables
lassoModelResults = lassoModelResults[,c(which(colnames(lassoModelResults)=="BestLambda"), which(colnames(lassoModelResults)!="BestLambda"))]
lassoModelResults = lassoModelResults[,c(which(colnames(lassoModelResults)=="Test MSE"),which(colnames(lassoModelResults)!="Test MSE"))]
lassoModelResults = t(lassoModelResults) # transpose for easier table display
```

In the following figure, one can see how the error changes as lambda is changed for the lasso model (based on the training data).

<center>------------------------------
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Lasso Model Errors vs Lambda</center>
```{r}
plot(lasso.cv.out)
```

<center>------------------------------
</center>

From the figure, one can see the lowest MSE occurs at a log(Lambda) value near -4. The MSE value is around 0.4. The figure also indicates this model has around 8 variables in it from the x axis on top of the plot. Remember, this figure is based on the model produced by the training set. 

How these variable coefficient values change with lambda (along with the number of variable coefficients) is given below in a visual manner.

<center>------------------------------
`r figCounter =  figCounter + 1`
Figure `r figCounter`: Lasso Model Coefficients vs Lambda
</center>
```{r}
plot_glmnet(lasso.modF)
abline(v=log(lasso.bestlam), lty = 2)
```
<center>------------------------------
</center>

From the figure, the dotted dash vertical line indicates the 'best' lambda value that results in the lowest MSE. This figure is based on all the data (that includes both the training and testing set). One can see that the nox coefficient has the largest magnitude in the 'best' model selected. This is also shown in the table below that also displays the actual values along with the best lambda and test MSE values. 

<center>------------------------------
`r tableCounter =  tableCounter + 1`
Table `r tableCounter`: Results for Lasso
</center>
```{r}
# display results in table
kable(lassoModelResults, format = 'markdown')
```

<center>------------------------------
