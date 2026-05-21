glm example
================
me
May 21, 2026

``` r
# Load dplyr for data manipulation and piping
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
# Make random number generation reproducible
set.seed(123)

# Number of observations/rows to generate
data_size <- 100
```

``` r
# create the dataframe
df <- data.frame(
 age = round(rnorm(data_size, 35, 10)),
 income = round(rnorm(data_size, 50000, 15000)),
 education_years = sample(8:18, data_size, replace = TRUE),
 purchased = sample(c("No", "Yes"), data_size, replace = TRUE, prob = c(0.7, 0.3))
)

# Ensure the dependent variable is a factor
df$purchased <- as.factor(df$purchased)

# Check the structure of the data
str(df)
```

    ## 'data.frame':    100 obs. of  4 variables:
    ##  $ age            : num  29 33 51 36 36 52 40 22 28 31 ...
    ##  $ income         : num  39344 53853 46300 44787 35726 ...
    ##  $ education_years: int  14 10 16 14 16 11 9 13 17 16 ...
    ##  $ purchased      : Factor w/ 2 levels "No","Yes": 2 1 1 1 1 2 1 1 1 1 ...

``` r
cat("\n\n")
```

``` r
summary(df)
```

    ##       age            income      education_years purchased
    ##  Min.   :12.00   Min.   :19201   Min.   : 8.00   No :63   
    ##  1st Qu.:30.00   1st Qu.:37983   1st Qu.:10.00   Yes:37   
    ##  Median :36.00   Median :46613   Median :13.00            
    ##  Mean   :35.93   Mean   :48387   Mean   :12.85            
    ##  3rd Qu.:42.00   3rd Qu.:57018   3rd Qu.:16.00            
    ##  Max.   :57.00   Max.   :98616   Max.   :18.00

``` r
#head(x, rows)
head(df, 10)
```

    ##    age income education_years purchased
    ## 1   29  39344              14       Yes
    ## 2   33  53853              10        No
    ## 3   51  46300              16        No
    ## 4   36  44787              14        No
    ## 5   36  35726              16        No
    ## 6   52  49325              11       Yes
    ## 7   40  38226               9        No
    ## 8   22  24981              13        No
    ## 9   28  44297              17        No
    ## 10  31  63785              16        No

Building logistic model using glm(family = “binomial”)

Try to predict purchased based on age, income, and education_years:

``` r
#build regression model,
#purchased ~ age + income + education_years

model <- glm(purchased ~ age + income + education_years,
             data=df, family = 'binomial')


#model <- glm(purchased ~ ., data = df, family = "binomial") if you want all variables, use a dot (.)
```

Summarizing and interpreting the model.

*Deviace Residuals:* indicate how well the mdoel is fit to the data,
smaller is better.

*Coefficients* Estimates are log-odds. Highr log-odd estimate means that
as predictor variable increases, so does the probability of a
postive/“Yes” categorization from the model

std. Error is the standard error of the coefficient estimate (log-odds)

z-value, Wald Z-Stat, testing the significance of each
coefficient/predictor

Pr(\>\|z\|), the p-value. Default significance is 0.05, if its less than
that then the model has shown the predictor is statistically
significant.

*Null Deviance:* deviance of the model with only its intercept

*Residual Deviance:* deviance of the fitted model. if this value is
significantly smaller than null deviance, then the predictors are
suggested to be good for the model

*AIC* measure of fitness, penalizes model for complexity. lower AIC is
indicative of a good model

``` r
# get model summary
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = purchased ~ age + income + education_years, family = "binomial", 
    ##     data = df)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)     -1.417e+00  1.455e+00  -0.974    0.330
    ## age              3.298e-02  2.348e-02   1.405    0.160
    ## income          -1.393e-05  1.525e-05  -0.913    0.361
    ## education_years  2.768e-02  6.301e-02   0.439    0.660
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 131.79  on 99  degrees of freedom
    ## Residual deviance: 128.57  on 96  degrees of freedom
    ## AIC: 136.57
    ## 
    ## Number of Fisher Scoring iterations: 4

Interpret log-odds as odd ratios: Odds ratio \> 1 means that per
one-unit increase in the predictor, the odds of the outcome (“Yes”)
increases by such a factor.

Odds ratio \< 1 means the odds decrese

For example, an odds ratio of 1.2 means the odds increase by 20% for
every one-unit increase in the predictor.

``` r
# Calculate odds ratios
cat("Odds Ratios:\n")
```

    ## Odds Ratios:

``` r
exp(coef(model))
```

    ##     (Intercept)             age          income education_years 
    ##       0.2423756       1.0335288       0.9999861       1.0280621

``` r
cat("\n\nConfidence Intervals:\n")
```

    ## 
    ## 
    ## Confidence Intervals:

``` r
# You can also get confidence intervals for odds ratios
exp(confint(model))
```

    ## Waiting for profiling to be done...

    ##                     2.5 %   97.5 %
    ## (Intercept)     0.0127716 4.015293
    ## age             0.9877914 1.083886
    ## income          0.9999549 1.000015
    ## education_years 0.9085221 1.164756

We’ve built the model and interpreted the performance. Now, predictions.

``` r
# Creating data for predictions
new_data <- data.frame(
  age = c(40, 25, 60),
  income = c(60000, 30000, 80000),
  education_years = c(16, 12, 18)
)

# Predictions, when we let type="response", we get a binary possibility
pred <- predict(model, newdata = new_data, type = 'response')
cat("Predictions:\n")
```

    ## Predictions:

``` r
print(pred)
```

    ##         1         2         3 
    ## 0.3796879 0.3366023 0.4863865

``` r
# now, convert the probabilities into binary outcomes:
pred_classes <- ifelse(pred > 0.5, 'Yes', 'No')
cat("Binary Predictions:\n")
```

    ## Binary Predictions:

``` r
print(pred_classes)
```

    ##    1    2    3 
    ## "No" "No" "No"

Evaluation of model performance. For binary classification, a confusion
matrix is common.

CM shows the counts of true/false positives/negatives. This allows us to
calculate accuracy statistics like accuracy, precision, revall, F1.

``` r
# build CM

# Get predictions from original dataset
predictions_on_df <- predict(model, type="response")

# make those og predicitons binary
predicted_classes_df <- ifelse(predictions_on_df > 0.5, "Yes", "No")

# ensure pred_og is a factor w/ the same levels as the df 
predicted_classes_df <- factor(predicted_classes_df , levels = levels(df$purchased))

# cm
cm <- table(Actual = df$purchased, Predicted = predicted_classes_df)

cat("Confusion Matrix:\n\n")
```

    ## Confusion Matrix:

``` r
print(cm)
```

    ##       Predicted
    ## Actual No Yes
    ##    No  56   7
    ##    Yes 34   3

``` r
#accuracy stats from cm

accuracy <- sum(diag(cm))/sum(cm)
cat("Model Accuracy: ", round(accuracy, 4), "\n")
```

    ## Model Accuracy:  0.59
