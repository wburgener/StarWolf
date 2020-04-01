---
title: "House Selling Prices"
class: "Math 425 - Linear Regression"
author: "Wes Burgener"
date: "Novemeber 18th, 2019"
output:
  html_document:  
    keep_md: true
    toc: true
    toc_float: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---



<br>

##Background & Analysis

<br>

For this analysis, we were given a dataset from a Kaggle competition. We were tasked to find a model to predict the sales prices of homes. When I first dived into the dataset, I gained understanding of the data by checking out the parirs plot and text description. I began by first making a rather complex model. I soon realized interpretation might be difficult, so I took a step back and removed all of my interaction and quadratic terms to make it easier to interpret. Let's dive into the play by play of the process I went through to develop my model.

<br>

I began thinking about what matters to people when they buy a home. I thought of the size of the home and accompanying land, the nieghborhood, the numbers of bedrooms and baths, etc. I first combined the total square footage of the first, second, and basement. I named this new column TotalSF. Here's the plot and the summary of the regression when I run TotalSF as the explanatory variable for SalesPrice.

<br>


```r
df <- read.csv(file = "/Users/wes/Documents/StarWolf/Data/HSPtrain.csv")
df_1 <- read.csv(file = "/Users/wes/Documents/StarWolf/Data/HSPtest.csv")

new_df <- df %>% 
  mutate(NewNeighborhood = as.numeric(as.factor(unlist(df[[13]])))) %>% 
  filter(TotalSF < 6500) %>% 
  filter(LotArea < 20000) %>% 
  filter(GarageCars < 4)

new_df$NeighborhoodNew [!new_df$Neighborhood %in% c("BrDale", "BrkSide", "Edwards", "IDOTRR", "MeadowV", "NAmes", "OldTown", "Sawyer", "SWISU")] <- 1
new_df$NeighborhoodNew [new_df$Neighborhood %in% c("BrDale", "BrkSide", "Edwards", "IDOTRR", "MeadowV", "NAmes", "OldTown", "Sawyer", "SWISU")] <- 0

new_df$Quality [new_df$OverallQual %in% c(7,8,9,10)] <- 1
new_df$Quality [!new_df$OverallQual %in% c(7,8,9,10)] <- 0

new_df$Bldg_Type [!new_df$BldgType %in% c("Duplex", "Twnhs")] <- 1
new_df$Bldg_Type [new_df$BldgType %in% c("Duplex", "Twnhs")] <- 0

n <- nrow(new_df)
keep <- sample(1:n,(n*.7))
my_train <- new_df[keep, ]
my_test <- new_df[-keep, ]
```



```r
sf.lm <- lm(SalePrice ~ TotalSF, data = new_df)
summary(sf.lm)
```

```
## 
## Call:
## lm(formula = SalePrice ~ TotalSF, data = new_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -182794  -21275     509   22018  256371 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -29488.684   3987.889  -7.395 2.43e-13 ***
## TotalSF         81.972      1.513  54.166  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 42210 on 1400 degrees of freedom
## Multiple R-squared:  0.677,	Adjusted R-squared:  0.6767 
## F-statistic:  2934 on 1 and 1400 DF,  p-value: < 2.2e-16
```

```r
plot(SalePrice ~ TotalSF, data = new_df, col = "grey", pch = 16)
b <- coef(sf.lm)

curve(b[1] + b[2]*x, add = TRUE, col = "firebrick")
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

<br>

It looks as if the the data here can be transformed. If I run a boxCox on my lm I find that lambda is around 0.25. If I rerun my plot from above, I get a nice smooth line and the model has improved somewhat with an adjusted r-squared value of 0.684.

<br>


```r
boxCox(sf.lm)
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
sf1.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df)
summary(sf1.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8497 -0.5563  0.1179  0.7343  4.1009 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 1.464e+01  1.058e-01  138.34   <2e-16 ***
## TotalSF     2.212e-03  4.017e-05   55.08   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.12 on 1400 degrees of freedom
## Multiple R-squared:  0.6842,	Adjusted R-squared:  0.684 
## F-statistic:  3034 on 1 and 1400 DF,  p-value: < 2.2e-16
```

```r
plot(sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df, col = "grey", pch = 16)
b <- coef(sf1.lm)

curve(b[1] + b[2]*x, add = TRUE, col = "firebrick")
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

<br>

Following this, I then was curious to see if any other column could be used to show groups in my previous plot. I ran neighborhood in my lm and had 25 additional estimates, because the Neighbor column is qualitative. I grouped those that were significant and those that weren't. I created a new column and gave the significant values 1's and the non-significant values 0's. The following plot and summary show neighborhood included in my original linear regression of TotalSF predicting SalePrice. I also did a similar look at OverallQual and grouped those that were 7-10 (because they were significant) and those below as separate 1's and 0's.

<br>


```r
sf2.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew, data = new_df)
summary(sf2.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew, 
##     data = new_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.4268 -0.4926  0.0833  0.5810  3.6033 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.564e+01  9.667e-02   161.8   <2e-16 ***
## TotalSF                 1.489e-03  4.419e-05    33.7   <2e-16 ***
## TotalSF:NeighborhoodNew 5.466e-04  2.186e-05    25.0   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9318 on 1399 degrees of freedom
## Multiple R-squared:  0.7818,	Adjusted R-squared:  0.7814 
## F-statistic:  2506 on 2 and 1399 DF,  p-value: < 2.2e-16
```

```r
#pander(summary(sf2.lm))
b <- coef(sf2.lm)

plot(sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df, col = as.factor(NeighborhoodNew), pch = 16)
curve(b[1] + b[2]*x, col="black", lwd=2, add=TRUE)
curve(b[1] + (b[2] + b[3])*x, col="red", lwd=2, add=TRUE)
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
sf3.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:Quality, data = new_df)
summary(sf3.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:Quality, 
##     data = new_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.6832 -0.5135  0.1014  0.6517  3.4788 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     1.589e+01  1.092e-01  145.53   <2e-16 ***
## TotalSF         1.493e-03  4.864e-05   30.69   <2e-16 ***
## TotalSF:Quality 4.989e-04  2.347e-05   21.25   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9745 on 1399 degrees of freedom
## Multiple R-squared:  0.7613,	Adjusted R-squared:  0.761 
## F-statistic:  2231 on 2 and 1399 DF,  p-value: < 2.2e-16
```

```r
#pander(summary(sf3.lm))
b <- coef(sf3.lm)

plot(sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df, col = as.factor(Quality), pch = 16)
curve(b[1] + b[2]*x, col="black", lwd=2, add=TRUE)
curve(b[1] + (b[2] + b[3])*x, col="red", lwd=2, add=TRUE)
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

<br>


```r
# dropped Quality and NeighborhoodNew
sf4.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
summary(sf4.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + 
##     TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7616 -0.4404  0.0697  0.5791  3.1344 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.605e+01  1.253e-01 128.166  < 2e-16 ***
## TotalSF                 1.283e-03  5.821e-05  22.039  < 2e-16 ***
## TotalSF:NeighborhoodNew 4.116e-04  2.892e-05  14.231  < 2e-16 ***
## TotalSF:Quality         2.055e-04  4.445e-05   4.624 4.27e-06 ***
## Quality:NewNeighborhood 2.004e-02  6.982e-03   2.870  0.00419 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9002 on 976 degrees of freedom
## Multiple R-squared:    0.8,	Adjusted R-squared:  0.7992 
## F-statistic: 976.1 on 4 and 976 DF,  p-value: < 2.2e-16
```

```r
#pander(summary(sf4.lm))
b <- coef(sf4.lm)

plot(sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df, col = interaction(NeighborhoodNew, Quality), pch = 16)

ColNeither <- "black"
ColNeighborhoodOn <- "red"
ColQualityOn <- "green3"
ColBothOn <- "blue"

NeighborhoodOn <- 0
QualityOn <- 0

curve(b[1] +
        (b[2]+b[3]*NeighborhoodOn+b[4]*QualityOn)*x, 
        add = TRUE, 
        col = ColNeither)

NeighborhoodOn <- 1
QualityOn <- 0

curve(b[1] +
        (b[2]+b[3]*NeighborhoodOn+b[4]*QualityOn)*x, 
        add = TRUE, 
        col = ColNeighborhoodOn)

NeighborhoodOn <- 0
QualityOn <- 1

curve(b[1] +
        (b[2]+b[3]*NeighborhoodOn+b[4]*QualityOn)*x, 
        add = TRUE, 
        col = ColQualityOn)

NeighborhoodOn <- 1
QualityOn <- 1

curve(b[1] +
        (b[2]+b[3]*NeighborhoodOn+b[4]*QualityOn)*x, 
        add = TRUE, 
        col = ColBothOn)
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


##Model Validation & Interpretation

<br>


```r
# Compute R-squared for each validation
# Get y-hat for each model on new data.
finallm <- sf4.lm

finallm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality, data = my_test)

yht <- predict(finallm, newdata=my_test)
# Compute y-bar
Ynew <- my_test$SalePrice
ybar <- mean(Ynew) #Yi is given by Ynew
  
# Compute SSTO
SSTO <- sum( (Ynew - ybar)^2 )
  
# Compute SSE for each model
SSE <- sum( (Ynew - yht)^2 )
  
# Compute R-squared for each
rs <- 1 - SSE/SSTO
  
# Compute adjusted R-squared for each
n <- length(Ynew)
pt <- length(coef(finallm))
rsa <- 1 - (n-1)/(n-pt)*SSE/SSTO
```


| Model   | Adjusted $R^2$ Train Data | Adjusted $R^2$ Test Data |
|---------|-------|----------------| 
| Final Model    | 0.8000115  | 0.8172982 |

<br>


```r
b
```

```
##             (Intercept)                 TotalSF TotalSF:NeighborhoodNew 
##            1.605453e+01            1.282874e-03            4.115751e-04 
##         TotalSF:Quality Quality:NewNeighborhood 
##            2.055123e-04            2.004193e-02
```

<br>

I got a pretty good adjusted r-squared value of around 0.80. It is a simple model overall, whichh makes it pretty simple to interpret. I have four lines and a short explanation of each found below:

(0,0) black line: This is where neighborhood and quality are turned off. This is my base line where totalSF interprets my SalePrice. 

(1,0) red line: Neighborhood is turned on here. This looks at the interaction between totalSF and Neighborhood, which is a 0,1 variable. The intercept was not significant, but the slope was. We can see a difference in the prices of homes where neighborhood is accounted for.

(0,1) green line: Quality is turned on here, with neighborhood switched off. We don't notice as much of a difference here, but it is significant. The interaction between quality and totalSF impacts the base line. We can see that there SalePrice increases when Quality is higher.

(1,1) blue line: This last line looks at when both neighborhood and quality are accounted for. We can see that this is the highest group in my graph. This shows that when they live in a nice neighborhood and have a nice home, then there SalePrice will be higher than the other groups.

I learned a lot from this analysis. There are many factors that influence SalePrice. I chose to keep it relatively simple, but sometimes those models are the best as they are more easily interpreted.



```r
r.lm <- rlm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
summary(sf4.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + 
##     TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7616 -0.4404  0.0697  0.5791  3.1344 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.605e+01  1.253e-01 128.166  < 2e-16 ***
## TotalSF                 1.283e-03  5.821e-05  22.039  < 2e-16 ***
## TotalSF:NeighborhoodNew 4.116e-04  2.892e-05  14.231  < 2e-16 ***
## TotalSF:Quality         2.055e-04  4.445e-05   4.624 4.27e-06 ***
## Quality:NewNeighborhood 2.004e-02  6.982e-03   2.870  0.00419 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9002 on 976 degrees of freedom
## Multiple R-squared:    0.8,	Adjusted R-squared:  0.7992 
## F-statistic: 976.1 on 4 and 976 DF,  p-value: < 2.2e-16
```

```r
summary(r.lm)
```

```
## 
## Call: rlm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + 
##     TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -4.7766027 -0.4785588  0.0009889  0.5186092  3.1096978 
## 
## Coefficients:
##                         Value    Std. Error t value 
## (Intercept)              16.1799   0.1079   149.9216
## TotalSF                   0.0013   0.0001    25.3070
## TotalSF:NeighborhoodNew   0.0004   0.0000    16.1317
## TotalSF:Quality           0.0002   0.0000     4.5662
## Quality:NewNeighborhood   0.0224   0.0060     3.7296
## 
## Residual standard error: 0.7427 on 976 degrees of freedom
```

```r
p.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality, data = my_train[-479,])

par(mfrow=c(2,2), mai=c(.5,.5,.5,.1))
plot(p.lm, which=c(1,4))
plot(r.lm, which=c(1,4))
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
summary(p.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + 
##     TotalSF:Quality + NewNeighborhood:Quality, data = my_train[-479, 
##     ])
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7656 -0.4413  0.0689  0.5803  3.1283 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.606e+01  1.253e-01 128.145  < 2e-16 ***
## TotalSF                 1.282e-03  5.823e-05  22.016  < 2e-16 ***
## TotalSF:NeighborhoodNew 4.118e-04  2.892e-05  14.237  < 2e-16 ***
## TotalSF:Quality         2.042e-04  4.447e-05   4.592 4.96e-06 ***
## Quality:NewNeighborhood 2.049e-02  7.002e-03   2.926  0.00351 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9003 on 975 degrees of freedom
## Multiple R-squared:  0.8002,	Adjusted R-squared:  0.7993 
## F-statistic:   976 on 4 and 975 DF,  p-value: < 2.2e-16
```

```r
summary(sf4.lm)
```

```
## 
## Call:
## lm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + 
##     TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.7616 -0.4404  0.0697  0.5791  3.1344 
## 
## Coefficients:
##                          Estimate Std. Error t value Pr(>|t|)    
## (Intercept)             1.605e+01  1.253e-01 128.166  < 2e-16 ***
## TotalSF                 1.283e-03  5.821e-05  22.039  < 2e-16 ***
## TotalSF:NeighborhoodNew 4.116e-04  2.892e-05  14.231  < 2e-16 ***
## TotalSF:Quality         2.055e-04  4.445e-05   4.624 4.27e-06 ***
## Quality:NewNeighborhood 2.004e-02  6.982e-03   2.870  0.00419 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9002 on 976 degrees of freedom
## Multiple R-squared:    0.8,	Adjusted R-squared:  0.7992 
## F-statistic: 976.1 on 4 and 976 DF,  p-value: < 2.2e-16
```

##Diagonostic Plots


```r
par(mfrow=c(1,3))
plot(sf4.lm, which=c(1,4,5))
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

<br>

There's some definite errors here. The points that stand out to me are 1178, 1131, and 697. I could try removing them or doing a robust linear regression to see where it ends.

<br>
