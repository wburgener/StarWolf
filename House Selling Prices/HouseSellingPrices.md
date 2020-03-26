---
title: "House Selling Prices"
author: "Wesley Burgener"
date: "11/23/2019"
output:
  html_document:  
    keep_md: true
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: 'center'
---



<br>

# {.tabset .tabset-pills .tabset-fade}

##Background & Analysis

<br>

For this analysis, we were given a dataset from a Kaggle competition. We were tasked to find a model to predict the sales prices of homes. When I first dived into the dataset, I gained understanding of the data by checking out the parirs plot and text description. I began by first making a rather complex model. I soon realized interpretation might be difficult, so I took a step back and removed all of my interaction and quadratic terms to make it easier to interpret. Let's dive into the play by play of the process I went through to develop my model.

<br>

I began thinking about what matters to people when they buy a home. I thought of the size of the home and accompanying land, the nieghborhood, the numbers of bedrooms and baths, etc. I first combined the total square footage of the first, second, and basement. I named this new column TotalSF. Here's the plot and the summary of the regression when I run TotalSF as the explanatory variable for SalesPrice.

<br>


```r
df <- read.csv(file = "train.csv")
df_1 <- read.csv(file = "test.csv")

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
pander(summary(sf.lm))
```


---------------------------------------------------------------
     &nbsp;        Estimate   Std. Error   t value   Pr(>|t|)  
----------------- ---------- ------------ --------- -----------
 **(Intercept)**    -29489       3988      -7.395    2.433e-13 

   **TotalSF**      81.97       1.513       54.17        0     
---------------------------------------------------------------


-------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$   Adjusted $R^2$ 
-------------- --------------------- ------- ----------------
     1402              42212          0.677       0.6767     
-------------------------------------------------------------

Table: Fitting linear model: SalePrice ~ TotalSF

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
pander(summary(sf1.lm))
```


--------------------------------------------------------------
     &nbsp;        Estimate   Std. Error   t value   Pr(>|t|) 
----------------- ---------- ------------ --------- ----------
 **(Intercept)**    14.64       0.1058      138.3       0     

   **TotalSF**     0.002212   4.017e-05     55.08       0     
--------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     1402              1.12           0.6842       0.684      
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF

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
pander(summary(sf2.lm))
```


-----------------------------------------------------------------------------
           &nbsp;              Estimate    Std. Error   t value    Pr(>|t|)  
----------------------------- ----------- ------------ --------- ------------
       **(Intercept)**           15.64      0.09667      161.8        0      

         **TotalSF**           0.001489    4.419e-05     33.7     9.202e-183 

 **TotalSF:NeighborhoodNew**   0.0005466   2.186e-05      25      2.365e-114 
-----------------------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     1402             0.9318          0.7818       0.7814     
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew

```r
b <- coef(sf2.lm)

plot(sqrt(sqrt(SalePrice)) ~ TotalSF, data = new_df, col = as.factor(NeighborhoodNew), pch = 16)
curve(b[1] + b[2]*x, col="black", lwd=2, add=TRUE)
curve(b[1] + (b[2] + b[3])*x, col="red", lwd=2, add=TRUE)
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-5-1.png)<!-- -->



```r
sf3.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:Quality, data = new_df)
pander(summary(sf3.lm))
```


---------------------------------------------------------------------
       &nbsp;          Estimate    Std. Error   t value    Pr(>|t|)  
--------------------- ----------- ------------ --------- ------------
   **(Intercept)**       15.89       0.1092      145.5        0      

     **TotalSF**       0.001493    4.864e-05     30.69    1.297e-158 

 **TotalSF:Quality**   0.0004989   2.347e-05     21.25    4.337e-87  
---------------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     1402             0.9745          0.7613       0.761      
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:Quality

```r
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
pander(summary(sf4.lm))
```


----------------------------------------------------------------------------
           &nbsp;              Estimate    Std. Error   t value   Pr(>|t|)  
----------------------------- ----------- ------------ --------- -----------
       **(Intercept)**           16.11       0.1226      131.4        0     

         **TotalSF**           0.001257    5.706e-05     22.03    1.175e-87 

 **TotalSF:NeighborhoodNew**   0.0003924   2.822e-05     13.91    3.054e-40 

     **TotalSF:Quality**       0.0002165   4.375e-05     4.949    8.784e-07 

 **Quality:NewNeighborhood**    0.01684     0.006856     2.456     0.0142   
----------------------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     981              0.8884          0.8004       0.7996     
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality

```r
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
| Final Model    | 0.8004209  | 0.8167533 |

<br>


```r
pander(b)
```


--------------------------------------------------------------------
 (Intercept)   TotalSF    TotalSF:NeighborhoodNew   TotalSF:Quality 
------------- ---------- ------------------------- -----------------
    16.11      0.001257          0.0003924             0.0002165    
--------------------------------------------------------------------

Table: Table continues below

 
-------------------------
 Quality:NewNeighborhood 
-------------------------
         0.01684         
-------------------------

<br>

I got a pretty good adjusted r-squared value of around 0.80. It is a simple model overall, whichh makes it pretty simple to interpret. I have four lines and a short explanation of each found below:

(0,0) black line: This is where neighborhood and quality are turned off. This is my base line where totalSF interprets my SalePrice. 

(1,0) red line: Neighborhood is turned on here. This looks at the interaction between totalSF and Neighborhood, which is a 0,1 variable. The intercept was not significant, but the slope was. We can see a difference in the prices of homes where neighborhood is accounted for.

(0,1) green line: Quality is turned on here, with neighborhood switched off. We don't notice as much of a difference here, but it is significant. The interaction between quality and totalSF impacts the base line. We can see that there SalePrice increases when Quality is higher.

(1,1) blue line: This last line looks at when both neighborhood and quality are accounted for. We can see that this is the highest group in my graph. This shows that when they live in a nice neighborhood and have a nice home, then there SalePrice will be higher than the other groups.

I learned a lot from this analysis. There are many factors that influence SalePrice. I chose to keep it relatively simple, but sometimes those models are the best as they are more easily interpreted.



```r
r.lm <- rlm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
pander(summary(sf4.lm))
```


----------------------------------------------------------------------------
           &nbsp;              Estimate    Std. Error   t value   Pr(>|t|)  
----------------------------- ----------- ------------ --------- -----------
       **(Intercept)**           16.11       0.1226      131.4        0     

         **TotalSF**           0.001257    5.706e-05     22.03    1.175e-87 

 **TotalSF:NeighborhoodNew**   0.0003924   2.822e-05     13.91    3.054e-40 

     **TotalSF:Quality**       0.0002165   4.375e-05     4.949    8.784e-07 

 **Quality:NewNeighborhood**    0.01684     0.006856     2.456     0.0142   
----------------------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     981              0.8884          0.8004       0.7996     
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality

```r
summary(r.lm)
```

```
## 
## Call: rlm(formula = sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + 
##     TotalSF:Quality + NewNeighborhood:Quality, data = my_train)
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -4.508997 -0.496997  0.001904  0.519632  3.255061 
## 
## Coefficients:
##                         Value    Std. Error t value 
## (Intercept)              16.2337   0.1103   147.1688
## TotalSF                   0.0012   0.0001    23.9414
## TotalSF:NeighborhoodNew   0.0004   0.0000    15.3993
## TotalSF:Quality           0.0002   0.0000     5.3491
## Quality:NewNeighborhood   0.0153   0.0062     2.4743
## 
## Residual standard error: 0.761 on 976 degrees of freedom
```

```r
p.lm <- lm(sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality, data = my_train[-479,])

par(mfrow=c(2,2), mai=c(.5,.5,.5,.1))
plot(p.lm, which=c(1,4))
plot(r.lm, which=c(1,4))
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
pander(summary(p.lm))
```


----------------------------------------------------------------------------
           &nbsp;              Estimate    Std. Error   t value   Pr(>|t|)  
----------------------------- ----------- ------------ --------- -----------
       **(Intercept)**           16.09       0.1228      131.1        0     

         **TotalSF**           0.001263    5.717e-05     22.1     4.746e-88 

 **TotalSF:NeighborhoodNew**   0.000395    2.826e-05     13.98    1.278e-40 

     **TotalSF:Quality**       0.0002113   4.386e-05     4.819    1.674e-06 

 **Quality:NewNeighborhood**    0.01698     0.006852     2.479     0.01336  
----------------------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     980              0.8879          0.8009        0.8       
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality

```r
pander(summary(sf4.lm))
```


----------------------------------------------------------------------------
           &nbsp;              Estimate    Std. Error   t value   Pr(>|t|)  
----------------------------- ----------- ------------ --------- -----------
       **(Intercept)**           16.11       0.1226      131.4        0     

         **TotalSF**           0.001257    5.706e-05     22.03    1.175e-87 

 **TotalSF:NeighborhoodNew**   0.0003924   2.822e-05     13.91    3.054e-40 

     **TotalSF:Quality**       0.0002165   4.375e-05     4.949    8.784e-07 

 **Quality:NewNeighborhood**    0.01684     0.006856     2.456     0.0142   
----------------------------------------------------------------------------


--------------------------------------------------------------
 Observations   Residual Std. Error   $R^2$    Adjusted $R^2$ 
-------------- --------------------- -------- ----------------
     981              0.8884          0.8004       0.7996     
--------------------------------------------------------------

Table: Fitting linear model: sqrt(sqrt(SalePrice)) ~ TotalSF + TotalSF:NeighborhoodNew + TotalSF:Quality + NewNeighborhood:Quality

##Diagonostic Plots


```r
par(mfrow=c(1,3))
plot(sf4.lm, which=c(1,4,5))
```

![](HouseSellingPrices_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

<br>

There's some definite errors here. The points that stand out to me are 1178, 1131, and 697. I could try removing them or doing a robust linear regression to see where it ends.

<br>