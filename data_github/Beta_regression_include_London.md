Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition
================

# read data

``` r
fca<- read.csv("accessibility_imd_ethnic.csv", 
                         sep=",")
```

# Import beta regression library

``` r
library(betareg)
```

    ## Warning: package 'betareg' was built under R version 4.0.5

# 2SFCA

## 10 miles

``` r
scfa2_10 = betareg(vaccination_rate_no_zero ~ X2sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca,  link = "loglog")
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2764  -0.4800  -0.0724   0.4375   7.3553 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5232392  0.0159284  95.630  < 2e-16 ***
    ## X2sfca_10_normalized -0.2910658  0.0474206  -6.138 8.36e-10 ***
    ## IMD19.SCORE          -0.0080484  0.0005339 -15.074  < 2e-16 ***
    ## Mixed.                4.8688831  0.6948848   7.007 2.44e-12 ***
    ## Asian.               -0.1294308  0.0617536  -2.096 0.036089 *  
    ## Black.               -1.0796949  0.1673414  -6.452 1.10e-10 ***
    ## Other.               -2.0155646  0.5676553  -3.551 0.000384 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2993     0.2064   59.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5467 on 8 Df
    ## Pseudo R-squared: 0.09647
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 15 miles

``` r
scfa2_15 = betareg(vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2611  -0.4808  -0.0680   0.4380   7.4468 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5145828  0.0163444  92.667  < 2e-16 ***
    ## X2sfca_15_normalized -0.0994424  0.0435451  -2.284   0.0224 *  
    ## IMD19.SCORE          -0.0076290  0.0005485 -13.910  < 2e-16 ***
    ## Mixed.                4.2077626  0.6958070   6.047 1.47e-09 ***
    ## Asian.               -0.0893631  0.0618008  -1.446   0.1482    
    ## Black.               -1.2135415  0.1703894  -7.122 1.06e-12 ***
    ## Other.               -2.7346791  0.5587073  -4.895 9.85e-07 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2427     0.2054   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5452 on 8 Df
    ## Pseudo R-squared: 0.09194
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_rate_no_zero ~ X2sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")

summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2794  -0.4772  -0.0676   0.4373   7.4270 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5058880  0.0166506  90.440  < 2e-16 ***
    ## X2sfca_20_normalized -0.0161609  0.0373873  -0.432    0.666    
    ## IMD19.SCORE          -0.0072957  0.0005552 -13.140  < 2e-16 ***
    ## Mixed.                3.8737976  0.6952846   5.572 2.53e-08 ***
    ## Asian.               -0.0886344  0.0619833  -1.430    0.153    
    ## Black.               -1.3103177  0.1696483  -7.724 1.13e-14 ***
    ## Other.               -3.0480090  0.5475770  -5.566 2.60e-08 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2331     0.2053   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5450 on 8 Df
    ## Pseudo R-squared: 0.09135
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 25 miles

``` r
scfa2_25 = betareg(vaccination_rate_no_zero ~ X2sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2833  -0.4768  -0.0662   0.4375   7.4295 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5041547  0.0169211  88.892  < 2e-16 ***
    ## X2sfca_25_normalized -0.0040342  0.0332203  -0.121    0.903    
    ## IMD19.SCORE          -0.0072334  0.0005582 -12.959  < 2e-16 ***
    ## Mixed.                3.8160013  0.6925930   5.510 3.59e-08 ***
    ## Asian.               -0.0898709  0.0621220  -1.447    0.148    
    ## Black.               -1.3257656  0.1690447  -7.843 4.41e-15 ***
    ## Other.               -3.0924414  0.5417543  -5.708 1.14e-08 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2327     0.2053   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5449 on 8 Df
    ## Pseudo R-squared: 0.09136
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

## 30 miles

``` r
scfa2_30 = betareg(vaccination_rate_no_zero ~ X2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2734  -0.4765  -0.0682   0.4360   7.4230 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5084531  0.0172099  87.650  < 2e-16 ***
    ## X2sfca_30_normalized -0.0218867  0.0311311  -0.703    0.482    
    ## IMD19.SCORE          -0.0073611  0.0005612 -13.117  < 2e-16 ***
    ## Mixed.                3.9135263  0.6918511   5.657 1.54e-08 ***
    ## Asian.               -0.0857576  0.0621978  -1.379    0.168    
    ## Black.               -1.3014258  0.1682392  -7.736 1.03e-14 ***
    ## Other.               -3.0365831  0.5383586  -5.640 1.70e-08 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2337     0.2053   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5450 on 8 Df
    ## Pseudo R-squared: 0.09148
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_rate_no_zero ~ E2sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3143  -0.4778  -0.0718   0.4362   7.3757 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5214454  0.0157069  96.865  < 2e-16 ***
    ## E2sfca_10_normalized -0.5379106  0.0592185  -9.083  < 2e-16 ***
    ## IMD19.SCORE          -0.0080333  0.0005238 -15.337  < 2e-16 ***
    ## Mixed.                5.2148588  0.6911532   7.545 4.52e-14 ***
    ## Asian.               -0.1557413  0.0615529  -2.530  0.01140 *  
    ## Black.               -1.1087727  0.1635030  -6.781 1.19e-11 ***
    ## Other.               -1.5704145  0.5648614  -2.780  0.00543 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.3709     0.2077   59.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5486 on 8 Df
    ## Pseudo R-squared: 0.1021
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_rate_no_zero ~ E2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2867  -0.4791  -0.0710   0.4381   7.3907 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5242172  0.0158289  96.293  < 2e-16 ***
    ## E2sfca_15_normalized -0.4126573  0.0541351  -7.623 2.48e-14 ***
    ## IMD19.SCORE          -0.0081158  0.0005295 -15.328  < 2e-16 ***
    ## Mixed.                5.1368808  0.6960101   7.380 1.58e-13 ***
    ## Asian.               -0.1396643  0.0616334  -2.266  0.02345 *  
    ## Black.               -1.0724343  0.1654301  -6.483 9.01e-11 ***
    ## Other.               -1.7486269  0.5686790  -3.075  0.00211 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   12.332      0.207   59.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5475 on 8 Df
    ## Pseudo R-squared: 0.09897
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_rate_no_zero ~ E2sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2618  -0.4793  -0.0724   0.4363   7.4125 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5243392  0.0159855  95.358  < 2e-16 ***
    ## E2sfca_20_normalized -0.3084330  0.0515822  -5.979 2.24e-09 ***
    ## IMD19.SCORE          -0.0080714  0.0005359 -15.061  < 2e-16 ***
    ## Mixed.                4.9178960  0.6989346   7.036 1.97e-12 ***
    ## Asian.               -0.1171826  0.0616624  -1.900 0.057382 .  
    ## Black.               -1.0857629  0.1672457  -6.492 8.47e-11 ***
    ## Other.               -2.0328089  0.5686571  -3.575 0.000351 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2953     0.2064   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5466 on 8 Df
    ## Pseudo R-squared: 0.09599
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_rate_no_zero ~ E2sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2558  -0.4801  -0.0703   0.4342   7.4205 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5225641  0.0161557  94.243  < 2e-16 ***
    ## E2sfca_25_normalized -0.2243331  0.0492368  -4.556 5.21e-06 ***
    ## IMD19.SCORE          -0.0079590  0.0005419 -14.686  < 2e-16 ***
    ## Mixed.                4.6800841  0.7003746   6.682 2.35e-11 ***
    ## Asian.               -0.1008105  0.0616931  -1.634    0.102    
    ## Black.               -1.1239132  0.1685450  -6.668 2.59e-11 ***
    ## Other.               -2.3060108  0.5661122  -4.073 4.63e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2700     0.2059   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5459 on 8 Df
    ## Pseudo R-squared: 0.09401
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_rate_no_zero ~ E2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2623  -0.4817  -0.0693   0.4346   7.4200 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.520130   0.016325  93.118  < 2e-16 ***
    ## E2sfca_30_normalized -0.163029   0.046919  -3.475 0.000511 ***
    ## IMD19.SCORE          -0.007835   0.000547 -14.322  < 2e-16 ***
    ## Mixed.                4.479275   0.700743   6.392 1.64e-10 ***
    ## Asian.               -0.091619   0.061739  -1.484 0.137818    
    ## Black.               -1.164694   0.169323  -6.879 6.05e-12 ***
    ## Other.               -2.520570   0.562383  -4.482 7.40e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2549     0.2057   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5455 on 8 Df
    ## Pseudo R-squared: 0.09289
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_rate_no_zero ~ X3sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X3sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2790  -0.4799  -0.0717   0.4389   7.3536 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5256063  0.0158392  96.318  < 2e-16 ***
    ## X3sfca_10_normalized -0.4487768  0.0567739  -7.905 2.69e-15 ***
    ## IMD19.SCORE          -0.0081132  0.0005286 -15.348  < 2e-16 ***
    ## Mixed.                5.0899664  0.6928449   7.346 2.04e-13 ***
    ## Asian.               -0.1374482  0.0615657  -2.233  0.02558 *  
    ## Black.               -1.1040193  0.1643450  -6.718 1.85e-11 ***
    ## Other.               -1.7358413  0.5663345  -3.065  0.00218 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.3385     0.2071   59.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5477 on 8 Df
    ## Pseudo R-squared: 0.09881
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## 15 miles

``` r
scfa3_15 = betareg(vaccination_rate_no_zero ~ X3sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X3sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3105  -0.4788  -0.0690   0.4351   7.3658 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5289345  0.0161166  94.867  < 2e-16 ***
    ## X3sfca_15_normalized -0.3239794  0.0515780  -6.281 3.36e-10 ***
    ## IMD19.SCORE          -0.0081754  0.0005386 -15.180  < 2e-16 ***
    ## Mixed.                4.9596102  0.6984092   7.101 1.24e-12 ***
    ## Asian.               -0.1143058  0.0616014  -1.856 0.063515 .  
    ## Black.               -1.0955991  0.1664293  -6.583 4.61e-11 ***
    ## Other.               -1.9951864  0.5673991  -3.516 0.000437 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.3014     0.2065   59.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5467 on 8 Df
    ## Pseudo R-squared: 0.09602
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## 20 miles

``` r
scfa3_20 = betareg(vaccination_rate_no_zero ~ X3sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X3sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3111  -0.4803  -0.0693   0.4343   7.3995 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.527934   0.016410  93.109  < 2e-16 ***
    ## X3sfca_20_normalized -0.228462   0.047533  -4.806 1.54e-06 ***
    ## IMD19.SCORE          -0.008080   0.000547 -14.773  < 2e-16 ***
    ## Mixed.                4.722625   0.700188   6.745 1.53e-11 ***
    ## Asian.               -0.098103   0.061663  -1.591    0.112    
    ## Black.               -1.120392   0.168185  -6.662 2.71e-11 ***
    ## Other.               -2.293081   0.563274  -4.071 4.68e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   12.274      0.206   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5460 on 8 Df
    ## Pseudo R-squared: 0.09427
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

## 25 miles

``` r
scfa3_25 = betareg(vaccination_rate_no_zero ~ X3sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X3sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3003  -0.4808  -0.0703   0.4364   7.4193 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5261548  0.0166568  91.624  < 2e-16 ***
    ## X3sfca_25_normalized -0.1742087  0.0447765  -3.891 1.00e-04 ***
    ## IMD19.SCORE          -0.0079789  0.0005527 -14.435  < 2e-16 ***
    ## Mixed.                4.5503819  0.7002575   6.498 8.13e-11 ***
    ## Asian.               -0.0897522  0.0617219  -1.454    0.146    
    ## Black.               -1.1480395  0.1691550  -6.787 1.15e-11 ***
    ## Other.               -2.4846357  0.5588650  -4.446 8.75e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2605     0.2057   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5457 on 8 Df
    ## Pseudo R-squared: 0.09344
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

## 30 miles

``` r
scfa3_30 = betareg(vaccination_rate_no_zero ~ X3sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "loglog")
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X3sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "loglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.2952  -0.4800  -0.0697   0.4349   7.4277 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5251876  0.0168595  90.464  < 2e-16 ***
    ## X3sfca_30_normalized -0.1463184  0.0429438  -3.407 0.000656 ***
    ## IMD19.SCORE          -0.0079191  0.0005567 -14.226  < 2e-16 ***
    ## Mixed.                4.4576500  0.7002263   6.366 1.94e-10 ***
    ## Asian.               -0.0855119  0.0617741  -1.384 0.166276    
    ## Black.               -1.1666269  0.1695542  -6.881 5.96e-12 ***
    ## Other.               -2.5898827  0.5554137  -4.663 3.12e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.2543     0.2056   59.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  5455 on 8 Df
    ## Pseudo R-squared: 0.0931
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)
