Beta regression comparing different link function (Exlude London)
================

# read data

``` r
fca<- read.csv("accessibility_imd_ethnic_exclude.csv", 
                         sep=",")
```

# Import beta regression library

``` r
library(betareg)
```

    ## Warning: package 'betareg' was built under R version 4.0.5

# 2SFCA/15 miles with different link function

## log-log

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
    ## -14.5052  -0.4786  -0.0756   0.4293   7.4742 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4245465  0.0217114  65.613  < 2e-16 ***
    ## X2sfca_15_normalized  0.3279227  0.0902879   3.632 0.000281 ***
    ## IMD19.SCORE          -0.0067540  0.0005607 -12.046  < 2e-16 ***
    ## Mixed.                6.2817239  0.8892951   7.064 1.62e-12 ***
    ## Asian.               -0.2312543  0.0804847  -2.873 0.004062 ** 
    ## Black.               -2.3830606  0.3420643  -6.967 3.24e-12 ***
    ## Other.               -2.5872962  0.9147228  -2.829 0.004677 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6669     0.2302   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4798 on 8 Df
    ## Pseudo R-squared: 0.07924
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

## logit

``` r
scfa2_15 = betareg(vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "logit")
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "logit")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.5023  -0.4776  -0.0757   0.4286   7.4723 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.3018956  0.0246255  52.868  < 2e-16 ***
    ## X2sfca_15_normalized  0.3664534  0.1013110   3.617 0.000298 ***
    ## IMD19.SCORE          -0.0077137  0.0006448 -11.962  < 2e-16 ***
    ## Mixed.                7.4205818  1.0274472   7.222 5.11e-13 ***
    ## Asian.               -0.2584503  0.0956499  -2.702 0.006891 ** 
    ## Black.               -2.9104800  0.4180455  -6.962 3.35e-12 ***
    ## Other.               -3.0936737  1.1087805  -2.790 0.005268 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6729     0.2303   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4799 on 8 Df
    ## Pseudo R-squared: 0.046
    ## Number of iterations: 14 (BFGS) + 3 (Fisher scoring)

## probit

``` r
scfa2_15 = betareg(vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "probit")
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "probit")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.5022  -0.4771  -0.0749   0.4291   7.4716 
    ## 
    ## Coefficients (mean model with probit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.7938780  0.0142827  55.583  < 2e-16 ***
    ## X2sfca_15_normalized  0.2088227  0.0581578   3.591  0.00033 ***
    ## IMD19.SCORE          -0.0045150  0.0003792 -11.908  < 2e-16 ***
    ## Mixed.                4.4011944  0.6019139   7.312 2.63e-13 ***
    ## Asian.               -0.1498942  0.0573886  -2.612  0.00900 ** 
    ## Black.               -1.7589568  0.2518386  -6.984 2.86e-12 ***
    ## Other.               -1.8562405  0.6683252  -2.777  0.00548 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6766     0.2303   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4799 on 8 Df
    ## Pseudo R-squared: 0.06255
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## cloglog

``` r
scfa2_15 = betareg(vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "cloglog")
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "cloglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.5003  -0.4778  -0.0753   0.4284   7.4692 
    ## 
    ## Coefficients (mean model with cloglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.4346288  0.0127396  34.116  < 2e-16 ***
    ## X2sfca_15_normalized  0.1813369  0.0509616   3.558 0.000373 ***
    ## IMD19.SCORE          -0.0040793  0.0003461 -11.787  < 2e-16 ***
    ## Mixed.                4.1183475  0.5497335   7.492 6.81e-14 ***
    ## Asian.               -0.1316918  0.0546055  -2.412 0.015879 *  
    ## Black.               -1.7164881  0.2454967  -6.992 2.71e-12 ***
    ## Other.               -1.7724779  0.6466925  -2.741 0.006128 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6837     0.2305   55.03   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4801 on 8 Df
    ## Pseudo R-squared: 0.01956
    ## Number of iterations: 21 (BFGS) + 4 (Fisher scoring)

## cauchit

``` r
scfa2_15 = betareg(vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "cauchit")
```

    ## Warning in betareg.fit(X, Y, Z, weights, offset, link, link.phi, type, control):
    ## no valid starting value for precision parameter found, using 1 instead

``` r
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "cauchit")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.5071  -0.4765  -0.0728   0.4278   7.4767 
    ## 
    ## Coefficients (mean model with cauchit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.2458374  0.0322064  38.683  < 2e-16 ***
    ## X2sfca_15_normalized  0.5405738  0.1414036   3.823 0.000132 ***
    ## IMD19.SCORE          -0.0094615  0.0007699 -12.289  < 2e-16 ***
    ## Mixed.                8.1500834  1.2558881   6.489 8.61e-11 ***
    ## Asian.               -0.3308555  0.0987147  -3.352 0.000803 ***
    ## Black.               -2.8227430  0.4204576  -6.714 1.90e-11 ***
    ## Other.               -3.1572204  1.0940131  -2.886 0.003903 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6412     0.2297   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4793 on 8 Df
    ## Pseudo R-squared: 0.0008054
    ## Number of iterations: 51 (BFGS) + 3 (Fisher scoring)

## log

``` r
scfa2_15 = betareg(vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "log")
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "log")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.5023  -0.4791  -0.0737   0.4301   7.4665 
    ## 
    ## Coefficients (mean model with log link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -0.2388894  0.0054288 -44.004  < 2e-16 ***
    ## X2sfca_15_normalized  0.0712123  0.0207107   3.438 0.000585 ***
    ## IMD19.SCORE          -0.0018029  0.0001566 -11.516  < 2e-16 ***
    ## Mixed.                1.9033772  0.2454312   7.755 8.82e-15 ***
    ## Asian.               -0.0546404  0.0267843  -2.040 0.041349 *  
    ## Black.               -0.8615629  0.1235767  -6.972 3.13e-12 ***
    ## Other.               -0.8666202  0.3217163  -2.694 0.007066 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6936     0.2307   55.03   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4803 on 8 Df
    ## Pseudo R-squared: 0.002923
    ## Number of iterations: 35 (BFGS) + 4 (Fisher scoring)
