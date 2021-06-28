Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition
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

# 2SFCA

## 10 miles

``` r
scfa2_10 = betareg(vaccination_rate_reshape ~ X2sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3410 -0.6820 -0.1665  0.4721 13.5120 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4611557  0.0191768  76.194  < 2e-16 ***
    ## X2sfca_10_normalized  0.1219277  0.0467660   2.607 0.009129 ** 
    ## IMD19.SCORE          -0.0106130  0.0005273 -20.128  < 2e-16 ***
    ## Mixed.                9.2023876  0.8522829  10.797  < 2e-16 ***
    ## Asian.               -0.2711742  0.0778898  -3.482 0.000499 ***
    ## Black.               -3.3474968  0.3407736  -9.823  < 2e-16 ***
    ## Other.               -3.2260428  0.9052032  -3.564 0.000365 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2216     0.3704   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6121 on 8 Df
    ## Pseudo R-squared: 0.08129
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## 15 miles

``` r
scfa2_15 = betareg(vaccination_rate_reshape ~ X2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3404 -0.6788 -0.1670  0.4721 13.5672 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4626652  0.0205053  71.331  < 2e-16 ***
    ## X2sfca_15_normalized  0.1740423  0.0842902   2.065 0.038942 *  
    ## IMD19.SCORE          -0.0106212  0.0005305 -20.020  < 2e-16 ***
    ## Mixed.                9.2891535  0.8529563  10.891  < 2e-16 ***
    ## Asian.               -0.2733605  0.0779846  -3.505 0.000456 ***
    ## Black.               -3.3536047  0.3409374  -9.836  < 2e-16 ***
    ## Other.               -3.1694737  0.9055203  -3.500 0.000465 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2131     0.3703   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6120 on 8 Df
    ## Pseudo R-squared: 0.08096
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_rate_reshape ~ X2sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3391 -0.6782 -0.1683  0.4719 13.5238 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4768328  0.0190779  77.410  < 2e-16 ***
    ## X2sfca_20_normalized  0.0886151  0.0734724   1.206 0.227779    
    ## IMD19.SCORE          -0.0106860  0.0005324 -20.071  < 2e-16 ***
    ## Mixed.                9.3916979  0.8544363  10.992  < 2e-16 ***
    ## Asian.               -0.2702379  0.0779942  -3.465 0.000531 ***
    ## Black.               -3.3350415  0.3408125  -9.786  < 2e-16 ***
    ## Other.               -3.1727885  0.9060911  -3.502 0.000462 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2033     0.3701   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6119 on 8 Df
    ## Pseudo R-squared: 0.08076
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## 25 miles

``` r
scfa2_25 = betareg(vaccination_rate_reshape ~ X2sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3387 -0.6790 -0.1628  0.4714 13.4595 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4948799  0.0182136  82.075  < 2e-16 ***
    ## X2sfca_25_normalized -0.0296375  0.0551667  -0.537 0.591105    
    ## IMD19.SCORE          -0.0108725  0.0005348 -20.330  < 2e-16 ***
    ## Mixed.                9.6274404  0.8513811  11.308  < 2e-16 ***
    ## Asian.               -0.2645913  0.0780459  -3.390 0.000698 ***
    ## Black.               -3.3259929  0.3411508  -9.749  < 2e-16 ***
    ## Other.               -3.2093854  0.9066529  -3.540 0.000400 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)    20.20       0.37   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6118 on 8 Df
    ## Pseudo R-squared: 0.08062
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## 30 miles

``` r
scfa2_30 = betareg(vaccination_rate_reshape ~ X2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3414 -0.6755 -0.1636  0.4688 13.4015 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5110718  0.0180130  83.888  < 2e-16 ***
    ## X2sfca_30_normalized -0.1060280  0.0441833  -2.400 0.016407 *  
    ## IMD19.SCORE          -0.0111190  0.0005381 -20.665  < 2e-16 ***
    ## Mixed.                9.8292984  0.8514142  11.545  < 2e-16 ***
    ## Asian.               -0.2574522  0.0780487  -3.299 0.000972 ***
    ## Black.               -3.3014150  0.3410222  -9.681  < 2e-16 ***
    ## Other.               -3.2544783  0.9067858  -3.589 0.000332 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2178     0.3703   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6121 on 8 Df
    ## Pseudo R-squared: 0.0812
    ## Number of iterations: 15 (BFGS) + 3 (Fisher scoring)

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_rate_reshape ~ E2sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3394 -0.6829 -0.1630  0.4701 13.4866 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4776871  0.0172083  85.871  < 2e-16 ***
    ## E2sfca_10_normalized  0.0958844  0.0565348   1.696 0.089881 .  
    ## IMD19.SCORE          -0.0107460  0.0005228 -20.554  < 2e-16 ***
    ## Mixed.                9.3217111  0.8540443  10.915  < 2e-16 ***
    ## Asian.               -0.2702359  0.0779276  -3.468 0.000525 ***
    ## Black.               -3.3406425  0.3408212  -9.802  < 2e-16 ***
    ## Other.               -3.2170111  0.9056513  -3.552 0.000382 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2080     0.3702   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6119 on 8 Df
    ## Pseudo R-squared: 0.08094
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_rate_reshape ~ E2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3401 -0.6809 -0.1657  0.4696 13.5034 
    ## 
    ## Coefficients (mean model with logit link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.468823   0.018710  78.503  < 2e-16 ***
    ## E2sfca_15_normalized  0.097273   0.047090   2.066 0.038857 *  
    ## IMD19.SCORE          -0.010689   0.000525 -20.361  < 2e-16 ***
    ## Mixed.                9.206752   0.858355  10.726  < 2e-16 ***
    ## Asian.               -0.272640   0.077943  -3.498 0.000469 ***
    ## Black.               -3.341721   0.340811  -9.805  < 2e-16 ***
    ## Other.               -3.213488   0.905343  -3.549 0.000386 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2127     0.3703   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6120 on 8 Df
    ## Pseudo R-squared: 0.08105
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_rate_reshape ~ E2sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3407 -0.6803 -0.1686  0.4723 13.5450 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4607022  0.0199526  73.209  < 2e-16 ***
    ## E2sfca_20_normalized  0.1452916  0.0611250   2.377 0.017456 *  
    ## IMD19.SCORE          -0.0106329  0.0005274 -20.160  < 2e-16 ***
    ## Mixed.                9.1205811  0.8607139  10.597  < 2e-16 ***
    ## Asian.               -0.2751340  0.0779655  -3.529 0.000417 ***
    ## Black.               -3.3443777  0.3407954  -9.813  < 2e-16 ***
    ## Other.               -3.2020659  0.9050706  -3.538 0.000403 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2176     0.3703   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6121 on 8 Df
    ## Pseudo R-squared: 0.08115
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_rate_reshape ~ E2sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3406 -0.6796 -0.1678  0.4715 13.5688 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4600890  0.0204819  71.287  < 2e-16 ***
    ## E2sfca_25_normalized  0.1654356  0.0726443   2.277 0.022766 *  
    ## IMD19.SCORE          -0.0106098  0.0005299 -20.023  < 2e-16 ***
    ## Mixed.                9.1352425  0.8612203  10.607  < 2e-16 ***
    ## Asian.               -0.2757052  0.0779955  -3.535 0.000408 ***
    ## Black.               -3.3448213  0.3408230  -9.814  < 2e-16 ***
    ## Other.               -3.1880201  0.9051358  -3.522 0.000428 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2162     0.3703   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6120 on 8 Df
    ## Pseudo R-squared: 0.08108
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_rate_reshape ~ E2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3400 -0.6778 -0.1707  0.4701 13.5559 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4673850  0.0203958  71.945  < 2e-16 ***
    ## E2sfca_30_normalized  0.1325738  0.0761540   1.741 0.081707 .  
    ## IMD19.SCORE          -0.0106372  0.0005322 -19.987  < 2e-16 ***
    ## Mixed.                9.2493906  0.8602571  10.752  < 2e-16 ***
    ## Asian.               -0.2738398  0.0780281  -3.510 0.000449 ***
    ## Black.               -3.3439597  0.3409201  -9.809  < 2e-16 ***
    ## Other.               -3.1803486  0.9055310  -3.512 0.000445 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2088     0.3702   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6119 on 8 Df
    ## Pseudo R-squared: 0.08088
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_rate_reshape ~ X3sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3407 -0.6813 -0.1683  0.4714 13.5256 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4714603  0.0172727  85.190  < 2e-16 ***
    ## X3sfca_10_normalized  0.1403528  0.0564242   2.487 0.012866 *  
    ## IMD19.SCORE          -0.0106708  0.0005243 -20.353  < 2e-16 ***
    ## Mixed.                9.2405789  0.8518155  10.848  < 2e-16 ***
    ## Asian.               -0.2685933  0.0778698  -3.449 0.000562 ***
    ## Black.               -3.3372277  0.3406379  -9.797  < 2e-16 ***
    ## Other.               -3.1891350  0.9052142  -3.523 0.000427 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2199     0.3704   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6121 on 8 Df
    ## Pseudo R-squared: 0.08131
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 15 miles

``` r
scfa3_15 = betareg(vaccination_rate_reshape ~ X3sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3394 -0.6807 -0.1674  0.4711 13.5031 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4750403  0.0184122  80.112  < 2e-16 ***
    ## X3sfca_15_normalized  0.0780079  0.0509433   1.531 0.125703    
    ## IMD19.SCORE          -0.0106722  0.0005293 -20.163  < 2e-16 ***
    ## Mixed.                9.3388577  0.8547997  10.925  < 2e-16 ***
    ## Asian.               -0.2688373  0.0779267  -3.450 0.000561 ***
    ## Black.               -3.3356708  0.3407895  -9.788  < 2e-16 ***
    ## Other.               -3.1798706  0.9058075  -3.511 0.000447 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2061     0.3701   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6119 on 8 Df
    ## Pseudo R-squared: 0.08088
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

## 20 miles

``` r
scfa3_20 = betareg(vaccination_rate_reshape ~ X3sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3390 -0.6789 -0.1633  0.4708 13.4774 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4902708  0.0190742  78.130  < 2e-16 ***
    ## X3sfca_20_normalized -0.0018257  0.0563976  -0.032 0.974175    
    ## IMD19.SCORE          -0.0108145  0.0005339 -20.256  < 2e-16 ***
    ## Mixed.                9.5697864  0.8565787  11.172  < 2e-16 ***
    ## Asian.               -0.2664906  0.0779792  -3.417 0.000632 ***
    ## Black.               -3.3320466  0.3408801  -9.775  < 2e-16 ***
    ## Other.               -3.1979222  0.9065145  -3.528 0.000419 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)    20.20       0.37   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6118 on 8 Df
    ## Pseudo R-squared: 0.0806
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

## 25 miles

``` r
scfa3_25 = betareg(vaccination_rate_reshape ~ X3sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3401 -0.6764 -0.1630  0.4717 13.4450 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5058708  0.0194751  77.323  < 2e-16 ***
    ## X3sfca_25_normalized -0.0820087  0.0590904  -1.388 0.165183    
    ## IMD19.SCORE          -0.0109863  0.0005372 -20.450  < 2e-16 ***
    ## Mixed.                9.7771309  0.8575690  11.401  < 2e-16 ***
    ## Asian.               -0.2632073  0.0779945  -3.375 0.000739 ***
    ## Black.               -3.3250884  0.3408543  -9.755  < 2e-16 ***
    ## Other.               -3.2293469  0.9068986  -3.561 0.000370 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2046     0.3701   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6119 on 8 Df
    ## Pseudo R-squared: 0.08079
    ## Number of iterations: 15 (BFGS) + 3 (Fisher scoring)

## 30 miles

``` r
scfa3_30 = betareg(vaccination_rate_reshape ~ X3sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3421 -0.6760 -0.1602  0.4708 13.4071 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5203825  0.0197008  77.174  < 2e-16 ***
    ## X3sfca_30_normalized -0.1525109  0.0592652  -2.573 0.010071 *  
    ## IMD19.SCORE          -0.0111598  0.0005396 -20.682  < 2e-16 ***
    ## Mixed.                9.9615737  0.8584177  11.605  < 2e-16 ***
    ## Asian.               -0.2597906  0.0779825  -3.331 0.000864 ***
    ## Black.               -3.3166782  0.3407573  -9.733  < 2e-16 ***
    ## Other.               -3.2708052  0.9070575  -3.606 0.000311 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.2207     0.3704   54.59   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6121 on 8 Df
    ## Pseudo R-squared: 0.0813
    ## Number of iterations: 15 (BFGS) + 3 (Fisher scoring)
