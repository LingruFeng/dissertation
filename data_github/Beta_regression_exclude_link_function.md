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
scfa2_20 = betareg(vaccination_rate_no_zero ~ X2sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "logit")

summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "logit")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.4581  -0.4772  -0.0749   0.4294   7.6075 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.3267719  0.0229425  57.830  < 2e-16 ***
    ## X2sfca_20_normalized  0.2210410  0.0884854   2.498  0.01249 *  
    ## IMD19.SCORE          -0.0078137  0.0006472 -12.073  < 2e-16 ***
    ## Mixed.                7.5739380  1.0297285   7.355 1.91e-13 ***
    ## Asian.               -0.2531184  0.0956910  -2.645  0.00817 ** 
    ## Black.               -2.8742297  0.4179946  -6.876 6.15e-12 ***
    ## Other.               -3.0852330  1.1099739  -2.780  0.00544 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)    12.66       0.23   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4796 on 8 Df
    ## Pseudo R-squared: 0.04519
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

## probit

``` r
scfa2_25 = betareg(vaccination_rate_no_zero ~ X2sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "probit")
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "probit")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.4369  -0.4798  -0.0735   0.4295   7.5838 
    ## 
    ## Coefficients (mean model with probit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.8159180  0.0127075  64.208  < 2e-16 ***
    ## X2sfca_25_normalized  0.0651090  0.0383459   1.698  0.08952 .  
    ## IMD19.SCORE          -0.0046229  0.0003823 -12.092  < 2e-16 ***
    ## Mixed.                4.5988447  0.6006743   7.656 1.92e-14 ***
    ## Asian.               -0.1458716  0.0574490  -2.539  0.01111 *  
    ## Black.               -1.7471987  0.2521473  -6.929 4.23e-12 ***
    ## Other.               -1.8541999  0.6691649  -2.771  0.00559 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6546     0.2299   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4795 on 8 Df
    ## Pseudo R-squared: 0.06094
    ## Number of iterations: 16 (BFGS) + 2 (Fisher scoring)

## cloglog

``` r
scfa2_30 = betareg(vaccination_rate_no_zero ~ X2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "cloglog")
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ X2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "cloglog")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.4021  -0.4779  -0.0744   0.4294   7.5694 
    ## 
    ## Coefficients (mean model with cloglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.4619101  0.0112374  41.105  < 2e-16 ***
    ## X2sfca_30_normalized  0.0082351  0.0272919   0.302  0.76285    
    ## IMD19.SCORE          -0.0042880  0.0003514 -12.202  < 2e-16 ***
    ## Mixed.                4.3853253  0.5478369   8.005 1.20e-15 ***
    ## Asian.               -0.1249187  0.0546651  -2.285  0.02230 *  
    ## Black.               -1.6895989  0.2457414  -6.876 6.18e-12 ***
    ## Other.               -1.7732541  0.6464874  -2.743  0.00609 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)    12.66       0.23   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4795 on 8 Df
    ## Pseudo R-squared: 0.01817
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

## cauchit

``` r
E2scfa_10 = betareg(vaccination_rate_no_zero ~ E2sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "cauchit")
```

    ## Warning in betareg.fit(X, Y, Z, weights, offset, link, link.phi, type, control):
    ## no valid starting value for precision parameter found, using 1 instead

``` r
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "cauchit")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.3850  -0.4750  -0.0737   0.4309   7.6072 
    ## 
    ## Coefficients (mean model with cauchit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.3215992  0.0269464  49.046  < 2e-16 ***
    ## E2sfca_10_normalized  0.0509166  0.0901103   0.565  0.57204    
    ## IMD19.SCORE          -0.0099107  0.0007591 -13.056  < 2e-16 ***
    ## Mixed.                8.9051767  1.2694528   7.015 2.30e-12 ***
    ## Asian.               -0.3124001  0.0987969  -3.162  0.00157 ** 
    ## Black.               -2.8108530  0.4212886  -6.672 2.52e-11 ***
    ## Other.               -3.3016153  1.0995690  -3.003  0.00268 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6104     0.2291   55.05   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4786 on 8 Df
    ## Pseudo R-squared: 0.001247
    ## Number of iterations: 37 (BFGS) + 3 (Fisher scoring)

## log

``` r
E2scfa_15 = betareg(vaccination_rate_no_zero ~ E2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,
                data = fca ,  link = "log")
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_no_zero ~ E2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca, link = "log")
    ## 
    ## Standardized weighted residuals 2:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.4220  -0.4802  -0.0736   0.4304   7.6367 
    ## 
    ## Coefficients (mean model with log link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -0.2326824  0.0050203 -46.349  < 2e-16 ***
    ## E2sfca_15_normalized  0.0240678  0.0120962   1.990  0.04662 *  
    ## IMD19.SCORE          -0.0018709  0.0001549 -12.080  < 2e-16 ***
    ## Mixed.                1.9316869  0.2467252   7.829 4.91e-15 ***
    ## Asian.               -0.0532757  0.0268167  -1.987  0.04696 *  
    ## Black.               -0.8507096  0.1235740  -6.884 5.81e-12 ***
    ## Other.               -0.8724830  0.3216157  -2.713  0.00667 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6765     0.2303   55.03   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4799 on 8 Df
    ## Pseudo R-squared: 0.002538
    ## Number of iterations: 38 (BFGS) + 3 (Fisher scoring)
