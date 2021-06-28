Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition
================

# read data

``` r
fca<- read.csv("accessibility_imd_ethnic_include.csv", 
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
    ## -5.2507 -0.6827 -0.1587  0.4790 13.7008 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5651401  0.0150880 103.734  < 2e-16 ***
    ## X2sfca_10_normalized -0.4253480  0.0454875  -9.351  < 2e-16 ***
    ## IMD19.SCORE          -0.0123115  0.0005077 -24.249  < 2e-16 ***
    ## Mixed.                7.0164458  0.6634330  10.576  < 2e-16 ***
    ## Asian.               -0.2175492  0.0592364  -3.673 0.000240 ***
    ## Black.               -1.5005541  0.1625587  -9.231  < 2e-16 ***
    ## Other.               -2.0496483  0.5565330  -3.683 0.000231 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.5764     0.3314   59.07   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  7006 on 8 Df
    ## Pseudo R-squared: 0.09824
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -13996.59

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
    ## -5.2230 -0.6778 -0.1578  0.4756 13.6732 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5527045  0.0155269 100.001  < 2e-16 ***
    ## X2sfca_15_normalized -0.1455433  0.0418094  -3.481 0.000499 ***
    ## IMD19.SCORE          -0.0117221  0.0005225 -22.435  < 2e-16 ***
    ## Mixed.                6.0804201  0.6657138   9.134  < 2e-16 ***
    ## Asian.               -0.1610180  0.0593710  -2.712 0.006687 ** 
    ## Black.               -1.6977669  0.1660757 -10.223  < 2e-16 ***
    ## Other.               -3.1225021  0.5514192  -5.663 1.49e-08 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3736     0.3279   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6972 on 8 Df
    ## Pseudo R-squared: 0.0931
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -13928.62

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
    ## -5.2185 -0.6781 -0.1586  0.4783 13.7219 
    ## 
    ## Coefficients (mean model with logit link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.539902   0.015827  97.295  < 2e-16 ***
    ## X2sfca_20_normalized -0.022218   0.035889  -0.619  0.53586    
    ## IMD19.SCORE          -0.011238   0.000529 -21.243  < 2e-16 ***
    ## Mixed.                5.598574   0.665608   8.411  < 2e-16 ***
    ## Asian.               -0.158665   0.059575  -2.663  0.00774 ** 
    ## Black.               -1.842608   0.165608 -11.126  < 2e-16 ***
    ## Other.               -3.618156   0.541949  -6.676 2.45e-11 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3394     0.3273   59.09   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6967 on 8 Df
    ## Pseudo R-squared: 0.09228
    ## Number of iterations: 18 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -13917.46

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
    ## -5.2186 -0.6779 -0.1586  0.4792 13.7152 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5413349  0.0160884  95.804  < 2e-16 ***
    ## X2sfca_25_normalized -0.0248901  0.0318187  -0.782  0.43407    
    ## IMD19.SCORE          -0.0112743  0.0005317 -21.204  < 2e-16 ***
    ## Mixed.                5.6199543  0.6634401   8.471  < 2e-16 ***
    ## Asian.               -0.1567544  0.0597017  -2.626  0.00865 ** 
    ## Black.               -1.8369867  0.1650171 -11.132  < 2e-16 ***
    ## Other.               -3.6107692  0.5361995  -6.734 1.65e-11 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3401     0.3273   59.09   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6967 on 8 Df
    ## Pseudo R-squared: 0.0923
    ## Number of iterations: 16 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -13917.69

## 30 miles

``` r
scfa2_30 = betareg(vaccination_rate_reshape ~ X2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other., data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2195 -0.6741 -0.1578  0.4777 13.6885 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5483064  0.0163621  94.628  < 2e-16 ***
    ## X2sfca_30_normalized -0.0513058  0.0297633  -1.724   0.0847 .  
    ## IMD19.SCORE          -0.0114727  0.0005346 -21.462  < 2e-16 ***
    ## Mixed.                5.7651072  0.6627020   8.699  < 2e-16 ***
    ## Asian.               -0.1506607  0.0597579  -2.521   0.0117 *  
    ## Black.               -1.8007732  0.1641626 -10.969  < 2e-16 ***
    ## Other.               -3.5247051  0.5322542  -6.622 3.54e-11 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.3471     0.3274   59.09   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6968 on 8 Df
    ## Pseudo R-squared: 0.09253
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -13920.04

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_rate_reshape ~ E2sfca_10_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_10_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2764 -0.6821 -0.1605  0.4840 13.7639 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5608822  0.0148482 105.122  < 2e-16 ***
    ## E2sfca_10_normalized -0.7257420  0.0573756 -12.649  < 2e-16 ***
    ## IMD19.SCORE          -0.0122003  0.0004973 -24.532  < 2e-16 ***
    ## Mixed.                7.3469225  0.6589841  11.149  < 2e-16 ***
    ## Asian.               -0.2466922  0.0590221  -4.180 2.92e-05 ***
    ## Black.               -1.5722281  0.1588068  -9.900  < 2e-16 ***
    ## Other.               -1.5771637  0.5530777  -2.852  0.00435 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.7602     0.3345   59.07   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  7037 on 8 Df
    ## Pseudo R-squared: 0.1031
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -14058.74

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_rate_reshape ~ E2sfca_15_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_15_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2632 -0.6799 -0.1578  0.4814 13.7153 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5654141  0.0149786 104.510  < 2e-16 ***
    ## E2sfca_15_normalized -0.5787114  0.0520949 -11.109  < 2e-16 ***
    ## IMD19.SCORE          -0.0123564  0.0005031 -24.558  < 2e-16 ***
    ## Mixed.                7.3239016  0.6639020  11.032  < 2e-16 ***
    ## Asian.               -0.2292350  0.0590984  -3.879 0.000105 ***
    ## Black.               -1.5068990  0.1606549  -9.380  < 2e-16 ***
    ## Other.               -1.7349551  0.5568365  -3.116 0.001835 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.6670     0.3329   59.07   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  7022 on 8 Df
    ## Pseudo R-squared: 0.1006
    ## Number of iterations: 17 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -14027.06

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_rate_reshape ~ E2sfca_20_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_20_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2473 -0.6795 -0.1581  0.4802 13.6643 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5660684  0.0151492 103.376  < 2e-16 ***
    ## E2sfca_20_normalized -0.4403260  0.0495240  -8.891  < 2e-16 ***
    ## IMD19.SCORE          -0.0123210  0.0005098 -24.167  < 2e-16 ***
    ## Mixed.                7.0557834  0.6673279  10.573  < 2e-16 ***
    ## Asian.               -0.2001027  0.0591554  -3.383 0.000718 ***
    ## Black.               -1.5183742  0.1625584  -9.340  < 2e-16 ***
    ## Other.               -2.1047742  0.5577973  -3.773 0.000161 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   19.552      0.331   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  7002 on 8 Df
    ## Pseudo R-squared: 0.09759
    ## Number of iterations: 18 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -13988.36

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_rate_reshape ~ E2sfca_25_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_25_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2358 -0.6775 -0.1590  0.4774 13.6440 
    ## 
    ## Coefficients (mean model with logit link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.563887   0.015328 102.027  < 2e-16 ***
    ## E2sfca_25_normalized -0.323916   0.047246  -6.856 7.08e-12 ***
    ## IMD19.SCORE          -0.012179   0.000516 -23.605  < 2e-16 ***
    ## Mixed.                6.739762   0.669248  10.071  < 2e-16 ***
    ## Asian.               -0.177827   0.059214  -3.003  0.00267 ** 
    ## Black.               -1.569494   0.163989  -9.571  < 2e-16 ***
    ## Other.               -2.487255   0.556499  -4.469 7.84e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4681     0.3295   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6988 on 8 Df
    ## Pseudo R-squared: 0.09548
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -13960.17

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_rate_reshape ~ E2sfca_30_normalized + IMD19.SCORE + Mixed. + Asian. + Black. + Other.,data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_30_normalized + IMD19.SCORE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.2290 -0.6768 -0.1574  0.4740 13.6433 
    ## 
    ## Coefficients (mean model with logit link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.560854   0.015500 100.701  < 2e-16 ***
    ## E2sfca_30_normalized -0.239672   0.045018  -5.324 1.02e-07 ***
    ## IMD19.SCORE          -0.012021   0.000521 -23.074  < 2e-16 ***
    ## Mixed.                6.474928   0.669990   9.664  < 2e-16 ***
    ## Asian.               -0.164818   0.059277  -2.780  0.00543 ** 
    ## Black.               -1.624376   0.164881  -9.852  < 2e-16 ***
    ## Other.               -2.790040   0.553858  -5.037 4.72e-07 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4183     0.3287   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6980 on 8 Df
    ## Pseudo R-squared: 0.09424
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -13943.53

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
    ## -5.2591 -0.6828 -0.1603  0.4825 13.6895 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5654982  0.0149998 104.368  < 2e-16 ***
    ## X3sfca_10_normalized -0.5821282  0.0548327 -10.616  < 2e-16 ***
    ## IMD19.SCORE          -0.0122712  0.0005028 -24.405  < 2e-16 ***
    ## Mixed.                7.1136108  0.6612453  10.758  < 2e-16 ***
    ## Asian.               -0.2215144  0.0591033  -3.748 0.000178 ***
    ## Black.               -1.5744464  0.1598174  -9.852  < 2e-16 ***
    ## Other.               -1.8675201  0.5550152  -3.365 0.000766 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.6372     0.3324   59.07   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  7016 on 8 Df
    ## Pseudo R-squared: 0.0996
    ## Number of iterations: 17 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -14016.84

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
    ## -5.2442 -0.6785 -0.1574  0.4788 13.6783 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5694874  0.0152841 102.687  < 2e-16 ***
    ## X3sfca_15_normalized -0.4180182  0.0495782  -8.431  < 2e-16 ***
    ## IMD19.SCORE          -0.0123473  0.0005128 -24.078  < 2e-16 ***
    ## Mixed.                6.9494858  0.6668217  10.422  < 2e-16 ***
    ## Asian.               -0.1932723  0.0591520  -3.267  0.00109 ** 
    ## Black.               -1.5643029  0.1619024  -9.662  < 2e-16 ***
    ## Other.               -2.2080102  0.5565198  -3.968 7.26e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.5307     0.3306   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6999 on 8 Df
    ## Pseudo R-squared: 0.09693
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -13981.01

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
    ## -5.2353 -0.6766 -0.1592  0.4762 13.6650 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5695671  0.0155747 100.777  < 2e-16 ***
    ## X3sfca_20_normalized -0.3095976  0.0455893  -6.791 1.11e-11 ***
    ## IMD19.SCORE          -0.0122805  0.0005209 -23.574  < 2e-16 ***
    ## Mixed.                6.7181597  0.6689428  10.043  < 2e-16 ***
    ## Asian.               -0.1735081  0.0591988  -2.931  0.00338 ** 
    ## Black.               -1.5833546  0.1636613  -9.675  < 2e-16 ***
    ## Other.               -2.5416989  0.5534831  -4.592 4.39e-06 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4663     0.3295   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6988 on 8 Df
    ## Pseudo R-squared: 0.09544
    ## Number of iterations: 18 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -13959.57

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
    ## -5.2309 -0.6750 -0.1588  0.4721 13.6538 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5687595  0.0158165  99.185  < 2e-16 ***
    ## X3sfca_25_normalized -0.2490144  0.0429173  -5.802 6.55e-09 ***
    ## IMD19.SCORE          -0.0122011  0.0005265 -23.176  < 2e-16 ***
    ## Mixed.                6.5483652  0.6693644   9.783  < 2e-16 ***
    ## Asian.               -0.1622893  0.0592497  -2.739  0.00616 ** 
    ## Black.               -1.6076320  0.1646588  -9.763  < 2e-16 ***
    ## Other.               -2.7592936  0.5499473  -5.017 5.24e-07 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4334     0.3289   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6982 on 8 Df
    ## Pseudo R-squared: 0.09472
    ## Number of iterations: 20 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -13948.73

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
    ## -5.2288 -0.6748 -0.1564  0.4710 13.6460 
    ## 
    ## Coefficients (mean model with logit link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.5684784  0.0160138  97.945  < 2e-16 ***
    ## X3sfca_30_normalized -0.2166664  0.0411485  -5.265 1.40e-07 ***
    ## IMD19.SCORE          -0.0121531  0.0005302 -22.921  < 2e-16 ***
    ## Mixed.                6.4535027  0.6695396   9.639  < 2e-16 ***
    ## Asian.               -0.1560250  0.0592977  -2.631  0.00851 ** 
    ## Black.               -1.6259315  0.1650870  -9.849  < 2e-16 ***
    ## Other.               -2.8864580  0.5469966  -5.277 1.31e-07 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  19.4175     0.3287   59.08   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6980 on 8 Df
    ## Pseudo R-squared: 0.09441
    ## Number of iterations: 18 (BFGS) + 4 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -13943.54
