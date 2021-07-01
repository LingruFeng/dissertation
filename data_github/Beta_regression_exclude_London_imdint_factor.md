Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition
================

# read data

``` r
fca<- read.csv("accessibility_imd_ethnic_exclude.csv", sep=",")
```

``` r
fca$MSOADECILE <- as.factor(fca$MSOADECILE)
```

# Import beta regression library

``` r
library(betareg)
```

    ## Warning: package 'betareg' was built under R version 4.0.5

# 2SFCA

## 10 miles

``` r
scfa2_10 = betareg(vaccination_rate_reshape ~ X2sfca_10_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_10_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3839 -0.6874 -0.1682  0.4793 13.9145 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97370    0.02547  38.236  < 2e-16 ***
    ## X2sfca_10_normalized  0.11260    0.04663   2.415  0.01575 *  
    ## MSOADECILE2           0.07128    0.02856   2.496  0.01258 *  
    ## MSOADECILE3           0.14107    0.02972   4.747 2.06e-06 ***
    ## MSOADECILE4           0.15289    0.02955   5.173 2.30e-07 ***
    ## MSOADECILE5           0.20714    0.02977   6.957 3.47e-12 ***
    ## MSOADECILE6           0.28075    0.03006   9.338  < 2e-16 ***
    ## MSOADECILE7           0.37499    0.03001  12.496  < 2e-16 ***
    ## MSOADECILE8           0.40707    0.03042  13.381  < 2e-16 ***
    ## MSOADECILE9           0.45536    0.03070  14.832  < 2e-16 ***
    ## MSOADECILE10          0.47143    0.03052  15.446  < 2e-16 ***
    ## Mixed.                9.48279    0.85115  11.141  < 2e-16 ***
    ## Asian.               -0.24024    0.07786  -3.086  0.00203 ** 
    ## Black.               -3.42885    0.33877 -10.121  < 2e-16 ***
    ## Other.               -3.69599    0.89980  -4.108 4.00e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4954     0.3755   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6158 on 16 Df
    ## Pseudo R-squared: 0.08891
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -12284.45

## 15 miles

``` r
scfa2_15 = betareg(vaccination_rate_reshape ~ X2sfca_15_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_15_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3828 -0.6838 -0.1677  0.4806 13.9472 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97778    0.02593  37.714  < 2e-16 ***
    ## X2sfca_15_normalized  0.12974    0.08409   1.543  0.12289    
    ## MSOADECILE2           0.07209    0.02857   2.524  0.01161 *  
    ## MSOADECILE3           0.14251    0.02973   4.794 1.64e-06 ***
    ## MSOADECILE4           0.15449    0.02957   5.225 1.74e-07 ***
    ## MSOADECILE5           0.20845    0.02978   6.999 2.58e-12 ***
    ## MSOADECILE6           0.28316    0.03008   9.413  < 2e-16 ***
    ## MSOADECILE7           0.37500    0.03006  12.474  < 2e-16 ***
    ## MSOADECILE8           0.40859    0.03047  13.407  < 2e-16 ***
    ## MSOADECILE9           0.45682    0.03082  14.824  < 2e-16 ***
    ## MSOADECILE10          0.47423    0.03065  15.473  < 2e-16 ***
    ## Mixed.                9.60851    0.85205  11.277  < 2e-16 ***
    ## Asian.               -0.24099    0.07796  -3.091  0.00199 ** 
    ## Black.               -3.42996    0.33897 -10.119  < 2e-16 ***
    ## Other.               -3.65027    0.90043  -4.054 5.04e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4835     0.3753   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6157 on 16 Df
    ## Pseudo R-squared: 0.08852
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -12281.12

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_rate_reshape ~ X2sfca_20_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_20_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3810 -0.6843 -0.1689  0.4777 13.9042 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98821    0.02508  39.406  < 2e-16 ***
    ## X2sfca_20_normalized  0.04327    0.07323   0.591   0.5547    
    ## MSOADECILE2           0.07270    0.02857   2.545   0.0109 *  
    ## MSOADECILE3           0.14372    0.02973   4.834 1.34e-06 ***
    ## MSOADECILE4           0.15589    0.02961   5.265 1.40e-07 ***
    ## MSOADECILE5           0.20959    0.02982   7.028 2.09e-12 ***
    ## MSOADECILE6           0.28513    0.03011   9.469  < 2e-16 ***
    ## MSOADECILE7           0.37668    0.03012  12.505  < 2e-16 ***
    ## MSOADECILE8           0.41093    0.03052  13.465  < 2e-16 ***
    ## MSOADECILE9           0.46041    0.03086  14.917  < 2e-16 ***
    ## MSOADECILE10          0.47863    0.03071  15.586  < 2e-16 ***
    ## Mixed.                9.72487    0.85372  11.391  < 2e-16 ***
    ## Asian.               -0.23771    0.07797  -3.049   0.0023 ** 
    ## Black.               -3.41414    0.33882 -10.077  < 2e-16 ***
    ## Other.               -3.66345    0.90102  -4.066 4.78e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4763     0.3752   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6156 on 16 Df
    ## Pseudo R-squared: 0.08837
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -12279.2

## 25 miles

``` r
scfa2_25 = betareg(vaccination_rate_reshape ~ X2sfca_25_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_25_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3808 -0.6835 -0.1668  0.4747 13.8520 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.99578    0.02460  40.476  < 2e-16 ***
    ## X2sfca_25_normalized -0.05567    0.05498  -1.012  0.31134    
    ## MSOADECILE2           0.07344    0.02857   2.570  0.01016 *  
    ## MSOADECILE3           0.14629    0.02975   4.916 8.81e-07 ***
    ## MSOADECILE4           0.16015    0.02968   5.395 6.85e-08 ***
    ## MSOADECILE5           0.21351    0.02986   7.150 8.67e-13 ***
    ## MSOADECILE6           0.29006    0.03018   9.612  < 2e-16 ***
    ## MSOADECILE7           0.38220    0.03021  12.650  < 2e-16 ***
    ## MSOADECILE8           0.41635    0.03058  13.617  < 2e-16 ***
    ## MSOADECILE9           0.46795    0.03091  15.141  < 2e-16 ***
    ## MSOADECILE10          0.48764    0.03081  15.828  < 2e-16 ***
    ## Mixed.                9.92045    0.85037  11.666  < 2e-16 ***
    ## Asian.               -0.23176    0.07802  -2.971  0.00297 ** 
    ## Black.               -3.39775    0.33915 -10.018  < 2e-16 ***
    ## Other.               -3.70801    0.90142  -4.113 3.90e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4789     0.3752   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6156 on 16 Df
    ## Pseudo R-squared: 0.08839
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -12279.86

## 30 miles

``` r
scfa2_30 = betareg(vaccination_rate_reshape ~ X2sfca_30_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other., data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_30_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3837 -0.6840 -0.1651  0.4745 13.8007 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.99987    0.02443  40.923  < 2e-16 ***
    ## X2sfca_30_normalized -0.12650    0.04408  -2.870  0.00411 ** 
    ## MSOADECILE2           0.07517    0.02856   2.632  0.00849 ** 
    ## MSOADECILE3           0.15075    0.02977   5.064 4.10e-07 ***
    ## MSOADECILE4           0.16669    0.02971   5.611 2.01e-08 ***
    ## MSOADECILE5           0.21936    0.02988   7.342 2.11e-13 ***
    ## MSOADECILE6           0.29740    0.03021   9.844  < 2e-16 ***
    ## MSOADECILE7           0.39090    0.03029  12.905  < 2e-16 ***
    ## MSOADECILE8           0.42428    0.03061  13.859  < 2e-16 ***
    ## MSOADECILE9           0.47766    0.03095  15.433  < 2e-16 ***
    ## MSOADECILE10          0.50089    0.03099  16.164  < 2e-16 ***
    ## Mixed.               10.10456    0.85012  11.886  < 2e-16 ***
    ## Asian.               -0.22378    0.07802  -2.868  0.00413 ** 
    ## Black.               -3.36735    0.33902  -9.932  < 2e-16 ***
    ## Other.               -3.76691    0.90148  -4.179 2.93e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.5041     0.3757   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6159 on 16 Df
    ## Pseudo R-squared: 0.08913
    ## Number of iterations: 23 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -12286.84

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_rate_reshape ~ E2sfca_10_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_10_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3826 -0.6838 -0.1645  0.4786 13.8967 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98177    0.02487  39.470  < 2e-16 ***
    ## E2sfca_10_normalized  0.11052    0.05638   1.960  0.04997 *  
    ## MSOADECILE2           0.07189    0.02856   2.517  0.01184 *  
    ## MSOADECILE3           0.14223    0.02971   4.787 1.69e-06 ***
    ## MSOADECILE4           0.15453    0.02953   5.232 1.67e-07 ***
    ## MSOADECILE5           0.20738    0.02978   6.964 3.31e-12 ***
    ## MSOADECILE6           0.28228    0.03005   9.395  < 2e-16 ***
    ## MSOADECILE7           0.37683    0.02998  12.568  < 2e-16 ***
    ## MSOADECILE8           0.41141    0.03034  13.560  < 2e-16 ***
    ## MSOADECILE9           0.46018    0.03057  15.055  < 2e-16 ***
    ## MSOADECILE10          0.47694    0.03032  15.729  < 2e-16 ***
    ## Mixed.                9.53055    0.85277  11.176  < 2e-16 ***
    ## Asian.               -0.24024    0.07788  -3.085  0.00204 ** 
    ## Black.               -3.42346    0.33878 -10.105  < 2e-16 ***
    ## Other.               -3.69747    0.89997  -4.108 3.98e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4887     0.3754   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6157 on 16 Df
    ## Pseudo R-squared: 0.08875
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -12282.62

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_rate_reshape ~ E2sfca_15_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_15_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3834 -0.6848 -0.1665  0.4769 13.9125 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97616    0.02539  38.444  < 2e-16 ***
    ## E2sfca_15_normalized  0.10182    0.04695   2.169  0.03009 *  
    ## MSOADECILE2           0.07139    0.02856   2.499  0.01245 *  
    ## MSOADECILE3           0.14132    0.02972   4.755 1.99e-06 ***
    ## MSOADECILE4           0.15316    0.02956   5.181 2.20e-07 ***
    ## MSOADECILE5           0.20656    0.02980   6.932 4.15e-12 ***
    ## MSOADECILE6           0.28129    0.03007   9.355  < 2e-16 ***
    ## MSOADECILE7           0.37572    0.03000  12.523  < 2e-16 ***
    ## MSOADECILE8           0.40957    0.03037  13.487  < 2e-16 ***
    ## MSOADECILE9           0.45777    0.03063  14.943  < 2e-16 ***
    ## MSOADECILE10          0.47393    0.03044  15.570  < 2e-16 ***
    ## Mixed.                9.43980    0.85694  11.016  < 2e-16 ***
    ## Asian.               -0.24243    0.07790  -3.112  0.00186 ** 
    ## Black.               -3.42488    0.33878 -10.109  < 2e-16 ***
    ## Other.               -3.68872    0.89974  -4.100 4.14e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4915     0.3754   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6158 on 16 Df
    ## Pseudo R-squared: 0.0888
    ## Number of iterations: 24 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -12283.38

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_rate_reshape ~ E2sfca_20_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_20_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3838 -0.6854 -0.1663  0.4788 13.9466 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97239    0.02583  37.641  < 2e-16 ***
    ## E2sfca_20_normalized  0.13629    0.06100   2.234  0.02547 *  
    ## MSOADECILE2           0.07141    0.02856   2.500  0.01242 *  
    ## MSOADECILE3           0.14109    0.02973   4.746 2.07e-06 ***
    ## MSOADECILE4           0.15262    0.02958   5.160 2.47e-07 ***
    ## MSOADECILE5           0.20660    0.02980   6.933 4.12e-12 ***
    ## MSOADECILE6           0.28098    0.03009   9.339  < 2e-16 ***
    ## MSOADECILE7           0.37477    0.03002  12.483  < 2e-16 ***
    ## MSOADECILE8           0.40830    0.03041  13.428  < 2e-16 ***
    ## MSOADECILE9           0.45586    0.03071  14.843  < 2e-16 ***
    ## MSOADECILE10          0.47182    0.03056  15.440  < 2e-16 ***
    ## Mixed.                9.40022    0.85940  10.938  < 2e-16 ***
    ## Asian.               -0.24413    0.07793  -3.133  0.00173 ** 
    ## Black.               -3.42715    0.33879 -10.116  < 2e-16 ***
    ## Other.               -3.67351    0.89965  -4.083 4.44e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4925     0.3755   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6158 on 16 Df
    ## Pseudo R-squared: 0.0888
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -12283.61

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_rate_reshape ~ E2sfca_25_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_25_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3833 -0.6831 -0.1656  0.4808 13.9578 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97458    0.02592  37.603  < 2e-16 ***
    ## E2sfca_25_normalized  0.13908    0.07256   1.917  0.05527 .  
    ## MSOADECILE2           0.07178    0.02856   2.513  0.01197 *  
    ## MSOADECILE3           0.14158    0.02973   4.762 1.92e-06 ***
    ## MSOADECILE4           0.15302    0.02960   5.170 2.34e-07 ***
    ## MSOADECILE5           0.20720    0.02981   6.952 3.61e-12 ***
    ## MSOADECILE6           0.28161    0.03010   9.355  < 2e-16 ***
    ## MSOADECILE7           0.37436    0.03006  12.455  < 2e-16 ***
    ## MSOADECILE8           0.40812    0.03045  13.403  < 2e-16 ***
    ## MSOADECILE9           0.45567    0.03079  14.799  < 2e-16 ***
    ## MSOADECILE10          0.47207    0.03067  15.393  < 2e-16 ***
    ## Mixed.                9.45516    0.86011  10.993  < 2e-16 ***
    ## Asian.               -0.24378    0.07797  -3.126  0.00177 ** 
    ## Black.               -3.42623    0.33885 -10.111  < 2e-16 ***
    ## Other.               -3.66087    0.89991  -4.068 4.74e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4880     0.3754   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6157 on 16 Df
    ## Pseudo R-squared: 0.08866
    ## Number of iterations: 23 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -12282.36

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_rate_reshape ~ E2sfca_30_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_30_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3822 -0.6829 -0.1677  0.4796 13.9376 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98132    0.02569  38.196  < 2e-16 ***
    ## E2sfca_30_normalized  0.09576    0.07606   1.259  0.20800    
    ## MSOADECILE2           0.07219    0.02857   2.527  0.01151 *  
    ## MSOADECILE3           0.14251    0.02974   4.791 1.66e-06 ***
    ## MSOADECILE4           0.15419    0.02962   5.205 1.94e-07 ***
    ## MSOADECILE5           0.20826    0.02982   6.984 2.88e-12 ***
    ## MSOADECILE6           0.28314    0.03013   9.397  < 2e-16 ***
    ## MSOADECILE7           0.37510    0.03010  12.460  < 2e-16 ***
    ## MSOADECILE8           0.40922    0.03049  13.420  < 2e-16 ***
    ## MSOADECILE9           0.45765    0.03085  14.832  < 2e-16 ***
    ## MSOADECILE10          0.47477    0.03077  15.432  < 2e-16 ***
    ## Mixed.                9.58594    0.85923  11.156  < 2e-16 ***
    ## Asian.               -0.24130    0.07801  -3.093  0.00198 ** 
    ## Black.               -3.42306    0.33895 -10.099  < 2e-16 ***
    ## Other.               -3.65915    0.90041  -4.064 4.83e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4807     0.3752   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6156 on 16 Df
    ## Pseudo R-squared: 0.08847
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -12280.38

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_rate_reshape ~ X3sfca_10_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_10_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3838 -0.6853 -0.1653  0.4781 13.9309 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97990    0.02478  39.539  < 2e-16 ***
    ## X3sfca_10_normalized  0.14275    0.05627   2.537  0.01119 *  
    ## MSOADECILE2           0.07191    0.02856   2.518  0.01180 *  
    ## MSOADECILE3           0.14186    0.02970   4.776 1.79e-06 ***
    ## MSOADECILE4           0.15355    0.02953   5.200 2.00e-07 ***
    ## MSOADECILE5           0.20622    0.02978   6.926 4.34e-12 ***
    ## MSOADECILE6           0.28036    0.03006   9.328  < 2e-16 ***
    ## MSOADECILE7           0.37520    0.02999  12.509  < 2e-16 ***
    ## MSOADECILE8           0.40977    0.03035  13.501  < 2e-16 ***
    ## MSOADECILE9           0.45770    0.03059  14.962  < 2e-16 ***
    ## MSOADECILE10          0.47326    0.03039  15.570  < 2e-16 ***
    ## Mixed.                9.48387    0.85066  11.149  < 2e-16 ***
    ## Asian.               -0.23808    0.07783  -3.059  0.00222 ** 
    ## Black.               -3.41974    0.33860 -10.100  < 2e-16 ***
    ## Other.               -3.66209    0.89971  -4.070 4.70e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4980     0.3756   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6159 on 16 Df
    ## Pseudo R-squared: 0.08905
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -12285.24

## 15 miles

``` r
scfa3_15 = betareg(vaccination_rate_reshape ~ X3sfca_15_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_15_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3819 -0.6832 -0.1659  0.4788 13.9048 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98428    0.02492  39.494  < 2e-16 ***
    ## X3sfca_15_normalized  0.07157    0.05080   1.409  0.15889    
    ## MSOADECILE2           0.07192    0.02857   2.517  0.01182 *  
    ## MSOADECILE3           0.14223    0.02974   4.783 1.73e-06 ***
    ## MSOADECILE4           0.15388    0.02960   5.198 2.01e-07 ***
    ## MSOADECILE5           0.20712    0.02986   6.937 4.00e-12 ***
    ## MSOADECILE6           0.28218    0.03014   9.363  < 2e-16 ***
    ## MSOADECILE7           0.37500    0.03008  12.467  < 2e-16 ***
    ## MSOADECILE8           0.40919    0.03044  13.442  < 2e-16 ***
    ## MSOADECILE9           0.45782    0.03075  14.890  < 2e-16 ***
    ## MSOADECILE10          0.47487    0.03061  15.513  < 2e-16 ***
    ## Mixed.                9.60493    0.85371  11.251  < 2e-16 ***
    ## Asian.               -0.23816    0.07789  -3.058  0.00223 ** 
    ## Black.               -3.41710    0.33877 -10.087  < 2e-16 ***
    ## Other.               -3.65569    0.90049  -4.060 4.91e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4820     0.3753   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6156 on 16 Df
    ## Pseudo R-squared: 0.08856
    ## Number of iterations: 24 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -12280.79

## 20 miles

``` r
scfa3_20 = betareg(vaccination_rate_reshape ~ X3sfca_20_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_20_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3810 -0.6830 -0.1680  0.4772 13.8786 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.99359    0.02493  39.849  < 2e-16 ***
    ## X3sfca_20_normalized -0.01772    0.05627  -0.315  0.75285    
    ## MSOADECILE2           0.07309    0.02858   2.558  0.01054 *  
    ## MSOADECILE3           0.14511    0.02976   4.876 1.08e-06 ***
    ## MSOADECILE4           0.15809    0.02966   5.329 9.86e-08 ***
    ## MSOADECILE5           0.21179    0.02990   7.084 1.40e-12 ***
    ## MSOADECILE6           0.28785    0.03020   9.532  < 2e-16 ***
    ## MSOADECILE7           0.37944    0.03016  12.581  < 2e-16 ***
    ## MSOADECILE8           0.41381    0.03053  13.553  < 2e-16 ***
    ## MSOADECILE9           0.46460    0.03088  15.045  < 2e-16 ***
    ## MSOADECILE10          0.48359    0.03082  15.692  < 2e-16 ***
    ## Mixed.                9.85444    0.85545  11.520  < 2e-16 ***
    ## Asian.               -0.23507    0.07795  -3.016  0.00256 ** 
    ## Black.               -3.40994    0.33887 -10.063  < 2e-16 ***
    ## Other.               -3.68657    0.90130  -4.090 4.31e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4755     0.3752   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6155 on 16 Df
    ## Pseudo R-squared: 0.08832
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -12278.95

## 25 miles

``` r
scfa3_25 = betareg(vaccination_rate_reshape ~ X3sfca_25_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_25_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3819 -0.6818 -0.1671  0.4750 13.8457 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.00192    0.02492  40.208  < 2e-16 ***
    ## X3sfca_25_normalized -0.10726    0.05900  -1.818  0.06905 .  
    ## MSOADECILE2           0.07442    0.02857   2.604  0.00921 ** 
    ## MSOADECILE3           0.14839    0.02977   4.985 6.21e-07 ***
    ## MSOADECILE4           0.16299    0.02969   5.490 4.02e-08 ***
    ## MSOADECILE5           0.21656    0.02991   7.241 4.45e-13 ***
    ## MSOADECILE6           0.29375    0.03022   9.719  < 2e-16 ***
    ## MSOADECILE7           0.38522    0.03021  12.751  < 2e-16 ***
    ## MSOADECILE8           0.41979    0.03059  13.723  < 2e-16 ***
    ## MSOADECILE9           0.47261    0.03097  15.260  < 2e-16 ***
    ## MSOADECILE10          0.49396    0.03098  15.944  < 2e-16 ***
    ## Mixed.               10.07624    0.85633  11.767  < 2e-16 ***
    ## Asian.               -0.23086    0.07796  -2.961  0.00306 ** 
    ## Black.               -3.39809    0.33883 -10.029  < 2e-16 ***
    ## Other.               -3.73410    0.90170  -4.141 3.46e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4868     0.3754   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6157 on 16 Df
    ## Pseudo R-squared: 0.08862
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -12282.05

## 30 miles

``` r
scfa3_30 = betareg(vaccination_rate_reshape ~ X3sfca_30_normalized + MSOADECILE + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_30_normalized + MSOADECILE + 
    ##     Mixed. + Asian. + Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.3844 -0.6797 -0.1676  0.4760 13.8075 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.00913    0.02488  40.560  < 2e-16 ***
    ## X3sfca_30_normalized -0.18628    0.05922  -3.146  0.00166 ** 
    ## MSOADECILE2           0.07561    0.02856   2.647  0.00811 ** 
    ## MSOADECILE3           0.15157    0.02976   5.093 3.53e-07 ***
    ## MSOADECILE4           0.16765    0.02969   5.646 1.64e-08 ***
    ## MSOADECILE5           0.22096    0.02990   7.390 1.47e-13 ***
    ## MSOADECILE6           0.29926    0.03023   9.900  < 2e-16 ***
    ## MSOADECILE7           0.39122    0.03024  12.938  < 2e-16 ***
    ## MSOADECILE8           0.42585    0.03062  13.907  < 2e-16 ***
    ## MSOADECILE9           0.48085    0.03104  15.493  < 2e-16 ***
    ## MSOADECILE10          0.50457    0.03110  16.222  < 2e-16 ***
    ## Mixed.               10.27478    0.85706  11.988  < 2e-16 ***
    ## Asian.               -0.22645    0.07794  -2.905  0.00367 ** 
    ## Black.               -3.38430    0.33872  -9.992  < 2e-16 ***
    ## Other.               -3.79396    0.90182  -4.207 2.59e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.5099     0.3758   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6160 on 16 Df
    ## Pseudo R-squared: 0.0893
    ## Number of iterations: 24 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -12288.44
