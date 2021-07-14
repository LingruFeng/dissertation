Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition
================

# read data

``` r
fca<- read.csv("accessibility_imd_ethnic_exclude.csv", sep=",")
```

``` r
#fca$MSOADECILE <- as.factor(fca$MSOADECILE)
fca$MSOADECILE <- factor(fca$MSOADECILE, levels = c(1,2,3,4,5,6,7,8,9,10))
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
    ## -5.3761 -0.6876 -0.1692  0.4776 14.1649 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97350    0.02550  38.180  < 2e-16 ***
    ## X2sfca_10_normalized  0.11220    0.04669   2.403  0.01626 *  
    ## MSOADECILE2           0.07126    0.02860   2.492  0.01271 *  
    ## MSOADECILE3           0.14133    0.02975   4.750 2.04e-06 ***
    ## MSOADECILE4           0.15288    0.02959   5.166 2.39e-07 ***
    ## MSOADECILE5           0.20714    0.02981   6.949 3.69e-12 ***
    ## MSOADECILE6           0.28075    0.03010   9.327  < 2e-16 ***
    ## MSOADECILE7           0.37544    0.03005  12.495  < 2e-16 ***
    ## MSOADECILE8           0.40817    0.03046  13.399  < 2e-16 ***
    ## MSOADECILE9           0.45649    0.03074  14.848  < 2e-16 ***
    ## MSOADECILE10          0.47182    0.03056  15.439  < 2e-16 ***
    ## Mixed.                9.49301    0.85225  11.139  < 2e-16 ***
    ## Asian.               -0.24023    0.07795  -3.082  0.00206 ** 
    ## Black.               -3.43154    0.33921 -10.116  < 2e-16 ***
    ## Other.               -3.69553    0.90097  -4.102 4.10e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4381     0.3745   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6152 on 16 Df
    ## Pseudo R-squared: 0.08811
    ## Number of iterations: 23 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -12271.33

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
    ## -5.3751 -0.6832 -0.1676  0.4787 14.1972 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97772    0.02596  37.664  < 2e-16 ***
    ## X2sfca_15_normalized  0.12784    0.08420   1.518  0.12893    
    ## MSOADECILE2           0.07208    0.02860   2.520  0.01173 *  
    ## MSOADECILE3           0.14278    0.02977   4.797 1.61e-06 ***
    ## MSOADECILE4           0.15451    0.02960   5.219 1.80e-07 ***
    ## MSOADECILE5           0.20847    0.02982   6.991 2.73e-12 ***
    ## MSOADECILE6           0.28318    0.03012   9.403  < 2e-16 ***
    ## MSOADECILE7           0.37548    0.03010  12.474  < 2e-16 ***
    ## MSOADECILE8           0.40973    0.03052  13.426  < 2e-16 ***
    ## MSOADECILE9           0.45802    0.03086  14.843  < 2e-16 ***
    ## MSOADECILE10          0.47470    0.03069  15.468  < 2e-16 ***
    ## Mixed.                9.62057    0.85317  11.276  < 2e-16 ***
    ## Asian.               -0.24091    0.07806  -3.086  0.00203 ** 
    ## Black.               -3.43248    0.33941 -10.113  < 2e-16 ***
    ## Other.               -3.65026    0.90161  -4.049 5.15e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4262     0.3742   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6150 on 16 Df
    ## Pseudo R-squared: 0.08772
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -12267.98

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
    ## -5.3732 -0.6843 -0.1694  0.4766 14.1540 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98805    0.02511  39.351  < 2e-16 ***
    ## X2sfca_20_normalized  0.04197    0.07332   0.572  0.56706    
    ## MSOADECILE2           0.07268    0.02860   2.541  0.01106 *  
    ## MSOADECILE3           0.14399    0.02977   4.837 1.32e-06 ***
    ## MSOADECILE4           0.15591    0.02965   5.259 1.45e-07 ***
    ## MSOADECILE5           0.20961    0.02986   7.020 2.22e-12 ***
    ## MSOADECILE6           0.28515    0.03015   9.458  < 2e-16 ***
    ## MSOADECILE7           0.37716    0.03016  12.505  < 2e-16 ***
    ## MSOADECILE8           0.41207    0.03056  13.484  < 2e-16 ***
    ## MSOADECILE9           0.46160    0.03091  14.935  < 2e-16 ***
    ## MSOADECILE10          0.47908    0.03075  15.581  < 2e-16 ***
    ## Mixed.                9.73648    0.85483  11.390  < 2e-16 ***
    ## Asian.               -0.23765    0.07807  -3.044  0.00233 ** 
    ## Black.               -3.41684    0.33926 -10.072  < 2e-16 ***
    ## Other.               -3.66349    0.90219  -4.061 4.89e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4192     0.3741   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6149 on 16 Df
    ## Pseudo R-squared: 0.08758
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -12266.11

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
    ## -5.3731 -0.6831 -0.1680  0.4739 14.1011 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.99560    0.02463  40.418  < 2e-16 ***
    ## X2sfca_25_normalized -0.05685    0.05505  -1.033  0.30177    
    ## MSOADECILE2           0.07343    0.02861   2.567  0.01026 *  
    ## MSOADECILE3           0.14657    0.02979   4.920 8.67e-07 ***
    ## MSOADECILE4           0.16019    0.02972   5.390 7.05e-08 ***
    ## MSOADECILE5           0.21355    0.02990   7.143 9.16e-13 ***
    ## MSOADECILE6           0.29010    0.03022   9.601  < 2e-16 ***
    ## MSOADECILE7           0.38271    0.03025  12.651  < 2e-16 ***
    ## MSOADECILE8           0.41750    0.03062  13.636  < 2e-16 ***
    ## MSOADECILE9           0.46916    0.03095  15.160  < 2e-16 ***
    ## MSOADECILE10          0.48813    0.03085  15.823  < 2e-16 ***
    ## Mixed.                9.93191    0.85147  11.664  < 2e-16 ***
    ## Asian.               -0.23168    0.07812  -2.966  0.00302 ** 
    ## Black.               -3.40020    0.33959 -10.013  < 2e-16 ***
    ## Other.               -3.70819    0.90259  -4.108 3.98e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4220     0.3742   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6149 on 16 Df
    ## Pseudo R-squared: 0.0876
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -12266.83

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
    ## -5.3760 -0.6832 -0.1654  0.4721 14.0493 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.99966    0.02446  40.863  < 2e-16 ***
    ## X2sfca_30_normalized -0.12742    0.04413  -2.887  0.00389 ** 
    ## MSOADECILE2           0.07517    0.02860   2.629  0.00858 ** 
    ## MSOADECILE3           0.15104    0.02980   5.068 4.03e-07 ***
    ## MSOADECILE4           0.16674    0.02974   5.606 2.07e-08 ***
    ## MSOADECILE5           0.21940    0.02991   7.334 2.23e-13 ***
    ## MSOADECILE6           0.29745    0.03025   9.833  < 2e-16 ***
    ## MSOADECILE7           0.39142    0.03033  12.906  < 2e-16 ***
    ## MSOADECILE8           0.42545    0.03066  13.878  < 2e-16 ***
    ## MSOADECILE9           0.47887    0.03099  15.451  < 2e-16 ***
    ## MSOADECILE10          0.50139    0.03103  16.160  < 2e-16 ***
    ## Mixed.               10.11570    0.85121  11.884  < 2e-16 ***
    ## Asian.               -0.22370    0.07812  -2.864  0.00419 ** 
    ## Black.               -3.36973    0.33946  -9.927  < 2e-16 ***
    ## Other.               -3.76712    0.90264  -4.173 3.00e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4474     0.3746   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6153 on 16 Df
    ## Pseudo R-squared: 0.08835
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -12273.87

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
    ## -5.3748 -0.6833 -0.1652  0.4774 14.1471 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98146    0.02491  39.407  < 2e-16 ***
    ## E2sfca_10_normalized  0.11107    0.05645   1.968  0.04912 *  
    ## MSOADECILE2           0.07187    0.02860   2.513  0.01198 *  
    ## MSOADECILE3           0.14246    0.02975   4.789 1.68e-06 ***
    ## MSOADECILE4           0.15450    0.02957   5.225 1.74e-07 ***
    ## MSOADECILE5           0.20734    0.02982   6.954 3.55e-12 ***
    ## MSOADECILE6           0.28223    0.03008   9.382  < 2e-16 ***
    ## MSOADECILE7           0.37726    0.03002  12.566  < 2e-16 ***
    ## MSOADECILE8           0.41249    0.03038  13.577  < 2e-16 ***
    ## MSOADECILE9           0.46126    0.03061  15.070  < 2e-16 ***
    ## MSOADECILE10          0.47728    0.03036  15.720  < 2e-16 ***
    ## Mixed.                9.53824    0.85386  11.171  < 2e-16 ***
    ## Asian.               -0.24026    0.07798  -3.081  0.00206 ** 
    ## Black.               -3.42626    0.33921 -10.101  < 2e-16 ***
    ## Other.               -3.69718    0.90113  -4.103 4.08e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4317     0.3743   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6151 on 16 Df
    ## Pseudo R-squared: 0.08796
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -12269.58

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
    ## -5.3756 -0.6853 -0.1673  0.4759 14.1630 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97591    0.02542  38.386  < 2e-16 ***
    ## E2sfca_15_normalized  0.10169    0.04701   2.163  0.03052 *  
    ## MSOADECILE2           0.07137    0.02860   2.495  0.01258 *  
    ## MSOADECILE3           0.14157    0.02976   4.757 1.97e-06 ***
    ## MSOADECILE4           0.15314    0.02960   5.175 2.28e-07 ***
    ## MSOADECILE5           0.20655    0.02984   6.923 4.42e-12 ***
    ## MSOADECILE6           0.28127    0.03011   9.342  < 2e-16 ***
    ## MSOADECILE7           0.37616    0.03004  12.522  < 2e-16 ***
    ## MSOADECILE8           0.41066    0.03041  13.504  < 2e-16 ***
    ## MSOADECILE9           0.45888    0.03068  14.959  < 2e-16 ***
    ## MSOADECILE10          0.47430    0.03048  15.563  < 2e-16 ***
    ## Mixed.                9.44935    0.85805  11.013  < 2e-16 ***
    ## Asian.               -0.24243    0.07800  -3.108  0.00188 ** 
    ## Black.               -3.42762    0.33922 -10.104  < 2e-16 ***
    ## Other.               -3.68831    0.90091  -4.094 4.24e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4344     0.3744   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6151 on 16 Df
    ## Pseudo R-squared: 0.08801
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -12270.29

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
    ## -5.3760 -0.6856 -0.1669  0.4757 14.1972 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97223    0.02587  37.587  < 2e-16 ***
    ## E2sfca_20_normalized  0.13549    0.06108   2.218  0.02653 *  
    ## MSOADECILE2           0.07140    0.02860   2.496  0.01254 *  
    ## MSOADECILE3           0.14136    0.02977   4.749 2.05e-06 ***
    ## MSOADECILE4           0.15262    0.02961   5.154 2.55e-07 ***
    ## MSOADECILE5           0.20661    0.02984   6.925 4.37e-12 ***
    ## MSOADECILE6           0.28098    0.03012   9.328  < 2e-16 ***
    ## MSOADECILE7           0.37522    0.03006  12.482  < 2e-16 ***
    ## MSOADECILE8           0.40941    0.03045  13.446  < 2e-16 ***
    ## MSOADECILE9           0.45701    0.03075  14.860  < 2e-16 ***
    ## MSOADECILE10          0.47225    0.03060  15.434  < 2e-16 ***
    ## Mixed.                9.41171    0.86052  10.937  < 2e-16 ***
    ## Asian.               -0.24409    0.07803  -3.128  0.00176 ** 
    ## Black.               -3.42983    0.33923 -10.111  < 2e-16 ***
    ## Other.               -3.67313    0.90083  -4.077 4.55e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4352     0.3744   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6151 on 16 Df
    ## Pseudo R-squared: 0.088
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -12270.48

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
    ## -5.3755 -0.6836 -0.1660  0.4793 14.2081 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97449    0.02595  37.552  < 2e-16 ***
    ## E2sfca_25_normalized  0.13766    0.07265   1.895   0.0581 .  
    ## MSOADECILE2           0.07177    0.02860   2.509   0.0121 *  
    ## MSOADECILE3           0.14185    0.02977   4.765 1.89e-06 ***
    ## MSOADECILE4           0.15304    0.02963   5.165 2.41e-07 ***
    ## MSOADECILE5           0.20722    0.02984   6.944 3.81e-12 ***
    ## MSOADECILE6           0.28164    0.03014   9.344  < 2e-16 ***
    ## MSOADECILE7           0.37484    0.03010  12.455  < 2e-16 ***
    ## MSOADECILE8           0.40925    0.03049  13.422  < 2e-16 ***
    ## MSOADECILE9           0.45685    0.03083  14.817  < 2e-16 ***
    ## MSOADECILE10          0.47253    0.03071  15.388  < 2e-16 ***
    ## Mixed.                9.46788    0.86123  10.993  < 2e-16 ***
    ## Asian.               -0.24370    0.07807  -3.121   0.0018 ** 
    ## Black.               -3.42886    0.33928 -10.106  < 2e-16 ***
    ## Other.               -3.66064    0.90109  -4.062 4.86e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4306     0.3743   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6151 on 16 Df
    ## Pseudo R-squared: 0.08787
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -12269.21

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
    ## -5.3745 -0.6825 -0.1679  0.4780 14.1874 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98124    0.02572  38.145  < 2e-16 ***
    ## E2sfca_30_normalized  0.09403    0.07615   1.235  0.21690    
    ## MSOADECILE2           0.07218    0.02860   2.523  0.01162 *  
    ## MSOADECILE3           0.14279    0.02978   4.795 1.63e-06 ***
    ## MSOADECILE4           0.15423    0.02966   5.200 1.99e-07 ***
    ## MSOADECILE5           0.20829    0.02986   6.976 3.04e-12 ***
    ## MSOADECILE6           0.28317    0.03017   9.387  < 2e-16 ***
    ## MSOADECILE7           0.37559    0.03014  12.460  < 2e-16 ***
    ## MSOADECILE8           0.41037    0.03054  13.439  < 2e-16 ***
    ## MSOADECILE9           0.45885    0.03090  14.851  < 2e-16 ***
    ## MSOADECILE10          0.47526    0.03081  15.428  < 2e-16 ***
    ## Mixed.                9.59908    0.86036  11.157  < 2e-16 ***
    ## Asian.               -0.24121    0.07811  -3.088  0.00201 ** 
    ## Black.               -3.42563    0.33939 -10.094  < 2e-16 ***
    ## Other.               -3.65909    0.90159  -4.059 4.94e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4235     0.3742   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6150 on 16 Df
    ## Pseudo R-squared: 0.08767
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -12267.25

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
    ## -5.3761 -0.6853 -0.1661  0.4765 14.1819 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.97959    0.02481  39.477  < 2e-16 ***
    ## X3sfca_10_normalized  0.14320    0.05635   2.541  0.01104 *  
    ## MSOADECILE2           0.07188    0.02859   2.514  0.01194 *  
    ## MSOADECILE3           0.14210    0.02974   4.778 1.77e-06 ***
    ## MSOADECILE4           0.15353    0.02957   5.192 2.08e-07 ***
    ## MSOADECILE5           0.20619    0.02981   6.916 4.65e-12 ***
    ## MSOADECILE6           0.28031    0.03009   9.315  < 2e-16 ***
    ## MSOADECILE7           0.37562    0.03003  12.507  < 2e-16 ***
    ## MSOADECILE8           0.41084    0.03039  13.518  < 2e-16 ***
    ## MSOADECILE9           0.45878    0.03063  14.977  < 2e-16 ***
    ## MSOADECILE10          0.47360    0.03043  15.562  < 2e-16 ***
    ## Mixed.                9.49198    0.85176  11.144  < 2e-16 ***
    ## Asian.               -0.23809    0.07792  -3.055  0.00225 ** 
    ## Black.               -3.42252    0.33904 -10.095  < 2e-16 ***
    ## Other.               -3.66170    0.90087  -4.065 4.81e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4410     0.3745   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6152 on 16 Df
    ## Pseudo R-squared: 0.08826
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -12272.2

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
    ## -5.3742 -0.6835 -0.1661  0.4775 14.1552 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.98401    0.02495  39.433  < 2e-16 ***
    ## X3sfca_15_normalized  0.07161    0.05087   1.408  0.15921    
    ## MSOADECILE2           0.07190    0.02861   2.514  0.01195 *  
    ## MSOADECILE3           0.14247    0.02978   4.785 1.71e-06 ***
    ## MSOADECILE4           0.15386    0.02964   5.191 2.09e-07 ***
    ## MSOADECILE5           0.20710    0.02989   6.928 4.27e-12 ***
    ## MSOADECILE6           0.28216    0.03018   9.351  < 2e-16 ***
    ## MSOADECILE7           0.37543    0.03012  12.466  < 2e-16 ***
    ## MSOADECILE8           0.41027    0.03048  13.459  < 2e-16 ***
    ## MSOADECILE9           0.45892    0.03079  14.905  < 2e-16 ***
    ## MSOADECILE10          0.47523    0.03065  15.505  < 2e-16 ***
    ## Mixed.                9.61395    0.85481  11.247  < 2e-16 ***
    ## Asian.               -0.23816    0.07799  -3.054  0.00226 ** 
    ## Black.               -3.41986    0.33921 -10.082  < 2e-16 ***
    ## Other.               -3.65528    0.90165  -4.054 5.04e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4249     0.3742   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6150 on 16 Df
    ## Pseudo R-squared: 0.08777
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -12267.72

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
    ## -5.3732 -0.6824 -0.1679  0.4761 14.1285 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           0.99338    0.02497  39.790  < 2e-16 ***
    ## X3sfca_20_normalized -0.01832    0.05634  -0.325   0.7451    
    ## MSOADECILE2           0.07308    0.02861   2.554   0.0106 *  
    ## MSOADECILE3           0.14537    0.02980   4.878 1.07e-06 ***
    ## MSOADECILE4           0.15810    0.02970   5.323 1.02e-07 ***
    ## MSOADECILE5           0.21181    0.02994   7.075 1.49e-12 ***
    ## MSOADECILE6           0.28786    0.03023   9.521  < 2e-16 ***
    ## MSOADECILE7           0.37991    0.03020  12.581  < 2e-16 ***
    ## MSOADECILE8           0.41493    0.03058  13.571  < 2e-16 ***
    ## MSOADECILE9           0.46575    0.03092  15.062  < 2e-16 ***
    ## MSOADECILE10          0.48402    0.03086  15.686  < 2e-16 ***
    ## Mixed.                9.86512    0.85656  11.517  < 2e-16 ***
    ## Asian.               -0.23505    0.07805  -3.012   0.0026 ** 
    ## Black.               -3.41264    0.33930 -10.058  < 2e-16 ***
    ## Other.               -3.68640    0.90247  -4.085 4.41e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4185     0.3741   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6149 on 16 Df
    ## Pseudo R-squared: 0.08753
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -12265.89

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
    ## -5.3742 -0.6807 -0.1675  0.4743 14.0950 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.00176    0.02495  40.151  < 2e-16 ***
    ## X3sfca_25_normalized -0.10832    0.05907  -1.834  0.06670 .  
    ## MSOADECILE2           0.07441    0.02861   2.601  0.00930 ** 
    ## MSOADECILE3           0.14867    0.02981   4.988 6.11e-07 ***
    ## MSOADECILE4           0.16303    0.02973   5.484 4.15e-08 ***
    ## MSOADECILE5           0.21660    0.02994   7.234 4.70e-13 ***
    ## MSOADECILE6           0.29380    0.03026   9.709  < 2e-16 ***
    ## MSOADECILE7           0.38571    0.03025  12.751  < 2e-16 ***
    ## MSOADECILE8           0.42094    0.03063  13.742  < 2e-16 ***
    ## MSOADECILE9           0.47381    0.03101  15.277  < 2e-16 ***
    ## MSOADECILE10          0.49443    0.03102  15.939  < 2e-16 ***
    ## Mixed.               10.08795    0.85744  11.765  < 2e-16 ***
    ## Asian.               -0.23081    0.07806  -2.957  0.00311 ** 
    ## Black.               -3.40070    0.33927 -10.024  < 2e-16 ***
    ## Other.               -3.73419    0.90287  -4.136 3.54e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4300     0.3743   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6151 on 16 Df
    ## Pseudo R-squared: 0.08783
    ## Number of iterations: 24 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -12269.03

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
    ## -5.3767 -0.6801 -0.1680  0.4743 14.0563 
    ## 
    ## Coefficients (mean model with logit link):
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.00899    0.02491  40.503  < 2e-16 ***
    ## X3sfca_30_normalized -0.18755    0.05929  -3.163  0.00156 ** 
    ## MSOADECILE2           0.07561    0.02860   2.644  0.00819 ** 
    ## MSOADECILE3           0.15186    0.02980   5.096 3.47e-07 ***
    ## MSOADECILE4           0.16770    0.02973   5.641 1.69e-08 ***
    ## MSOADECILE5           0.22102    0.02994   7.383 1.55e-13 ***
    ## MSOADECILE6           0.29932    0.03027   9.890  < 2e-16 ***
    ## MSOADECILE7           0.39174    0.03028  12.938  < 2e-16 ***
    ## MSOADECILE8           0.42702    0.03066  13.926  < 2e-16 ***
    ## MSOADECILE9           0.48207    0.03108  15.511  < 2e-16 ***
    ## MSOADECILE10          0.50509    0.03114  16.218  < 2e-16 ***
    ## Mixed.               10.28701    0.85817  11.987  < 2e-16 ***
    ## Asian.               -0.22639    0.07804  -2.901  0.00372 ** 
    ## Black.               -3.38684    0.33915  -9.986  < 2e-16 ***
    ## Other.               -3.79429    0.90299  -4.202 2.65e-05 ***
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.4532     0.3747   54.58   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6154 on 16 Df
    ## Pseudo R-squared: 0.08852
    ## Number of iterations: 24 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -12275.48
