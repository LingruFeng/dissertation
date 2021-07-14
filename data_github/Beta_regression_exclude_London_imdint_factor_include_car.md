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
scfa2_10 = betareg(vaccination_rate_reshape ~ X2sfca_10_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_10_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4364 -0.6875 -0.1701  0.4816 14.5580 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.24355    0.06901   3.529
    ## X2sfca_10_normalized                                 0.08445    0.04002   2.110
    ## MSOADECILE2                                         -0.03925    0.02985  -1.315
    ## MSOADECILE3                                         -0.02901    0.03292  -0.881
    ## MSOADECILE4                                         -0.07067    0.03510  -2.013
    ## MSOADECILE5                                         -0.05186    0.03714  -1.396
    ## MSOADECILE6                                         -0.01039    0.03910  -0.266
    ## MSOADECILE7                                          0.04585    0.04117   1.114
    ## MSOADECILE8                                          0.06478    0.04268   1.518
    ## MSOADECILE9                                          0.09445    0.04379   2.157
    ## MSOADECILE10                                         0.08063    0.04574   1.763
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.22239    0.10613  11.518
    ## Mixed.                                              11.24027    0.85868  13.090
    ## Asian.                                              -0.31116    0.07765  -4.007
    ## Black.                                              -3.48708    0.33544 -10.396
    ## Other.                                              -0.77611    0.94361  -0.822
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         0.000417 ***
    ## X2sfca_10_normalized                                0.034848 *  
    ## MSOADECILE2                                         0.188529    
    ## MSOADECILE3                                         0.378137    
    ## MSOADECILE4                                         0.044089 *  
    ## MSOADECILE5                                         0.162602    
    ## MSOADECILE6                                         0.790533    
    ## MSOADECILE7                                         0.265490    
    ## MSOADECILE8                                         0.129067    
    ## MSOADECILE9                                         0.031029 *  
    ## MSOADECILE10                                        0.077950 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              6.14e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                              0.410796    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8872     0.3828   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6214 on 17 Df
    ## Pseudo R-squared: 0.1012
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -12394.12

## 15 miles

``` r
scfa2_15 = betareg(vaccination_rate_reshape ~ X2sfca_15_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_15_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4352 -0.6893 -0.1697  0.4857 14.5298 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.253266   0.068861
    ## X2sfca_15_normalized                                 0.087474   0.073823
    ## MSOADECILE2                                         -0.036558   0.029819
    ## MSOADECILE3                                         -0.025401   0.032875
    ## MSOADECILE4                                         -0.065876   0.035008
    ## MSOADECILE5                                         -0.047930   0.037077
    ## MSOADECILE6                                         -0.004788   0.038991
    ## MSOADECILE7                                          0.049727   0.041121
    ## MSOADECILE8                                          0.070182   0.042586
    ## MSOADECILE9                                          0.099618   0.043720
    ## MSOADECILE10                                         0.087906   0.045611
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.210535   0.105983
    ## Mixed.                                              11.374754   0.858027
    ## Asian.                                              -0.311450   0.077733
    ## Black.                                              -3.493428   0.335619
    ## Other.                                              -0.753284   0.944257
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.678 0.000235 ***
    ## X2sfca_15_normalized                                  1.185 0.236049    
    ## MSOADECILE2                                          -1.226 0.220198    
    ## MSOADECILE3                                          -0.773 0.439730    
    ## MSOADECILE4                                          -1.882 0.059870 .  
    ## MSOADECILE5                                          -1.293 0.196108    
    ## MSOADECILE6                                          -0.123 0.902260    
    ## MSOADECILE7                                           1.209 0.226557    
    ## MSOADECILE8                                           1.648 0.099352 .  
    ## MSOADECILE9                                           2.279 0.022694 *  
    ## MSOADECILE10                                          1.927 0.053941 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.422  < 2e-16 ***
    ## Mixed.                                               13.257  < 2e-16 ***
    ## Asian.                                               -4.007 6.16e-05 ***
    ## Black.                                              -10.409  < 2e-16 ***
    ## Other.                                               -0.798 0.425014    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8766     0.3826   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6213 on 17 Df
    ## Pseudo R-squared: 0.1008
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -12391.18

## 20 mmiles

``` r
scfa2_20 = betareg(vaccination_rate_reshape ~ X2sfca_20_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_20_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4328 -0.6918 -0.1698  0.4826 14.5411 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                       Estimate Std. Error
    ## (Intercept)                                          0.2661409  0.0684151
    ## X2sfca_20_normalized                                -0.0639456  0.0674604
    ## MSOADECILE2                                         -0.0362662  0.0298211
    ## MSOADECILE3                                         -0.0234575  0.0328564
    ## MSOADECILE4                                         -0.0633368  0.0349950
    ## MSOADECILE5                                         -0.0453493  0.0370567
    ## MSOADECILE6                                         -0.0006802  0.0389393
    ## MSOADECILE7                                          0.0537265  0.0410853
    ## MSOADECILE8                                          0.0747823  0.0425298
    ## MSOADECILE9                                          0.1066613  0.0436406
    ## MSOADECILE10                                         0.0968638  0.0454740
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.2138651  0.1061487
    ## Mixed.                                              11.6293225  0.8621744
    ## Asian.                                              -0.3061426  0.0777156
    ## Black.                                              -3.4819373  0.3355384
    ## Other.                                              -0.7842316  0.9449775
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.890   0.0001 ***
    ## X2sfca_20_normalized                                 -0.948   0.3432    
    ## MSOADECILE2                                          -1.216   0.2239    
    ## MSOADECILE3                                          -0.714   0.4753    
    ## MSOADECILE4                                          -1.810   0.0703 .  
    ## MSOADECILE5                                          -1.224   0.2210    
    ## MSOADECILE6                                          -0.017   0.9861    
    ## MSOADECILE7                                           1.308   0.1910    
    ## MSOADECILE8                                           1.758   0.0787 .  
    ## MSOADECILE9                                           2.444   0.0145 *  
    ## MSOADECILE10                                          2.130   0.0332 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.436  < 2e-16 ***
    ## Mixed.                                               13.488  < 2e-16 ***
    ## Asian.                                               -3.939 8.17e-05 ***
    ## Black.                                              -10.377  < 2e-16 ***
    ## Other.                                               -0.830   0.4066    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8746     0.3826   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6212 on 17 Df
    ## Pseudo R-squared: 0.1008
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -12390.71

## 25 miles

``` r
scfa2_25 = betareg(vaccination_rate_reshape ~ X2sfca_25_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa2_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_25_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4346 -0.6927 -0.1652  0.4822 14.5335 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.259988   0.068367
    ## X2sfca_25_normalized                                -0.143972   0.052008
    ## MSOADECILE2                                         -0.036755   0.029807
    ## MSOADECILE3                                         -0.022639   0.032843
    ## MSOADECILE4                                         -0.060707   0.034997
    ## MSOADECILE5                                         -0.044100   0.037038
    ## MSOADECILE6                                          0.001482   0.038923
    ## MSOADECILE7                                          0.055909   0.041071
    ## MSOADECILE8                                          0.076201   0.042506
    ## MSOADECILE9                                          0.109533   0.043600
    ## MSOADECILE10                                         0.101808   0.045434
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.232076   0.106326
    ## Mixed.                                              11.813858   0.859521
    ## Asian.                                              -0.300383   0.077731
    ## Black.                                              -3.451855   0.335565
    ## Other.                                              -0.780462   0.945167
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.803 0.000143 ***
    ## X2sfca_25_normalized                                 -2.768 0.005635 ** 
    ## MSOADECILE2                                          -1.233 0.217539    
    ## MSOADECILE3                                          -0.689 0.490627    
    ## MSOADECILE4                                          -1.735 0.082805 .  
    ## MSOADECILE5                                          -1.191 0.233778    
    ## MSOADECILE6                                           0.038 0.969633    
    ## MSOADECILE7                                           1.361 0.173430    
    ## MSOADECILE8                                           1.793 0.073022 .  
    ## MSOADECILE9                                           2.512 0.011997 *  
    ## MSOADECILE10                                          2.241 0.025039 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.588  < 2e-16 ***
    ## Mixed.                                               13.745  < 2e-16 ***
    ## Asian.                                               -3.864 0.000111 ***
    ## Black.                                              -10.287  < 2e-16 ***
    ## Other.                                               -0.826 0.408951    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   20.899      0.383   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6216 on 17 Df
    ## Pseudo R-squared: 0.1015
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -12397.35

## 30 miles

``` r
scfa2_30 = betareg(vaccination_rate_reshape ~ X2sfca_30_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(scfa2_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X2sfca_30_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4394 -0.6950 -0.1635  0.4824 14.5127 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.248795   0.068381
    ## X2sfca_30_normalized                                -0.197722   0.042744
    ## MSOADECILE2                                         -0.036889   0.029778
    ## MSOADECILE3                                         -0.020812   0.032818
    ## MSOADECILE4                                         -0.058008   0.034969
    ## MSOADECILE5                                         -0.042894   0.037005
    ## MSOADECILE6                                          0.003666   0.038890
    ## MSOADECILE7                                          0.058996   0.041048
    ## MSOADECILE8                                          0.078236   0.042475
    ## MSOADECILE9                                          0.112266   0.043564
    ## MSOADECILE10                                         0.108311   0.045429
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.254822   0.106384
    ## Mixed.                                              11.996949   0.859060
    ## Asian.                                              -0.294385   0.077693
    ## Black.                                              -3.416300   0.335326
    ## Other.                                              -0.781464   0.944977
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.638 0.000274 ***
    ## X2sfca_30_normalized                                 -4.626 3.73e-06 ***
    ## MSOADECILE2                                          -1.239 0.215427    
    ## MSOADECILE3                                          -0.634 0.525971    
    ## MSOADECILE4                                          -1.659 0.097150 .  
    ## MSOADECILE5                                          -1.159 0.246399    
    ## MSOADECILE6                                           0.094 0.924890    
    ## MSOADECILE7                                           1.437 0.150649    
    ## MSOADECILE8                                           1.842 0.065484 .  
    ## MSOADECILE9                                           2.577 0.009965 ** 
    ## MSOADECILE10                                          2.384 0.017118 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.795  < 2e-16 ***
    ## Mixed.                                               13.965  < 2e-16 ***
    ## Asian.                                               -3.789 0.000151 ***
    ## Black.                                              -10.188  < 2e-16 ***
    ## Other.                                               -0.827 0.408256    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9471     0.3839   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6222 on 17 Df
    ## Pseudo R-squared: 0.103
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -12410.54

# E2SFCA

## 10 miles

``` r
E2scfa_10 = betareg(vaccination_rate_reshape ~ E2sfca_10_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_10_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4416 -0.6895 -0.1698  0.4821 14.4459 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.20549    0.06986   2.941
    ## E2sfca_10_normalized                                 0.34558    0.08717   3.964
    ## MSOADECILE2                                         -0.04630    0.02990  -1.549
    ## MSOADECILE3                                         -0.03966    0.03304  -1.200
    ## MSOADECILE4                                         -0.08369    0.03528  -2.372
    ## MSOADECILE5                                         -0.06848    0.03744  -1.829
    ## MSOADECILE6                                         -0.02888    0.03945  -0.732
    ## MSOADECILE7                                          0.02857    0.04149   0.689
    ## MSOADECILE8                                          0.04851    0.04290   1.131
    ## MSOADECILE9                                          0.07691    0.04405   1.746
    ## MSOADECILE10                                         0.06051    0.04604   1.314
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.28321    0.10749  11.937
    ## Mixed.                                              10.99119    0.85726  12.821
    ## Asian.                                              -0.31989    0.07759  -4.123
    ## Black.                                              -3.48035    0.33506 -10.387
    ## Other.                                              -0.68873    0.94261  -0.731
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                          0.00327 ** 
    ## E2sfca_10_normalized                                7.36e-05 ***
    ## MSOADECILE2                                          0.12148    
    ## MSOADECILE3                                          0.23004    
    ## MSOADECILE4                                          0.01769 *  
    ## MSOADECILE5                                          0.06741 .  
    ## MSOADECILE6                                          0.46410    
    ## MSOADECILE7                                          0.49109    
    ## MSOADECILE8                                          0.25816    
    ## MSOADECILE9                                          0.08082 .  
    ## MSOADECILE10                                         0.18874    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              3.74e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                               0.46498    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9272     0.3835   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6220 on 17 Df
    ## Pseudo R-squared: 0.1025
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -12405.36

## 15 miles

``` r
E2scfa_15 = betareg(vaccination_rate_reshape ~ E2sfca_15_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_15_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4403 -0.6871 -0.1727  0.4843 14.5167 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.21509    0.06973   3.085
    ## E2sfca_15_normalized                                 0.20696    0.05989   3.455
    ## MSOADECILE2                                         -0.04402    0.02988  -1.473
    ## MSOADECILE3                                         -0.03629    0.03300  -1.099
    ## MSOADECILE4                                         -0.07990    0.03524  -2.268
    ## MSOADECILE5                                         -0.06305    0.03735  -1.688
    ## MSOADECILE6                                         -0.02279    0.03934  -0.579
    ## MSOADECILE7                                          0.03423    0.04138   0.827
    ## MSOADECILE8                                          0.05341    0.04284   1.247
    ## MSOADECILE9                                          0.08175    0.04400   1.858
    ## MSOADECILE10                                         0.06555    0.04600   1.425
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.26034    0.10690  11.790
    ## Mixed.                                              10.99847    0.86017  12.786
    ## Asian.                                              -0.31899    0.07764  -4.109
    ## Black.                                              -3.47999    0.33521 -10.382
    ## Other.                                              -0.71247    0.94269  -0.756
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                          0.00204 ** 
    ## E2sfca_15_normalized                                 0.00055 ***
    ## MSOADECILE2                                          0.14071    
    ## MSOADECILE3                                          0.27157    
    ## MSOADECILE4                                          0.02336 *  
    ## MSOADECILE5                                          0.09136 .  
    ## MSOADECILE6                                          0.56246    
    ## MSOADECILE7                                          0.40809    
    ## MSOADECILE8                                          0.21252    
    ## MSOADECILE9                                          0.06320 .  
    ## MSOADECILE10                                         0.15417    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              3.98e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                               0.44978    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9138     0.3833   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6218 on 17 Df
    ## Pseudo R-squared: 0.102
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -12401.5

## 20 miles

``` r
E2scfa_20 = betareg(vaccination_rate_reshape ~ E2sfca_20_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_20_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4386 -0.6895 -0.1720  0.4846 14.5554 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.22697    0.06955   3.264
    ## E2sfca_20_normalized                                 0.14490    0.05208   2.782
    ## MSOADECILE2                                         -0.04099    0.02985  -1.373
    ## MSOADECILE3                                         -0.03216    0.03296  -0.976
    ## MSOADECILE4                                         -0.07477    0.03516  -2.126
    ## MSOADECILE5                                         -0.05705    0.03724  -1.532
    ## MSOADECILE6                                         -0.01603    0.03922  -0.409
    ## MSOADECILE7                                          0.04012    0.04129   0.972
    ## MSOADECILE8                                          0.05951    0.04276   1.392
    ## MSOADECILE9                                          0.08767    0.04393   1.996
    ## MSOADECILE10                                         0.07267    0.04592   1.582
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.23898    0.10643  11.641
    ## Mixed.                                              11.07344    0.86242  12.840
    ## Asian.                                              -0.31715    0.07768  -4.082
    ## Black.                                              -3.48537    0.33535 -10.393
    ## Other.                                              -0.72969    0.94305  -0.774
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                           0.0011 ** 
    ## E2sfca_20_normalized                                  0.0054 ** 
    ## MSOADECILE2                                           0.1697    
    ## MSOADECILE3                                           0.3292    
    ## MSOADECILE4                                           0.0335 *  
    ## MSOADECILE5                                           0.1256    
    ## MSOADECILE6                                           0.6828    
    ## MSOADECILE7                                           0.3312    
    ## MSOADECILE8                                           0.1640    
    ## MSOADECILE9                                           0.0460 *  
    ## MSOADECILE10                                          0.1135    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              4.46e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                                0.4391    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   20.899      0.383   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6216 on 17 Df
    ## Pseudo R-squared: 0.1015
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_20)
```

    ## [1] -12397.32

## 25 miles

``` r
E2scfa_25 = betareg(vaccination_rate_reshape ~ E2sfca_25_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other., data = fca)
summary(E2scfa_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_25_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4362 -0.6888 -0.1678  0.4865 14.5514 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.244284   0.069230
    ## E2sfca_25_normalized                                 0.107512   0.063043
    ## MSOADECILE2                                         -0.037954   0.029832
    ## MSOADECILE3                                         -0.027754   0.032920
    ## MSOADECILE4                                         -0.069256   0.035104
    ## MSOADECILE5                                         -0.051232   0.037168
    ## MSOADECILE6                                         -0.008792   0.039120
    ## MSOADECILE7                                          0.046319   0.041214
    ## MSOADECILE8                                          0.066591   0.042686
    ## MSOADECILE9                                          0.095285   0.043856
    ## MSOADECILE10                                         0.082299   0.045821
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.219601   0.106116
    ## Mixed.                                              11.228881   0.864645
    ## Asian.                                              -0.313642   0.077730
    ## Black.                                              -3.488550   0.335505
    ## Other.                                              -0.750497   0.943742
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.529 0.000418 ***
    ## E2sfca_25_normalized                                  1.705 0.088122 .  
    ## MSOADECILE2                                          -1.272 0.203285    
    ## MSOADECILE3                                          -0.843 0.399191    
    ## MSOADECILE4                                          -1.973 0.048508 *  
    ## MSOADECILE5                                          -1.378 0.168089    
    ## MSOADECILE6                                          -0.225 0.822170    
    ## MSOADECILE7                                           1.124 0.261068    
    ## MSOADECILE8                                           1.560 0.118753    
    ## MSOADECILE9                                           2.173 0.029805 *  
    ## MSOADECILE10                                          1.796 0.072480 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.493  < 2e-16 ***
    ## Mixed.                                               12.987  < 2e-16 ***
    ## Asian.                                               -4.035 5.46e-05 ***
    ## Black.                                              -10.398  < 2e-16 ***
    ## Other.                                               -0.795 0.426476    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8819     0.3827   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6213 on 17 Df
    ## Pseudo R-squared: 0.101
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -12392.62

## 30 miles

``` r
E2scfa_30 = betareg(vaccination_rate_reshape ~ E2sfca_30_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,data = fca)
summary(E2scfa_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ E2sfca_30_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4339 -0.6915 -0.1707  0.4829 14.5391 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.260474   0.068845
    ## E2sfca_30_normalized                                 0.025014   0.068606
    ## MSOADECILE2                                         -0.036131   0.029824
    ## MSOADECILE3                                         -0.024459   0.032895
    ## MSOADECILE4                                         -0.064978   0.035066
    ## MSOADECILE5                                         -0.046954   0.037118
    ## MSOADECILE6                                         -0.003011   0.039044
    ## MSOADECILE7                                          0.051425   0.041164
    ## MSOADECILE8                                          0.072389   0.042622
    ## MSOADECILE9                                          0.102559   0.043777
    ## MSOADECILE10                                         0.091657   0.045713
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.209575   0.105996
    ## Mixed.                                              11.443834   0.865796
    ## Asian.                                              -0.309201   0.077761
    ## Black.                                              -3.486515   0.335614
    ## Other.                                              -0.768824   0.944511
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.784 0.000155 ***
    ## E2sfca_30_normalized                                  0.365 0.715408    
    ## MSOADECILE2                                          -1.211 0.225716    
    ## MSOADECILE3                                          -0.744 0.457156    
    ## MSOADECILE4                                          -1.853 0.063881 .  
    ## MSOADECILE5                                          -1.265 0.205876    
    ## MSOADECILE6                                          -0.077 0.938535    
    ## MSOADECILE7                                           1.249 0.211570    
    ## MSOADECILE8                                           1.698 0.089428 .  
    ## MSOADECILE9                                           2.343 0.019140 *  
    ## MSOADECILE10                                          2.005 0.044959 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.411  < 2e-16 ***
    ## Mixed.                                               13.218  < 2e-16 ***
    ## Asian.                                               -3.976    7e-05 ***
    ## Black.                                              -10.388  < 2e-16 ***
    ## Other.                                               -0.814 0.415650    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8719     0.3825   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6212 on 17 Df
    ## Pseudo R-squared: 0.1007
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -12389.95

# 3SFCA

## 10 miles

``` r
scfa3_10 = betareg(vaccination_rate_reshape ~ X3sfca_10_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
data = fca)
summary(scfa3_10)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_10_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4428 -0.6909 -0.1655  0.4823 14.4600 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.21582    0.06923   3.117
    ## X3sfca_10_normalized                                 0.33481    0.07937   4.218
    ## MSOADECILE2                                         -0.04496    0.02986  -1.506
    ## MSOADECILE3                                         -0.03756    0.03296  -1.140
    ## MSOADECILE4                                         -0.08146    0.03517  -2.316
    ## MSOADECILE5                                         -0.06538    0.03729  -1.753
    ## MSOADECILE6                                         -0.02620    0.03928  -0.667
    ## MSOADECILE7                                          0.03152    0.04134   0.763
    ## MSOADECILE8                                          0.05122    0.04276   1.198
    ## MSOADECILE9                                          0.07931    0.04390   1.807
    ## MSOADECILE10                                         0.06253    0.04585   1.364
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.26766    0.10675  11.875
    ## Mixed.                                              10.97057    0.85696  12.802
    ## Asian.                                              -0.31444    0.07753  -4.056
    ## Black.                                              -3.47507    0.33495 -10.375
    ## Other.                                              -0.66877    0.94267  -0.709
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                          0.00182 ** 
    ## X3sfca_10_normalized                                2.46e-05 ***
    ## MSOADECILE2                                          0.13210    
    ## MSOADECILE3                                          0.25445    
    ## MSOADECILE4                                          0.02055 *  
    ## MSOADECILE5                                          0.07959 .  
    ## MSOADECILE6                                          0.50479    
    ## MSOADECILE7                                          0.44567    
    ## MSOADECILE8                                          0.23102    
    ## MSOADECILE9                                          0.07081 .  
    ## MSOADECILE10                                         0.17264    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              5.00e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                               0.47805    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9355     0.3837   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6221 on 17 Df
    ## Pseudo R-squared: 0.1028
    ## Number of iterations: 24 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -12407.65

## 15 miles

``` r
scfa3_15 = betareg(vaccination_rate_reshape ~ X3sfca_15_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_15)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_15_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4377 -0.6880 -0.1695  0.4869 14.4688 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.23758    0.06900   3.443
    ## X3sfca_15_normalized                                 0.15474    0.05679   2.725
    ## MSOADECILE2                                         -0.04124    0.02986  -1.381
    ## MSOADECILE3                                         -0.03223    0.03297  -0.978
    ## MSOADECILE4                                         -0.07543    0.03520  -2.143
    ## MSOADECILE5                                         -0.05800    0.03729  -1.555
    ## MSOADECILE6                                         -0.01659    0.03926  -0.423
    ## MSOADECILE7                                          0.03951    0.04133   0.956
    ## MSOADECILE8                                          0.05929    0.04278   1.386
    ## MSOADECILE9                                          0.08772    0.04394   1.997
    ## MSOADECILE10                                         0.07338    0.04589   1.599
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.23573    0.10636  11.618
    ## Mixed.                                              11.11537    0.86062  12.916
    ## Asian.                                              -0.31352    0.07764  -4.038
    ## Black.                                              -3.47623    0.33531 -10.367
    ## Other.                                              -0.70522    0.94372  -0.747
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         0.000575 ***
    ## X3sfca_15_normalized                                0.006433 ** 
    ## MSOADECILE2                                         0.167281    
    ## MSOADECILE3                                         0.328315    
    ## MSOADECILE4                                         0.032138 *  
    ## MSOADECILE5                                         0.119848    
    ## MSOADECILE6                                         0.672652    
    ## MSOADECILE7                                         0.339069    
    ## MSOADECILE8                                         0.165730    
    ## MSOADECILE9                                         0.045878 *  
    ## MSOADECILE10                                        0.109827    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              5.39e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                              0.454897    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)   20.898      0.383   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6216 on 17 Df
    ## Pseudo R-squared: 0.1017
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -12397.19

## 20 miles

``` r
scfa3_20 = betareg(vaccination_rate_reshape ~ X3sfca_20_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_20)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_20_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4338 -0.6910 -0.1703  0.4832 14.5286 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.258149   0.068731
    ## X3sfca_20_normalized                                 0.042121   0.055214
    ## MSOADECILE2                                         -0.037099   0.029856
    ## MSOADECILE3                                         -0.025854   0.032950
    ## MSOADECILE4                                         -0.066963   0.035179
    ## MSOADECILE5                                         -0.048918   0.037229
    ## MSOADECILE6                                         -0.005427   0.039185
    ## MSOADECILE7                                          0.049345   0.041271
    ## MSOADECILE8                                          0.070012   0.042732
    ## MSOADECILE9                                          0.099992   0.043888
    ## MSOADECILE10                                         0.088403   0.045854
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.212847   0.106092
    ## Mixed.                                              11.391148   0.862958
    ## Asian.                                              -0.309716   0.077715
    ## Black.                                              -3.483793   0.335524
    ## Other.                                              -0.756000   0.944667
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.756 0.000173 ***
    ## X3sfca_20_normalized                                  0.763 0.445542    
    ## MSOADECILE2                                          -1.243 0.214023    
    ## MSOADECILE3                                          -0.785 0.432660    
    ## MSOADECILE4                                          -1.903 0.056980 .  
    ## MSOADECILE5                                          -1.314 0.188860    
    ## MSOADECILE6                                          -0.138 0.889852    
    ## MSOADECILE7                                           1.196 0.231843    
    ## MSOADECILE8                                           1.638 0.101339    
    ## MSOADECILE9                                           2.278 0.022705 *  
    ## MSOADECILE10                                          1.928 0.053866 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.432  < 2e-16 ***
    ## Mixed.                                               13.200  < 2e-16 ***
    ## Asian.                                               -3.985 6.74e-05 ***
    ## Black.                                              -10.383  < 2e-16 ***
    ## Other.                                               -0.800 0.423548    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8734     0.3825   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6212 on 17 Df
    ## Pseudo R-squared: 0.1008
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_20)
```

    ## [1] -12390.4

## 25 miles

``` r
scfa3_25 = betareg(vaccination_rate_reshape ~ X3sfca_25_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_25)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_25_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4329 -0.6916 -0.1707  0.4800 14.5355 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.269174   0.068546
    ## X3sfca_25_normalized                                -0.054872   0.050052
    ## MSOADECILE2                                         -0.034575   0.029839
    ## MSOADECILE3                                         -0.021417   0.032923
    ## MSOADECILE4                                         -0.060660   0.035129
    ## MSOADECILE5                                         -0.042974   0.037164
    ## MSOADECILE6                                          0.002425   0.039107
    ## MSOADECILE7                                          0.056255   0.041211
    ## MSOADECILE8                                          0.077766   0.042672
    ## MSOADECILE9                                          0.109430   0.043824
    ## MSOADECILE10                                         0.100899   0.045797
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.207815   0.105989
    ## Mixed.                                              11.663020   0.864374
    ## Asian.                                              -0.305558   0.077734
    ## Black.                                              -3.483246   0.335516
    ## Other.                                              -0.793741   0.945189
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.927 8.61e-05 ***
    ## X3sfca_25_normalized                                 -1.096   0.2730    
    ## MSOADECILE2                                          -1.159   0.2466    
    ## MSOADECILE3                                          -0.651   0.5154    
    ## MSOADECILE4                                          -1.727   0.0842 .  
    ## MSOADECILE5                                          -1.156   0.2475    
    ## MSOADECILE6                                           0.062   0.9506    
    ## MSOADECILE7                                           1.365   0.1722    
    ## MSOADECILE8                                           1.822   0.0684 .  
    ## MSOADECILE9                                           2.497   0.0125 *  
    ## MSOADECILE10                                          2.203   0.0276 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.396  < 2e-16 ***
    ## Mixed.                                               13.493  < 2e-16 ***
    ## Asian.                                               -3.931 8.47e-05 ***
    ## Black.                                              -10.382  < 2e-16 ***
    ## Other.                                               -0.840   0.4010    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8758     0.3826   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6213 on 17 Df
    ## Pseudo R-squared: 0.1008
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -12391.01

## 30 miles

``` r
scfa3_30 = betareg(vaccination_rate_reshape ~ X3sfca_30_normalized + MSOADECILE + Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + Black. + Other.,
                data = fca)
summary(scfa3_30)
```

    ## 
    ## Call:
    ## betareg(formula = vaccination_rate_reshape ~ X3sfca_30_normalized + MSOADECILE + 
    ##     Per_cent_of_households_with_at_least_one_car_or_van + Mixed. + Asian. + 
    ##     Black. + Other., data = fca)
    ## 
    ## Standardized weighted residuals 2:
    ##     Min      1Q  Median      3Q     Max 
    ## -5.4348 -0.6932 -0.1674  0.4852 14.5173 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.274125   0.068407
    ## X3sfca_30_normalized                                -0.154164   0.052520
    ## MSOADECILE2                                         -0.033335   0.029813
    ## MSOADECILE3                                         -0.018314   0.032887
    ## MSOADECILE4                                         -0.056200   0.035075
    ## MSOADECILE5                                         -0.039088   0.037107
    ## MSOADECILE6                                          0.007928   0.039033
    ## MSOADECILE7                                          0.061342   0.041155
    ## MSOADECILE8                                          0.083268   0.042610
    ## MSOADECILE9                                          0.117090   0.043763
    ## MSOADECILE10                                         0.111801   0.045737
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.214459   0.105961
    ## Mixed.                                              11.937089   0.865476
    ## Asian.                                              -0.301374   0.077705
    ## Black.                                              -3.474287   0.335345
    ## Other.                                              -0.834342   0.945300
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           4.007 6.14e-05 ***
    ## X3sfca_30_normalized                                 -2.935 0.003332 ** 
    ## MSOADECILE2                                          -1.118 0.263517    
    ## MSOADECILE3                                          -0.557 0.577618    
    ## MSOADECILE4                                          -1.602 0.109093    
    ## MSOADECILE5                                          -1.053 0.292167    
    ## MSOADECILE6                                           0.203 0.839042    
    ## MSOADECILE7                                           1.491 0.136090    
    ## MSOADECILE8                                           1.954 0.050678 .  
    ## MSOADECILE9                                           2.676 0.007461 ** 
    ## MSOADECILE10                                          2.444 0.014509 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.461  < 2e-16 ***
    ## Mixed.                                               13.793  < 2e-16 ***
    ## Asian.                                               -3.878 0.000105 ***
    ## Black.                                              -10.360  < 2e-16 ***
    ## Other.                                               -0.883 0.377441    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9022     0.3831   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6216 on 17 Df
    ## Pseudo R-squared: 0.1016
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -12398.24
