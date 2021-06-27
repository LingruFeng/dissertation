Beta regression between vaccine rate \~ accessibility + IMD decile +
ethnic composition (Exlude London)
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
    ## -14.4205  -0.4792  -0.0691   0.4300   7.7441 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4435517  0.0202895  71.148  < 2e-16 ***
    ## X2sfca_10_normalized  0.1365340  0.0496881   2.748  0.00600 ** 
    ## IMD19.SCORE          -0.0068948  0.0005574 -12.369  < 2e-16 ***
    ## Mixed.                6.4125290  0.8902530   7.203 5.89e-13 ***
    ## Asian.               -0.2232939  0.0804372  -2.776  0.00550 ** 
    ## Black.               -2.3700408  0.3423225  -6.923 4.41e-12 ***
    ## Other.               -2.6685554  0.9156909  -2.914  0.00357 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6548     0.2299   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4795 on 8 Df
    ## Pseudo R-squared: 0.07919
    ## Number of iterations: 14 (BFGS) + 3 (Fisher scoring)

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
    ## -14.4602  -0.4772  -0.0751   0.4290   7.6098 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4468055  0.0202546  71.431  < 2e-16 ***
    ## X2sfca_20_normalized  0.1971983  0.0792357   2.489  0.01282 *  
    ## IMD19.SCORE          -0.0068404  0.0005627 -12.156  < 2e-16 ***
    ## Mixed.                6.4238379  0.8918219   7.203 5.89e-13 ***
    ## Asian.               -0.2262454  0.0805518  -2.809  0.00497 ** 
    ## Black.               -2.3528328  0.3421965  -6.876 6.17e-12 ***
    ## Other.               -2.5900555  0.9169527  -2.825  0.00473 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6521     0.2299   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4794 on 8 Df
    ## Pseudo R-squared: 0.07873
    ## Number of iterations: 15 (BFGS) + 2 (Fisher scoring)

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
    ## -14.4358  -0.4792  -0.0749   0.4292   7.5875 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4596119  0.0193119  75.581  < 2e-16 ***
    ## X2sfca_25_normalized  0.0974498  0.0595874   1.635  0.10196    
    ## IMD19.SCORE          -0.0069161  0.0005653 -12.235  < 2e-16 ***
    ## Mixed.                6.6052059  0.8886858   7.433 1.07e-13 ***
    ## Asian.               -0.2243205  0.0806113  -2.783  0.00539 ** 
    ## Black.               -2.3670434  0.3426300  -6.908 4.90e-12 ***
    ## Other.               -2.6077304  0.9179070  -2.841  0.00450 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6439     0.2297   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4793 on 8 Df
    ## Pseudo R-squared: 0.07812
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

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
    ## -14.3977  -0.4785  -0.0722   0.4287   7.5758 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4741844  0.0190829  77.252  < 2e-16 ***
    ## X2sfca_30_normalized  0.0083003  0.0474530   0.175  0.86114    
    ## IMD19.SCORE          -0.0070952  0.0005689 -12.472  < 2e-16 ***
    ## Mixed.                6.7997316  0.8895569   7.644 2.11e-14 ***
    ## Asian.               -0.2183335  0.0806798  -2.706  0.00681 ** 
    ## Black.               -2.3565013  0.3429803  -6.871 6.39e-12 ***
    ## Other.               -2.6452061  0.9189686  -2.878  0.00400 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6382     0.2296   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4791 on 8 Df
    ## Pseudo R-squared: 0.07798
    ## Number of iterations: 15 (BFGS) + 3 (Fisher scoring)

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
    ## -14.3945  -0.4770  -0.0736   0.4315   7.5979 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4714390  0.0182455  80.647  < 2e-16 ***
    ## E2sfca_10_normalized  0.0343903  0.0598319   0.575  0.56544    
    ## IMD19.SCORE          -0.0070968  0.0005526 -12.843  < 2e-16 ***
    ## Mixed.                6.7341271  0.8935673   7.536 4.84e-14 ***
    ## Asian.               -0.2190703  0.0805661  -2.719  0.00655 ** 
    ## Black.               -2.3582253  0.3427372  -6.881 5.96e-12 ***
    ## Other.               -2.6520107  0.9179932  -2.889  0.00387 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6389     0.2296   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4791 on 8 Df
    ## Pseudo R-squared: 0.07817
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

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
    ## -14.4109  -0.4779  -0.0711   0.4297   7.6516 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                       Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.454019   0.019812  73.390  < 2e-16 ***
    ## E2sfca_15_normalized  0.100240   0.050000   2.005  0.04498 *  
    ## IMD19.SCORE          -0.006993   0.000555 -12.600  < 2e-16 ***
    ## Mixed.                6.452091   0.897153   7.192 6.40e-13 ***
    ## Asian.               -0.224394   0.080502  -2.787  0.00531 ** 
    ## Black.               -2.364492   0.342558  -6.902 5.11e-12 ***
    ## Other.               -2.651728   0.915924  -2.895  0.00379 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6470     0.2298   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4793 on 8 Df
    ## Pseudo R-squared: 0.0787
    ## Number of iterations: 14 (BFGS) + 3 (Fisher scoring)

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
    ## -14.4568  -0.4814  -0.0718   0.4267   7.6494 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4346820  0.0211071  67.971  < 2e-16 ***
    ## E2sfca_20_normalized  0.2041237  0.0650271   3.139  0.00169 ** 
    ## IMD19.SCORE          -0.0068642  0.0005576 -12.311  < 2e-16 ***
    ## Mixed.                6.1926674  0.8983292   6.894 5.44e-12 ***
    ## Asian.               -0.2305797  0.0804605  -2.866  0.00416 ** 
    ## Black.               -2.3682834  0.3422723  -6.919 4.54e-12 ***
    ## Other.               -2.6370205  0.9140549  -2.885  0.00391 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)    12.66       0.23   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4796 on 8 Df
    ## Pseudo R-squared: 0.07927
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

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
    ## -14.4957  -0.4802  -0.0749   0.4280   7.6251 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4253029  0.0216892  65.715  < 2e-16 ***
    ## E2sfca_25_normalized  0.2799782  0.0776748   3.604 0.000313 ***
    ## IMD19.SCORE          -0.0067735  0.0005601 -12.092  < 2e-16 ***
    ## Mixed.                6.0845104  0.8980813   6.775 1.24e-11 ***
    ## Asian.               -0.2342539  0.0804673  -2.911 0.003601 ** 
    ## Black.               -2.3678381  0.3420644  -6.922 4.45e-12 ***
    ## Other.               -2.6149932  0.9135733  -2.862 0.004205 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6666     0.2301   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4798 on 8 Df
    ## Pseudo R-squared: 0.07943
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

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
    ## -14.5043  -0.4782  -0.0733   0.4297   7.6151 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4274333  0.0216384  65.968  < 2e-16 ***
    ## E2sfca_30_normalized  0.2849220  0.0819369   3.477 0.000506 ***
    ## IMD19.SCORE          -0.0067426  0.0005626 -11.986  < 2e-16 ***
    ## Mixed.                6.1307175  0.8968922   6.836 8.17e-12 ***
    ## Asian.               -0.2343634  0.0805051  -2.911 0.003601 ** 
    ## Black.               -2.3686044  0.3420481  -6.925 4.37e-12 ***
    ## Other.               -2.5978643  0.9142884  -2.841 0.004492 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6647     0.2301   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4797 on 8 Df
    ## Pseudo R-squared: 0.07914
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

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
    ## -14.4088  -0.4814  -0.0734   0.4300   7.6277 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4656742  0.0183133  80.034  < 2e-16 ***
    ## X3sfca_10_normalized  0.0774347  0.0597609   1.296  0.19506    
    ## IMD19.SCORE          -0.0070429  0.0005543 -12.707  < 2e-16 ***
    ## Mixed.                6.6381884  0.8910716   7.450 9.36e-14 ***
    ## Asian.               -0.2189661  0.0804971  -2.720  0.00652 ** 
    ## Black.               -2.3576396  0.3425564  -6.882 5.88e-12 ***
    ## Other.               -2.6345979  0.9174721  -2.872  0.00408 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6419     0.2297   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4792 on 8 Df
    ## Pseudo R-squared: 0.07853
    ## Number of iterations: 14 (BFGS) + 3 (Fisher scoring)

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
    ## -14.4061  -0.4771  -0.0747   0.4302   7.6179 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4636184  0.0195043  75.041  < 2e-16 ***
    ## X3sfca_15_normalized  0.0640968  0.0540470   1.186  0.23564    
    ## IMD19.SCORE          -0.0070048  0.0005595 -12.520  < 2e-16 ***
    ## Mixed.                6.6311082  0.8937758   7.419 1.18e-13 ***
    ## Asian.               -0.2196844  0.0805256  -2.728  0.00637 ** 
    ## Black.               -2.3566418  0.3425727  -6.879 6.02e-12 ***
    ## Other.               -2.6280472  0.9177336  -2.864  0.00419 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6412     0.2297   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4792 on 8 Df
    ## Pseudo R-squared: 0.07835
    ## Number of iterations: 14 (BFGS) + 3 (Fisher scoring)

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
    ## -14.4074  -0.4778  -0.0745   0.4287   7.5947 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4637866  0.0201968  72.476  < 2e-16 ***
    ## X3sfca_20_normalized  0.0625957  0.0600871   1.042  0.29753    
    ## IMD19.SCORE          -0.0069928  0.0005643 -12.393  < 2e-16 ***
    ## Mixed.                6.6487442  0.8948019   7.430 1.08e-13 ***
    ## Asian.               -0.2200619  0.0805576  -2.732  0.00630 ** 
    ## Black.               -2.3570819  0.3425991  -6.880 5.99e-12 ***
    ## Other.               -2.6258048  0.9180836  -2.860  0.00424 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6405     0.2297   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4792 on 8 Df
    ## Pseudo R-squared: 0.0781
    ## Number of iterations: 15 (BFGS) + 3 (Fisher scoring)

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
    ## -14.4049  -0.4766  -0.0741   0.4281   7.5786 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4676080  0.0206327  71.130  < 2e-16 ***
    ## X3sfca_25_normalized  0.0422621  0.0631899   0.669  0.50362    
    ## IMD19.SCORE          -0.0070279  0.0005679 -12.375  < 2e-16 ***
    ## Mixed.                6.7099144  0.8955440   7.493 6.75e-14 ***
    ## Asian.               -0.2194259  0.0805923  -2.723  0.00648 ** 
    ## Black.               -2.3568876  0.3426721  -6.878 6.07e-12 ***
    ## Other.               -2.6311797  0.9186191  -2.864  0.00418 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6391     0.2296   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4791 on 8 Df
    ## Pseudo R-squared: 0.07794
    ## Number of iterations: 14 (BFGS) + 2 (Fisher scoring)

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
    ## -14.3952  -0.4774  -0.0723   0.4284   7.5736 
    ## 
    ## Coefficients (mean model with loglog link):
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)           1.4747334  0.0208864  70.607  < 2e-16 ***
    ## X3sfca_30_normalized  0.0054891  0.0635020   0.086  0.93112    
    ## IMD19.SCORE          -0.0071068  0.0005706 -12.454  < 2e-16 ***
    ## Mixed.                6.8071876  0.8966196   7.592 3.15e-14 ***
    ## Asian.               -0.2178593  0.0806215  -2.702  0.00689 ** 
    ## Black.               -2.3553109  0.3427799  -6.871 6.37e-12 ***
    ## Other.               -2.6469484  0.9192477  -2.879  0.00398 ** 
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  12.6382     0.2296   55.04   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  4791 on 8 Df
    ## Pseudo R-squared: 0.078
    ## Number of iterations: 15 (BFGS) + 3 (Fisher scoring)
