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

``` r
head(fca)
```

    ##   LA_Code_2020 LA_name_2020  MSOA11CD   MSOA11NM vaccination_rate_60.69
    ## 1    E08000001       Bolton E02000984 Bolton 001              0.9244250
    ## 2    E08000001       Bolton E02000985 Bolton 002              0.8627244
    ## 3    E08000001       Bolton E02000986 Bolton 003              0.8751394
    ## 4    E08000001       Bolton E02000987 Bolton 004              0.9175127
    ## 5    E08000001       Bolton E02000988 Bolton 005              0.8581731
    ## 6    E08000001       Bolton E02000989 Bolton 006              0.8353488
    ##   X2sfca_10_normalized X2sfca_15_normalized X2sfca_20_normalized
    ## 1            0.1883586            0.1308648            0.1377971
    ## 2            0.1883586            0.1114452            0.1345836
    ## 3            0.1883586            0.1492922            0.1608479
    ## 4            0.2803523            0.1308045            0.1216605
    ## 5            0.1883586            0.1631090            0.1608479
    ## 6            0.1883586            0.1289626            0.1651710
    ##   X2sfca_25_normalized X2sfca_30_normalized E2sfca_10_normalized
    ## 1            0.1831588            0.1753636           0.17532189
    ## 2            0.1779780            0.1715311           0.14050745
    ## 3            0.2020778            0.1827658           0.15872148
    ## 4            0.1557064            0.1824680           0.08174672
    ## 5            0.2054264            0.2004080           0.16504389
    ## 6            0.1795343            0.1740994           0.13599317
    ##   E2sfca_15_normalized E2sfca_20_normalized E2sfca_25_normalized
    ## 1            0.2408040            0.1982467            0.1782222
    ## 2            0.2183409            0.1873824            0.1692326
    ## 3            0.2382942            0.2088240            0.1907568
    ## 4            0.2152038            0.2033797            0.1770977
    ## 5            0.2511792            0.2243475            0.2044065
    ## 6            0.2092465            0.1913222            0.1794907
    ##   E2sfca_30_normalized X3sfca_10_normalized X3sfca_15_normalized
    ## 1            0.1727624            0.1252273            0.1847983
    ## 2            0.1646716            0.1263457            0.1661307
    ## 3            0.1844549            0.1337630            0.1708205
    ## 4            0.1644445            0.0481584            0.1355276
    ## 5            0.1958458            0.1432557            0.1685240
    ## 6            0.1765988            0.1369282            0.1630848
    ##   X3sfca_20_normalized X3sfca_25_normalized X3sfca_30_normalized
    ## 1            0.1760775            0.1610311            0.1536948
    ## 2            0.1645415            0.1558005            0.1504570
    ## 3            0.1640745            0.1557533            0.1528387
    ## 4            0.1659422            0.1715500            0.1676815
    ## 5            0.1612793            0.1558707            0.1548629
    ## 6            0.1521903            0.1457924            0.1453543
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   geometry
    ## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              POLYGON ((372121.740692092 414318.5820284747, 372147.1839180924 413616.0947827902, 371702.6454096416 413659.7824457224, 371827.7002863795 413133.9144642824, 371127.5921295869 413520.6338658539, 371044.1355199007 412457.7789452779, 370563.1300719351 412570.4830982488, 370344.1973272361 411735.6423399099, 368805.8519260563 413441.5264223354, 369273.03834101 414007.6864742129, 370082.7246238173 414033.0940174414, 369786.4439685926 414330.5955067142, 370439.1287112203 415133.4772392786, 370328.6379910178 416196.3783576144, 371095.1401160229 416704.8787693487, 371919.4464567718 415472.1040790394, 372121.740692092 414318.5820284747))
    ## 2                                                                                                                                                                                                                                                                                                          POLYGON ((372971.32544739 411456.0757330438, 373104.9660326273 411786.9346117839, 372403.4818300866 411682.3939734377, 372192.2923809371 411876.1650843857, 372549.9492235794 412191.2826706693, 372260.1359181411 412998.7853403708, 372506.840112354 413010.974232301, 372592.4822673436 413615.8473160079, 372503.9486068891 413391.7829738613, 372147.1839180924 413616.0947827902, 372121.740692092 414318.5820284747, 373292.3317096322 414365.1066220087, 373505.652913931 413431.1165834295, 374264.038610657 413116.3574516368, 373775.3448951351 412765.7052222025, 373778.1373174939 412458.7877736727, 373992.4521501801 412633.8743023995, 374145.9236210133 412356.4355328308, 373777.1377712048 412164.7864449316, 374003.8413448701 411866.1018520696, 373390.1380285973 411879.7839072304, 372971.32544739 411456.0757330438))
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                POLYGON ((372147.1839180924 413616.0947827902, 372503.9486068891 413391.7829738613, 372592.4822673436 413615.8473160079, 372506.840112354 413010.974232301, 372260.1359181411 412998.7853403708, 372549.9492235794 412191.2826706693, 372192.2923809371 411876.1650843857, 371567.5700009299 411759.8473628205, 371044.1355199007 412457.7789452779, 371127.5921295869 413520.6338658539, 371827.7002863795 413133.9144642824, 371702.6454096416 413659.7824457224, 372147.1839180924 413616.0947827902))
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              POLYGON ((363078.5560578937 411480.5286678746, 363041.920424702 411516.8692610519, 363593.9119929793 412513.5128240865, 364294.0656929562 412769.050901489, 364577.5179104541 411472.6705110331, 363933.33884928 411417.4733997922, 364071.0460017346 410956.528897219, 363645.4667881263 410864.7453871562, 363344.7899978039 411020.0870752722, 363498.5323147997 411509.986285847, 363078.5560578937 411480.5286678746))
    ## 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                POLYGON ((371044.1355199007 412457.7789452779, 371567.5700009299 411759.8473628205, 372192.2923809371 411876.1650843857, 372403.4818300866 411682.3939734377, 372015.1375330461 411660.0904985466, 372346.6384834584 411171.0894849913, 371961.5443420812 411112.2768025493, 372068.6690501694 410687.1862619451, 371852.1392506702 410478.772534886, 371727.0995036518 410936.8941232641, 371489.1389071815 410639.7718630652, 371488.1994683027 411222.4107704869, 370664.1461853441 411366.8416224947, 370344.1973272361 411735.6423399099, 370563.1300719351 412570.4830982488, 371044.1355199007 412457.7789452779))
    ## 6 POLYGON ((375025.5385550009 414993.896433391, 374739.5421874931 414406.6108748912, 375207.5380997168 414012.9877312119, 375000.1361411166 413832.2031654804, 376080.2083505784 412704.2709914471, 375847.3883579105 411760.1339557605, 375722.6377981862 411119.6920298627, 375150.9981421491 410811.5879417898, 375204.6383989609 410110.1747155235, 374753.5930790966 410126.8584318775, 374751.4846462585 410701.1453159083, 374263.4197291153 410630.216265487, 374156.8351022587 410258.182940204, 374113.6400539136 410934.6881216902, 373684.3285153967 411054.2832090846, 372997.3308984101 411161.7825125497, 372971.32544739 411456.0757330438, 373390.1380285973 411879.7839072304, 374003.8413448701 411866.1018520696, 373777.1377712048 412164.7864449316, 374145.9236210133 412356.4355328308, 373992.4521501801 412633.8743023995, 373778.1373174939 412458.7877736727, 373775.3448951351 412765.7052222025, 374264.038610657 413116.3574516368, 373505.652913931 413431.1165834295, 373292.3317096322 414365.1066220087, 373561.6345520437 414304.2016083957, 373788.8537351935 415149.5173290095, 375025.5385550009 414993.896433391))
    ##   IMD19.SCORE MSOADECILE White. Mixed. Asian. Black. Other.
    ## 1    5.686451         10 0.9684 0.0104 0.0177 0.0013 0.0022
    ## 2    8.467447          9 0.9753 0.0098 0.0107 0.0029 0.0013
    ## 3   12.653141          8 0.8818 0.0130 0.0954 0.0053 0.0045
    ## 4   29.730829          3 0.9760 0.0097 0.0085 0.0032 0.0026
    ## 5   32.753429          2 0.7080 0.0205 0.2523 0.0156 0.0037
    ## 6   12.145260          8 0.9768 0.0089 0.0104 0.0024 0.0015
    ##   Per_cent_of_households_with_at_least_one_car_or_van vaccination_rate_reshape
    ## 1                                           0.9421488                0.9243625
    ## 2                                           0.8761310                0.8626710
    ## 3                                           0.8370451                0.8750841
    ## 4                                           0.7128954                0.9174512
    ## 5                                           0.6787757                0.8581203
    ## 6                                           0.8641738                0.8352995

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
    ## -5.4383 -0.6886 -0.1704  0.4837 14.5823 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.242662   0.068817
    ## X2sfca_10_normalized                                 0.119395   0.046365
    ## MSOADECILE2                                         -0.037767   0.029808
    ## MSOADECILE3                                         -0.027768   0.032868
    ## MSOADECILE4                                         -0.069145   0.035014
    ## MSOADECILE5                                         -0.050742   0.037081
    ## MSOADECILE6                                         -0.008928   0.038984
    ## MSOADECILE7                                          0.048066   0.041086
    ## MSOADECILE8                                          0.066718   0.042562
    ## MSOADECILE9                                          0.095134   0.043684
    ## MSOADECILE10                                         0.082007   0.045548
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.211862   0.105927
    ## Mixed.                                              11.155339   0.859259
    ## Asian.                                              -0.312871   0.077621
    ## Black.                                              -3.501081   0.335510
    ## Other.                                              -0.787425   0.943085
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.526 0.000422 ***
    ## X2sfca_10_normalized                                  2.575 0.010021 *  
    ## MSOADECILE2                                          -1.267 0.205155    
    ## MSOADECILE3                                          -0.845 0.398215    
    ## MSOADECILE4                                          -1.975 0.048293 *  
    ## MSOADECILE5                                          -1.368 0.171180    
    ## MSOADECILE6                                          -0.229 0.818856    
    ## MSOADECILE7                                           1.170 0.242053    
    ## MSOADECILE8                                           1.568 0.116984    
    ## MSOADECILE9                                           2.178 0.029425 *  
    ## MSOADECILE10                                          1.800 0.071788 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.441  < 2e-16 ***
    ## Mixed.                                               12.983  < 2e-16 ***
    ## Asian.                                               -4.031 5.56e-05 ***
    ## Black.                                              -10.435  < 2e-16 ***
    ## Other.                                               -0.835 0.403748    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8949     0.3829   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6215 on 17 Df
    ## Pseudo R-squared: 0.1014
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa2_10)
```

    ## [1] -12396.15

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
    ## -5.4363 -0.6884 -0.1677  0.4874 14.5154 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.250820   0.068836
    ## X2sfca_15_normalized                                 0.123428   0.083630
    ## MSOADECILE2                                         -0.036436   0.029813
    ## MSOADECILE3                                         -0.025486   0.032868
    ## MSOADECILE4                                         -0.066349   0.035005
    ## MSOADECILE5                                         -0.048156   0.037067
    ## MSOADECILE6                                         -0.004958   0.038964
    ## MSOADECILE7                                          0.049599   0.041098
    ## MSOADECILE8                                          0.069963   0.042563
    ## MSOADECILE9                                          0.098647   0.043707
    ## MSOADECILE10                                         0.087151   0.045556
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.207830   0.105977
    ## Mixed.                                              11.309862   0.860823
    ## Asian.                                              -0.312777   0.077728
    ## Black.                                              -3.501458   0.335726
    ## Other.                                              -0.748781   0.943973
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.644 0.000269 ***
    ## X2sfca_15_normalized                                  1.476 0.139975    
    ## MSOADECILE2                                          -1.222 0.221646    
    ## MSOADECILE3                                          -0.775 0.438104    
    ## MSOADECILE4                                          -1.895 0.058037 .  
    ## MSOADECILE5                                          -1.299 0.193889    
    ## MSOADECILE6                                          -0.127 0.898753    
    ## MSOADECILE7                                           1.207 0.227486    
    ## MSOADECILE8                                           1.644 0.100224    
    ## MSOADECILE9                                           2.257 0.024006 *  
    ## MSOADECILE10                                          1.913 0.055739 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.397  < 2e-16 ***
    ## Mixed.                                               13.138  < 2e-16 ***
    ## Asian.                                               -4.024 5.72e-05 ***
    ## Black.                                              -10.430  < 2e-16 ***
    ## Other.                                               -0.793 0.427648    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8794     0.3827   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6213 on 17 Df
    ## Pseudo R-squared: 0.1009
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_15)
```

    ## [1] -12391.89

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
    ## -5.4332 -0.6925 -0.1700  0.4838 14.5367 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.263516   0.068423
    ## X2sfca_20_normalized                                 0.001142   0.072850
    ## MSOADECILE2                                         -0.035890   0.029821
    ## MSOADECILE3                                         -0.023860   0.032858
    ## MSOADECILE4                                         -0.064148   0.034999
    ## MSOADECILE5                                         -0.046175   0.037056
    ## MSOADECILE6                                         -0.001903   0.038936
    ## MSOADECILE7                                          0.052436   0.041082
    ## MSOADECILE8                                          0.073483   0.042528
    ## MSOADECILE9                                          0.104092   0.043644
    ## MSOADECILE10                                         0.093697   0.045465
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.208922   0.106190
    ## Mixed.                                              11.500150   0.863758
    ## Asian.                                              -0.308072   0.077739
    ## Black.                                              -3.484779   0.335587
    ## Other.                                              -0.771981   0.944702
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.851 0.000118 ***
    ## X2sfca_20_normalized                                  0.016 0.987496    
    ## MSOADECILE2                                          -1.204 0.228778    
    ## MSOADECILE3                                          -0.726 0.467749    
    ## MSOADECILE4                                          -1.833 0.066827 .  
    ## MSOADECILE5                                          -1.246 0.212740    
    ## MSOADECILE6                                          -0.049 0.961013    
    ## MSOADECILE7                                           1.276 0.201822    
    ## MSOADECILE8                                           1.728 0.084015 .  
    ## MSOADECILE9                                           2.385 0.017077 *  
    ## MSOADECILE10                                          2.061 0.039315 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.384  < 2e-16 ***
    ## Mixed.                                               13.314  < 2e-16 ***
    ## Asian.                                               -3.963  7.4e-05 ***
    ## Black.                                              -10.384  < 2e-16 ***
    ## Other.                                               -0.817 0.413832    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8714     0.3825   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6212 on 17 Df
    ## Pseudo R-squared: 0.1007
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_20)
```

    ## [1] -12389.83

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
    ## -5.4334 -0.6934 -0.1700  0.4816 14.5460 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                       Estimate Std. Error
    ## (Intercept)                                          0.2613154  0.0683903
    ## X2sfca_25_normalized                                -0.1050525  0.0546931
    ## MSOADECILE2                                         -0.0363091  0.0298145
    ## MSOADECILE3                                         -0.0228843  0.0328528
    ## MSOADECILE4                                         -0.0616919  0.0350064
    ## MSOADECILE5                                         -0.0447394  0.0370478
    ## MSOADECILE6                                          0.0004344  0.0389310
    ## MSOADECILE7                                          0.0549819  0.0410826
    ## MSOADECILE8                                          0.0755260  0.0425177
    ## MSOADECILE9                                          0.1082161  0.0436166
    ## MSOADECILE10                                         0.0994730  0.0454459
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.2253105  0.1063796
    ## Mixed.                                              11.7349080  0.8606145
    ## Asian.                                              -0.3015556  0.0777713
    ## Black.                                              -3.4580596  0.3357912
    ## Other.                                              -0.7895871  0.9451616
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.821 0.000133 ***
    ## X2sfca_25_normalized                                 -1.921 0.054762 .  
    ## MSOADECILE2                                          -1.218 0.223286    
    ## MSOADECILE3                                          -0.697 0.486071    
    ## MSOADECILE4                                          -1.762 0.078018 .  
    ## MSOADECILE5                                          -1.208 0.227196    
    ## MSOADECILE6                                           0.011 0.991097    
    ## MSOADECILE7                                           1.338 0.180790    
    ## MSOADECILE8                                           1.776 0.075676 .  
    ## MSOADECILE9                                           2.481 0.013099 *  
    ## MSOADECILE10                                          2.189 0.028610 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.518  < 2e-16 ***
    ## Mixed.                                               13.635  < 2e-16 ***
    ## Asian.                                               -3.877 0.000106 ***
    ## Black.                                              -10.298  < 2e-16 ***
    ## Other.                                               -0.835 0.403493    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8846     0.3828   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6214 on 17 Df
    ## Pseudo R-squared: 0.1011
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_25)
```

    ## [1] -12393.42

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
    ## -5.4374 -0.6965 -0.1648  0.4818 14.5325 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.251288   0.068410
    ## X2sfca_30_normalized                                -0.174098   0.043871
    ## MSOADECILE2                                         -0.036368   0.029790
    ## MSOADECILE3                                         -0.021008   0.032832
    ## MSOADECILE4                                         -0.058582   0.034986
    ## MSOADECILE5                                         -0.043102   0.037022
    ## MSOADECILE6                                          0.003196   0.038907
    ## MSOADECILE7                                          0.058576   0.041068
    ## MSOADECILE8                                          0.077913   0.042493
    ## MSOADECILE9                                          0.112065   0.043590
    ## MSOADECILE10                                         0.107046   0.045459
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.248294   0.106434
    ## Mixed.                                              11.961425   0.860374
    ## Asian.                                              -0.294066   0.077747
    ## Black.                                              -3.423836   0.335529
    ## Other.                                              -0.804351   0.945092
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.673 0.000239 ***
    ## X2sfca_30_normalized                                 -3.968 7.23e-05 ***
    ## MSOADECILE2                                          -1.221 0.222149    
    ## MSOADECILE3                                          -0.640 0.522252    
    ## MSOADECILE4                                          -1.674 0.094042 .  
    ## MSOADECILE5                                          -1.164 0.244322    
    ## MSOADECILE6                                           0.082 0.934530    
    ## MSOADECILE7                                           1.426 0.153777    
    ## MSOADECILE8                                           1.834 0.066717 .  
    ## MSOADECILE9                                           2.571 0.010144 *  
    ## MSOADECILE10                                          2.355 0.018535 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.728  < 2e-16 ***
    ## Mixed.                                               13.903  < 2e-16 ***
    ## Asian.                                               -3.782 0.000155 ***
    ## Black.                                              -10.204  < 2e-16 ***
    ## Other.                                               -0.851 0.394724    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9269     0.3835   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6219 on 17 Df
    ## Pseudo R-squared: 0.1023
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa2_30)
```

    ## [1] -12405

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
    ## -5.4401 -0.6885 -0.1698  0.4840 14.4932 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.21839    0.06962   3.137
    ## E2sfca_10_normalized                                 0.19502    0.05654   3.449
    ## MSOADECILE2                                         -0.04171    0.02984  -1.398
    ## MSOADECILE3                                         -0.03419    0.03296  -1.037
    ## MSOADECILE4                                         -0.07728    0.03515  -2.198
    ## MSOADECILE5                                         -0.06238    0.03733  -1.671
    ## MSOADECILE6                                         -0.02088    0.03925  -0.532
    ## MSOADECILE7                                          0.03693    0.04131   0.894
    ## MSOADECILE8                                          0.05792    0.04272   1.356
    ## MSOADECILE9                                          0.08510    0.04388   1.940
    ## MSOADECILE10                                         0.07047    0.04583   1.537
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.25507    0.10676  11.756
    ## Mixed.                                              11.05811    0.85737  12.898
    ## Asian.                                              -0.31841    0.07760  -4.103
    ## Black.                                              -3.50362    0.33533 -10.448
    ## Other.                                              -0.69794    0.94290  -0.740
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         0.001708 ** 
    ## E2sfca_10_normalized                                0.000562 ***
    ## MSOADECILE2                                         0.162169    
    ## MSOADECILE3                                         0.299586    
    ## MSOADECILE4                                         0.027927 *  
    ## MSOADECILE5                                         0.094684 .  
    ## MSOADECILE6                                         0.594721    
    ## MSOADECILE7                                         0.371298    
    ## MSOADECILE8                                         0.175111    
    ## MSOADECILE9                                         0.052429 .  
    ## MSOADECILE10                                        0.124174    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              4.08e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                              0.459176    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9135     0.3833   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6218 on 17 Df
    ## Pseudo R-squared: 0.1021
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_10)
```

    ## [1] -12401.46

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
    ## -5.4403 -0.6889 -0.1710  0.4871 14.5384 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.22162    0.06954   3.187
    ## E2sfca_15_normalized                                 0.15223    0.04679   3.253
    ## MSOADECILE2                                         -0.04077    0.02983  -1.367
    ## MSOADECILE3                                         -0.03277    0.03294  -0.995
    ## MSOADECILE4                                         -0.07575    0.03513  -2.156
    ## MSOADECILE5                                         -0.05933    0.03725  -1.592
    ## MSOADECILE6                                         -0.01754    0.03917  -0.448
    ## MSOADECILE7                                          0.03993    0.04123   0.968
    ## MSOADECILE8                                          0.05997    0.04268   1.405
    ## MSOADECILE9                                          0.08696    0.04385   1.983
    ## MSOADECILE10                                         0.07224    0.04579   1.577
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.24001    0.10631  11.664
    ## Mixed.                                              10.98135    0.86201  12.739
    ## Asian.                                              -0.31959    0.07764  -4.116
    ## Black.                                              -3.50230    0.33540 -10.442
    ## Other.                                              -0.71752    0.94259  -0.761
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                          0.00144 ** 
    ## E2sfca_15_normalized                                 0.00114 ** 
    ## MSOADECILE2                                          0.17162    
    ## MSOADECILE3                                          0.31983    
    ## MSOADECILE4                                          0.03105 *  
    ## MSOADECILE5                                          0.11128    
    ## MSOADECILE6                                          0.65437    
    ## MSOADECILE7                                          0.33281    
    ## MSOADECILE8                                          0.15997    
    ## MSOADECILE9                                          0.04736 *  
    ## MSOADECILE10                                         0.11469    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              3.85e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                               0.44653    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9087     0.3832   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6217 on 17 Df
    ## Pseudo R-squared: 0.1018
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_15)
```

    ## [1] -12399.98

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
    ## -5.4398 -0.6922 -0.1697  0.4867 14.5633 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.22774    0.06938   3.283
    ## E2sfca_20_normalized                                 0.17744    0.06067   2.925
    ## MSOADECILE2                                         -0.03919    0.02981  -1.315
    ## MSOADECILE3                                         -0.03051    0.03291  -0.927
    ## MSOADECILE4                                         -0.07307    0.03508  -2.083
    ## MSOADECILE5                                         -0.05542    0.03717  -1.491
    ## MSOADECILE6                                         -0.01354    0.03909  -0.346
    ## MSOADECILE7                                          0.04306    0.04117   1.046
    ## MSOADECILE8                                          0.06292    0.04263   1.476
    ## MSOADECILE9                                          0.08971    0.04381   2.048
    ## MSOADECILE10                                         0.07552    0.04574   1.651
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.22641    0.10605  11.565
    ## Mixed.                                              10.98859    0.86550  12.696
    ## Asian.                                              -0.31968    0.07768  -4.115
    ## Black.                                              -3.50323    0.33547 -10.443
    ## Other.                                              -0.72817    0.94264  -0.772
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                          0.00103 ** 
    ## E2sfca_20_normalized                                 0.00345 ** 
    ## MSOADECILE2                                          0.18864    
    ## MSOADECILE3                                          0.35389    
    ## MSOADECILE4                                          0.03727 *  
    ## MSOADECILE5                                          0.13594    
    ## MSOADECILE6                                          0.72903    
    ## MSOADECILE7                                          0.29566    
    ## MSOADECILE8                                          0.13999    
    ## MSOADECILE9                                          0.04060 *  
    ## MSOADECILE10                                         0.09869 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              3.87e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                               0.43983    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9018     0.3831   54.56   <2e-16 ***
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

    ## [1] -12397.95

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
    ## -5.4378 -0.6891 -0.1690  0.4885 14.5502 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.24081    0.06907   3.487
    ## E2sfca_25_normalized                                 0.15839    0.07218   2.194
    ## MSOADECILE2                                         -0.03742    0.02981  -1.255
    ## MSOADECILE3                                         -0.02767    0.03289  -0.841
    ## MSOADECILE4                                         -0.06955    0.03505  -1.984
    ## MSOADECILE5                                         -0.05130    0.03711  -1.382
    ## MSOADECILE6                                         -0.00880    0.03902  -0.226
    ## MSOADECILE7                                          0.04666    0.04113   1.135
    ## MSOADECILE8                                          0.06702    0.04259   1.574
    ## MSOADECILE9                                          0.09442    0.04376   2.157
    ## MSOADECILE10                                         0.08147    0.04567   1.784
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.21393    0.10595  11.458
    ## Mixed.                                              11.10568    0.86763  12.800
    ## Asian.                                              -0.31716    0.07773  -4.080
    ## Black.                                              -3.50048    0.33558 -10.431
    ## Other.                                              -0.74257    0.94320  -0.787
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         0.000489 ***
    ## E2sfca_25_normalized                                0.028215 *  
    ## MSOADECILE2                                         0.209426    
    ## MSOADECILE3                                         0.400121    
    ## MSOADECILE4                                         0.047242 *  
    ## MSOADECILE5                                         0.166935    
    ## MSOADECILE6                                         0.821589    
    ## MSOADECILE7                                         0.256581    
    ## MSOADECILE8                                         0.115581    
    ## MSOADECILE9                                         0.030969 *  
    ## MSOADECILE10                                        0.074435 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                               4.5e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                              0.431117    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8888     0.3828   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6214 on 17 Df
    ## Pseudo R-squared: 0.1011
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(E2scfa_25)
```

    ## [1] -12394.39

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
    ## -5.4353 -0.6905 -0.1683  0.4865 14.5378 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.254823   0.068738
    ## E2sfca_30_normalized                                 0.086589   0.075688
    ## MSOADECILE2                                         -0.036280   0.029815
    ## MSOADECILE3                                         -0.025339   0.032877
    ## MSOADECILE4                                         -0.066418   0.035038
    ## MSOADECILE5                                         -0.048121   0.037087
    ## MSOADECILE6                                         -0.004692   0.038988
    ## MSOADECILE7                                          0.049973   0.041116
    ## MSOADECILE8                                          0.070855   0.042566
    ## MSOADECILE9                                          0.099762   0.043720
    ## MSOADECILE10                                         0.088117   0.045608
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.207530   0.105992
    ## Mixed.                                              11.299886   0.868102
    ## Asian.                                              -0.312763   0.077776
    ## Black.                                              -3.494734   0.335712
    ## Other.                                              -0.758632   0.944007
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.707  0.00021 ***
    ## E2sfca_30_normalized                                  1.144  0.25262    
    ## MSOADECILE2                                          -1.217  0.22366    
    ## MSOADECILE3                                          -0.771  0.44087    
    ## MSOADECILE4                                          -1.896  0.05801 .  
    ## MSOADECILE5                                          -1.298  0.19446    
    ## MSOADECILE6                                          -0.120  0.90421    
    ## MSOADECILE7                                           1.215  0.22421    
    ## MSOADECILE8                                           1.665  0.09599 .  
    ## MSOADECILE9                                           2.282  0.02250 *  
    ## MSOADECILE10                                          1.932  0.05335 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.393  < 2e-16 ***
    ## Mixed.                                               13.017  < 2e-16 ***
    ## Asian.                                               -4.021 5.79e-05 ***
    ## Black.                                              -10.410  < 2e-16 ***
    ## Other.                                               -0.804  0.42161    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8763     0.3826   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6213 on 17 Df
    ## Pseudo R-squared: 0.1008
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(E2scfa_30)
```

    ## [1] -12391.07

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
    ## -5.4403 -0.6898 -0.1711  0.4837 14.5177 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                     Estimate Std. Error z value
    ## (Intercept)                                          0.23046    0.06903   3.339
    ## X3sfca_10_normalized                                 0.19479    0.05620   3.466
    ## MSOADECILE2                                         -0.03968    0.02981  -1.331
    ## MSOADECILE3                                         -0.03127    0.03290  -0.950
    ## MSOADECILE4                                         -0.07423    0.03507  -2.116
    ## MSOADECILE5                                         -0.05867    0.03720  -1.577
    ## MSOADECILE6                                         -0.01736    0.03912  -0.444
    ## MSOADECILE7                                          0.04027    0.04120   0.977
    ## MSOADECILE8                                          0.06132    0.04262   1.439
    ## MSOADECILE9                                          0.08837    0.04376   2.019
    ## MSOADECILE10                                         0.07319    0.04571   1.601
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.23719    0.10622  11.648
    ## Mixed.                                              11.08697    0.85676  12.941
    ## Asian.                                              -0.31274    0.07756  -4.032
    ## Black.                                              -3.49391    0.33520 -10.423
    ## Other.                                              -0.68200    0.94318  -0.723
    ##                                                     Pr(>|z|)    
    ## (Intercept)                                         0.000842 ***
    ## X3sfca_10_normalized                                0.000528 ***
    ## MSOADECILE2                                         0.183202    
    ## MSOADECILE3                                         0.341859    
    ## MSOADECILE4                                         0.034303 *  
    ## MSOADECILE5                                         0.114787    
    ## MSOADECILE6                                         0.657259    
    ## MSOADECILE7                                         0.328356    
    ## MSOADECILE8                                         0.150219    
    ## MSOADECILE9                                         0.043449 *  
    ## MSOADECILE10                                        0.109289    
    ## Per_cent_of_households_with_at_least_one_car_or_van  < 2e-16 ***
    ## Mixed.                                               < 2e-16 ***
    ## Asian.                                              5.53e-05 ***
    ## Black.                                               < 2e-16 ***
    ## Other.                                              0.469624    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9144     0.3833   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6218 on 17 Df
    ## Pseudo R-squared: 0.1021
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_10)
```

    ## [1] -12401.69

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
    ## -5.4358 -0.6886 -0.1684  0.4884 14.5262 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.250932   0.068754
    ## X3sfca_15_normalized                                 0.089494   0.050558
    ## MSOADECILE2                                         -0.037485   0.029823
    ## MSOADECILE3                                         -0.027344   0.032907
    ## MSOADECILE4                                         -0.069155   0.035090
    ## MSOADECILE5                                         -0.051971   0.037197
    ## MSOADECILE6                                         -0.008833   0.039097
    ## MSOADECILE7                                          0.046746   0.041194
    ## MSOADECILE8                                          0.067575   0.042630
    ## MSOADECILE9                                          0.096004   0.043787
    ## MSOADECILE10                                         0.083584   0.045705
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.214288   0.106002
    ## Mixed.                                              11.250868   0.861215
    ## Asian.                                              -0.311260   0.077660
    ## Black.                                              -3.490321   0.335505
    ## Other.                                              -0.729497   0.944177
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.650 0.000263 ***
    ## X3sfca_15_normalized                                  1.770 0.076706 .  
    ## MSOADECILE2                                          -1.257 0.208786    
    ## MSOADECILE3                                          -0.831 0.406005    
    ## MSOADECILE4                                          -1.971 0.048747 *  
    ## MSOADECILE5                                          -1.397 0.162356    
    ## MSOADECILE6                                          -0.226 0.821260    
    ## MSOADECILE7                                           1.135 0.256471    
    ## MSOADECILE8                                           1.585 0.112926    
    ## MSOADECILE9                                           2.193 0.028343 *  
    ## MSOADECILE10                                          1.829 0.067433 .  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.455  < 2e-16 ***
    ## Mixed.                                               13.064  < 2e-16 ***
    ## Asian.                                               -4.008 6.12e-05 ***
    ## Black.                                              -10.403  < 2e-16 ***
    ## Other.                                               -0.773 0.439743    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8823     0.3827   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6213 on 17 Df
    ## Pseudo R-squared: 0.1011
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_15)
```

    ## [1] -12392.86

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
    ## -5.4331 -0.6922 -0.1710  0.4820 14.5355 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                       Estimate Std. Error
    ## (Intercept)                                          0.2657211  0.0685343
    ## X3sfca_20_normalized                                -0.0265354  0.0559534
    ## MSOADECILE2                                         -0.0356010  0.0298244
    ## MSOADECILE3                                         -0.0230707  0.0328976
    ## MSOADECILE4                                         -0.0629151  0.0350808
    ## MSOADECILE5                                         -0.0449018  0.0371512
    ## MSOADECILE6                                         -0.0003365  0.0390474
    ## MSOADECILE7                                          0.0537641  0.0411656
    ## MSOADECILE8                                          0.0748798  0.0426067
    ## MSOADECILE9                                          0.1060837  0.0437592
    ## MSOADECILE10                                         0.0962923  0.0456694
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.2097378  0.1060098
    ## Mixed.                                              11.5741789  0.8643741
    ## Asian.                                              -0.3069809  0.0777266
    ## Black.                                              -3.4823503  0.3356152
    ## Other.                                              -0.7820948  0.9450865
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.877 0.000106 ***
    ## X3sfca_20_normalized                                 -0.474 0.635328    
    ## MSOADECILE2                                          -1.194 0.232600    
    ## MSOADECILE3                                          -0.701 0.483124    
    ## MSOADECILE4                                          -1.793 0.072904 .  
    ## MSOADECILE5                                          -1.209 0.226808    
    ## MSOADECILE6                                          -0.009 0.993124    
    ## MSOADECILE7                                           1.306 0.191538    
    ## MSOADECILE8                                           1.757 0.078839 .  
    ## MSOADECILE9                                           2.424 0.015340 *  
    ## MSOADECILE10                                          2.108 0.034991 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.412  < 2e-16 ***
    ## Mixed.                                               13.390  < 2e-16 ***
    ## Asian.                                               -3.949 7.83e-05 ***
    ## Black.                                              -10.376  < 2e-16 ***
    ## Other.                                               -0.828 0.407932    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8722     0.3825   54.56   <2e-16 ***
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

    ## [1] -12390.04

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
    ## -5.4337 -0.6939 -0.1714  0.4834 14.5179 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.270075   0.068408
    ## X3sfca_25_normalized                                -0.136830   0.058667
    ## MSOADECILE2                                         -0.034930   0.029812
    ## MSOADECILE3                                         -0.020538   0.032876
    ## MSOADECILE4                                         -0.058825   0.035050
    ## MSOADECILE5                                         -0.041213   0.037101
    ## MSOADECILE6                                          0.004541   0.038996
    ## MSOADECILE7                                          0.058094   0.041129
    ## MSOADECILE8                                          0.079337   0.042570
    ## MSOADECILE9                                          0.112870   0.043719
    ## MSOADECILE10                                         0.105737   0.045635
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.219646   0.106084
    ## Mixed.                                              11.863364   0.866147
    ## Asian.                                              -0.302364   0.077730
    ## Black.                                              -3.468894   0.335515
    ## Other.                                              -0.819263   0.945432
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.948 7.88e-05 ***
    ## X3sfca_25_normalized                                 -2.332  0.01968 *  
    ## MSOADECILE2                                          -1.172  0.24132    
    ## MSOADECILE3                                          -0.625  0.53216    
    ## MSOADECILE4                                          -1.678  0.09329 .  
    ## MSOADECILE5                                          -1.111  0.26664    
    ## MSOADECILE6                                           0.116  0.90731    
    ## MSOADECILE7                                           1.412  0.15781    
    ## MSOADECILE8                                           1.864  0.06237 .  
    ## MSOADECILE9                                           2.582  0.00983 ** 
    ## MSOADECILE10                                          2.317  0.02050 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.497  < 2e-16 ***
    ## Mixed.                                               13.697  < 2e-16 ***
    ## Asian.                                               -3.890  0.00010 ***
    ## Black.                                              -10.339  < 2e-16 ***
    ## Other.                                               -0.867  0.38619    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.8906     0.3829   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6215 on 17 Df
    ## Pseudo R-squared: 0.1013
    ## Number of iterations: 25 (BFGS) + 2 (Fisher scoring)

``` r
AIC(scfa3_25)
```

    ## [1] -12395.04

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
    ## -5.4366 -0.6961 -0.1626  0.4797 14.5013 
    ## 
    ## Coefficients (mean model with logit link):
    ##                                                      Estimate Std. Error
    ## (Intercept)                                          0.269980   0.068322
    ## X3sfca_30_normalized                                -0.228638   0.058900
    ## MSOADECILE2                                         -0.034895   0.029791
    ## MSOADECILE3                                         -0.018946   0.032848
    ## MSOADECILE4                                         -0.056174   0.035014
    ## MSOADECILE5                                         -0.039217   0.037056
    ## MSOADECILE6                                          0.007469   0.038949
    ## MSOADECILE7                                          0.061106   0.041092
    ## MSOADECILE8                                          0.082254   0.042531
    ## MSOADECILE9                                          0.118100   0.043684
    ## MSOADECILE10                                         0.113409   0.045606
    ## Per_cent_of_households_with_at_least_one_car_or_van  1.233882   0.106135
    ## Mixed.                                              12.115081   0.867372
    ## Asian.                                              -0.298030   0.077694
    ## Black.                                              -3.453563   0.335311
    ## Other.                                              -0.858208   0.945368
    ##                                                     z value Pr(>|z|)    
    ## (Intercept)                                           3.952 7.76e-05 ***
    ## X3sfca_30_normalized                                 -3.882 0.000104 ***
    ## MSOADECILE2                                          -1.171 0.241473    
    ## MSOADECILE3                                          -0.577 0.564092    
    ## MSOADECILE4                                          -1.604 0.108644    
    ## MSOADECILE5                                          -1.058 0.289912    
    ## MSOADECILE6                                           0.192 0.847919    
    ## MSOADECILE7                                           1.487 0.137000    
    ## MSOADECILE8                                           1.934 0.053119 .  
    ## MSOADECILE9                                           2.704 0.006861 ** 
    ## MSOADECILE10                                          2.487 0.012892 *  
    ## Per_cent_of_households_with_at_least_one_car_or_van  11.626  < 2e-16 ***
    ## Mixed.                                               13.968  < 2e-16 ***
    ## Asian.                                               -3.836 0.000125 ***
    ## Black.                                              -10.300  < 2e-16 ***
    ## Other.                                               -0.908 0.363982    
    ## 
    ## Phi coefficients (precision model with identity link):
    ##       Estimate Std. Error z value Pr(>|z|)    
    ## (phi)  20.9245     0.3835   54.56   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Type of estimator: ML (maximum likelihood)
    ## Log-likelihood:  6219 on 17 Df
    ## Pseudo R-squared: 0.1023
    ## Number of iterations: 25 (BFGS) + 3 (Fisher scoring)

``` r
AIC(scfa3_30)
```

    ## [1] -12404.32
