Untitled
================
12 juli, 2025

``` r
sdt <- dagitty::dagitty(paste(readLines("theory/sdt.txt"), collapse = "\n"))
sdt_pruned <- theorytools:::prune_dag(sdt,
                                      exposure = "intrinsic_motivation",
                                      outcome = "wellbeing")
sdt_pruned
```

    ## dag {
    ## intrinsic_motivation
    ## needs
    ## wellbeing
    ## intrinsic_motivation -> wellbeing
    ## needs -> intrinsic_motivation
    ## needs -> wellbeing
    ## }

``` r
set.seed(1)
df <- theorytools::simulate_data(sdt_pruned, n = 100)
head(df)
```

    ##   intrinsic_motivation      needs  wellbeing
    ## 1           -0.8106383 -0.3320551 -1.1859445
    ## 2           -1.2225131 -0.4105931 -1.9002197
    ## 3           -0.4850829 -0.4792975  0.2180346
    ## 4            0.2811818 -0.9784317  1.2473513
    ## 5            0.8229563 -0.2662130  0.3359753
    ## 6            0.5946914 -1.0096257 -1.1656961

``` r
res <- lm(wellbeing ~ intrinsic_motivation + needs, data = df)
summary(res)
```

    ## 
    ## Call:
    ## lm(formula = wellbeing ~ intrinsic_motivation + needs, data = df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.93688 -0.87862 -0.03086  0.74147  2.56125 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          -0.02492    0.11579  -0.215 0.830069    
    ## intrinsic_motivation  0.44496    0.11522   3.862 0.000203 ***
    ## needs                -0.30228    0.11815  -2.558 0.012061 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.126 on 97 degrees of freedom
    ## Multiple R-squared:  0.1839, Adjusted R-squared:  0.1671 
    ## F-statistic: 10.93 on 2 and 97 DF,  p-value: 5.231e-05

``` r
sum_res <- summary(res)
sum_res$coefficients["intrinsic_motivation", "Pr(>|t|)"] < .05
```

    ## [1] TRUE

``` r
knitr::kable(study_results, digits = 2)
```

| beta |   n | power |
|-----:|----:|------:|
|  0.1 |  50 |  0.09 |
|  0.2 |  50 |  0.21 |
|  0.4 |  50 |  0.72 |
|  0.1 | 100 |  0.18 |
|  0.2 | 100 |  0.49 |
|  0.4 | 100 |  0.99 |
|  0.1 | 200 |  0.29 |
|  0.2 | 200 |  0.80 |
|  0.4 | 200 |  1.00 |
