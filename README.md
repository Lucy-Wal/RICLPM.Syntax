# RICLPM.Syntax

This R package writes lavaan syntax for Random Intercept Cross Lag Panel Modelling, in line with Mulder & Hamaker ([2021](https://doi.org/10.1080/10705511.2020.1784738)). 

## How To Install

You can install this package through devtools

```r
devtools::install_github("Lucy-Wal/RICLPM.Syntax")
```

## Generating Syntax
`riclpm()` requires the user to specify *v* variable names and *time* number of time points. An array of additional constraints can also be specified (default to FALSE).
The function will deliver a corresponding text syntax, which can be passed straight to lavaan or copied to an R script for adjustment.

```r
#Example:
syntax <- riclpm(v = c("X", "Y"), time = 4)  #Two variables (X and Y) recorded at 4 time points
model <- lavaan(syntax, data, missing = 'ML')
cat(syntax)
```

```r
 ##Random Intercepts## 
 
 RI_X =~  1*T1_X +  1*T2_X +  1*T3_X +  1*T4_X 
 RI_Y =~  1*T1_Y +  1*T2_Y +  1*T3_Y +  1*T4_Y 
 
 ##Residuals## 
 wx1 ~~ wx1 
 wx2 ~~ wx2 
 wx3 ~~ wx3 
 wx4 ~~ wx4 
 
 wy1 ~~ wy1 
 wy2 ~~ wy2 
 wy3 ~~ wy3 
 wy4 ~~ wy4 
 
 
 ##Within Person Variables## 
 
 wx1 =~ 1*T1_X 
 wx2 =~ 1*T2_X 
 wx3 =~ 1*T3_X 
 wx4 =~ 1*T4_X 
 
 wy1 =~ 1*T1_Y 
 wy2 =~ 1*T2_Y 
 wy3 =~ 1*T3_Y 
 wy4 =~ 1*T4_Y 
 
 ##Cross Lag Regressions## 
 
 wx2 ~ wx1 + wy1 
 wx3 ~ wx2 + wy2 
 wx4 ~ wx3 + wy3 
 
 wy2 ~ wx1 + wy1 
 wy3 ~ wx2 + wy2 
 wy4 ~ wx3 + wy3 
 
 ##Residual Covariances in Same Wave## 
 
 wx1 ~~ wy1 
 wx2 ~~ wy2 
 wx3 ~~ wy3 
 wx4 ~~ wy4 
 
 ##Variances and Covariances of Random Intercepts## 
 
 RI_X ~~ RI_X 
 RI_Y ~~ RI_Y 
 
 RI_X ~~ RI_Y
```


### Latent Variables
`latent_riclpm()` can be used to generate syntax for latent factors in the same manner. This function requires an additional *items* input, corresponding to the number of items in each variable.
Additional constraints and error terms can be included. 

```r
#Example:
syntax <- latent_riclpm(v = c("X", "Y"), items = c(3, 5), time = 4)  #Variable X is measured by 3 items, variabale Y is measured by 5 items, across 4 time points
model <- lavaan(syntax, data, missing = 'ML')
```

