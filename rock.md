礦石判斷模型
================

資料前處理
----------

### 資料讀取

此資料來源為UCI Machine Learning Repository。

記載礦物與石頭接受各個不同角度的聲波撞擊後，接收到的回聲數值，一共有60個參數，代表使用一特別角度的聲波撞擊礦石所得回聲。另外，分類結果為二元分類，包括礦物 (M) 與石頭 (R) 。

``` r
library(mlbench)
data(Sonar) 
str(Sonar) 
```

    ## 'data.frame':    208 obs. of  61 variables:
    ##  $ V1   : num  0.02 0.0453 0.0262 0.01 0.0762 0.0286 0.0317 0.0519 0.0223 0.0164 ...
    ##  $ V2   : num  0.0371 0.0523 0.0582 0.0171 0.0666 0.0453 0.0956 0.0548 0.0375 0.0173 ...
    ##  $ V3   : num  0.0428 0.0843 0.1099 0.0623 0.0481 ...
    ##  $ V4   : num  0.0207 0.0689 0.1083 0.0205 0.0394 ...
    ##  $ V5   : num  0.0954 0.1183 0.0974 0.0205 0.059 ...
    ##  $ V6   : num  0.0986 0.2583 0.228 0.0368 0.0649 ...
    ##  $ V7   : num  0.154 0.216 0.243 0.11 0.121 ...
    ##  $ V8   : num  0.16 0.348 0.377 0.128 0.247 ...
    ##  $ V9   : num  0.3109 0.3337 0.5598 0.0598 0.3564 ...
    ##  $ V10  : num  0.211 0.287 0.619 0.126 0.446 ...
    ##  $ V11  : num  0.1609 0.4918 0.6333 0.0881 0.4152 ...
    ##  $ V12  : num  0.158 0.655 0.706 0.199 0.395 ...
    ##  $ V13  : num  0.2238 0.6919 0.5544 0.0184 0.4256 ...
    ##  $ V14  : num  0.0645 0.7797 0.532 0.2261 0.4135 ...
    ##  $ V15  : num  0.066 0.746 0.648 0.173 0.453 ...
    ##  $ V16  : num  0.227 0.944 0.693 0.213 0.533 ...
    ##  $ V17  : num  0.31 1 0.6759 0.0693 0.7306 ...
    ##  $ V18  : num  0.3 0.887 0.755 0.228 0.619 ...
    ##  $ V19  : num  0.508 0.802 0.893 0.406 0.203 ...
    ##  $ V20  : num  0.48 0.782 0.862 0.397 0.464 ...
    ##  $ V21  : num  0.578 0.521 0.797 0.274 0.415 ...
    ##  $ V22  : num  0.507 0.405 0.674 0.369 0.429 ...
    ##  $ V23  : num  0.433 0.396 0.429 0.556 0.573 ...
    ##  $ V24  : num  0.555 0.391 0.365 0.485 0.54 ...
    ##  $ V25  : num  0.671 0.325 0.533 0.314 0.316 ...
    ##  $ V26  : num  0.641 0.32 0.241 0.533 0.229 ...
    ##  $ V27  : num  0.71 0.327 0.507 0.526 0.7 ...
    ##  $ V28  : num  0.808 0.277 0.853 0.252 1 ...
    ##  $ V29  : num  0.679 0.442 0.604 0.209 0.726 ...
    ##  $ V30  : num  0.386 0.203 0.851 0.356 0.472 ...
    ##  $ V31  : num  0.131 0.379 0.851 0.626 0.51 ...
    ##  $ V32  : num  0.26 0.295 0.504 0.734 0.546 ...
    ##  $ V33  : num  0.512 0.198 0.186 0.612 0.288 ...
    ##  $ V34  : num  0.7547 0.2341 0.2709 0.3497 0.0981 ...
    ##  $ V35  : num  0.854 0.131 0.423 0.395 0.195 ...
    ##  $ V36  : num  0.851 0.418 0.304 0.301 0.418 ...
    ##  $ V37  : num  0.669 0.384 0.612 0.541 0.46 ...
    ##  $ V38  : num  0.61 0.106 0.676 0.881 0.322 ...
    ##  $ V39  : num  0.494 0.184 0.537 0.986 0.283 ...
    ##  $ V40  : num  0.274 0.197 0.472 0.917 0.243 ...
    ##  $ V41  : num  0.051 0.167 0.465 0.612 0.198 ...
    ##  $ V42  : num  0.2834 0.0583 0.2587 0.5006 0.2444 ...
    ##  $ V43  : num  0.282 0.14 0.213 0.321 0.185 ...
    ##  $ V44  : num  0.4256 0.1628 0.2222 0.3202 0.0841 ...
    ##  $ V45  : num  0.2641 0.0621 0.2111 0.4295 0.0692 ...
    ##  $ V46  : num  0.1386 0.0203 0.0176 0.3654 0.0528 ...
    ##  $ V47  : num  0.1051 0.053 0.1348 0.2655 0.0357 ...
    ##  $ V48  : num  0.1343 0.0742 0.0744 0.1576 0.0085 ...
    ##  $ V49  : num  0.0383 0.0409 0.013 0.0681 0.023 0.0264 0.0507 0.0285 0.0777 0.0092 ...
    ##  $ V50  : num  0.0324 0.0061 0.0106 0.0294 0.0046 0.0081 0.0159 0.0178 0.0439 0.0198 ...
    ##  $ V51  : num  0.0232 0.0125 0.0033 0.0241 0.0156 0.0104 0.0195 0.0052 0.0061 0.0118 ...
    ##  $ V52  : num  0.0027 0.0084 0.0232 0.0121 0.0031 0.0045 0.0201 0.0081 0.0145 0.009 ...
    ##  $ V53  : num  0.0065 0.0089 0.0166 0.0036 0.0054 0.0014 0.0248 0.012 0.0128 0.0223 ...
    ##  $ V54  : num  0.0159 0.0048 0.0095 0.015 0.0105 0.0038 0.0131 0.0045 0.0145 0.0179 ...
    ##  $ V55  : num  0.0072 0.0094 0.018 0.0085 0.011 0.0013 0.007 0.0121 0.0058 0.0084 ...
    ##  $ V56  : num  0.0167 0.0191 0.0244 0.0073 0.0015 0.0089 0.0138 0.0097 0.0049 0.0068 ...
    ##  $ V57  : num  0.018 0.014 0.0316 0.005 0.0072 0.0057 0.0092 0.0085 0.0065 0.0032 ...
    ##  $ V58  : num  0.0084 0.0049 0.0164 0.0044 0.0048 0.0027 0.0143 0.0047 0.0093 0.0035 ...
    ##  $ V59  : num  0.009 0.0052 0.0095 0.004 0.0107 0.0051 0.0036 0.0048 0.0059 0.0056 ...
    ##  $ V60  : num  0.0032 0.0044 0.0078 0.0117 0.0094 0.0062 0.0103 0.0053 0.0022 0.004 ...
    ##  $ Class: Factor w/ 2 levels "M","R": 2 2 2 2 2 2 2 2 2 2 ...

### 將資料隨機分為訓練組與測試組

隨機將2/3的資料分到訓練組（Test==F），剩下1/3為測試組（Test==T）

``` r
Sonar$Test<-F 
Sonar[sample(1:nrow(Sonar),nrow(Sonar)/3),]$Test<-T 
c(sum(Sonar$Test==F),sum(Sonar$Test==T)) 
```

    ## [1] 139  69

可得訓練組案例數為139，測試組案例數為69

預測模型建立
------------

### 模型建立

由於變數多，且多為連續變項，而輸出為二元類別變項，故選擇邏輯迴歸演算法建立模型，並使用雙向逐步選擇最佳參數組合。

``` r
fit<-glm(Class~., Sonar[Sonar$Test==F,],family="binomial")
library(MASS)
finalFit<-stepAIC(fit,direction = "both",trace = F)
summary(finalFit)$coefficients
```

    ##                Estimate Std. Error      z value  Pr(>|z|)
    ## (Intercept)    167.9259   57424.99  0.002924266 0.9976668
    ## V1          -21229.2935 1148700.63 -0.018481137 0.9852550
    ## V8            2861.5031  188715.60  0.015163045 0.9879021
    ## V12          -4123.5750  249845.80 -0.016504480 0.9868319
    ## V17           6930.4608  385798.78  0.017963926 0.9856676
    ## V18          -5737.0434  331185.27 -0.017322762 0.9861791
    ## V20           1545.5919   86275.23  0.017914664 0.9857069
    ## V22          -5611.5108  318030.64 -0.017644560 0.9859224
    ## V23           7494.8013  517429.24  0.014484688 0.9884433
    ## V24          -9929.6184  583227.96 -0.017025278 0.9864164
    ## V25          10202.1649  563165.85  0.018115738 0.9855465
    ## V26          -4453.0719  266016.70 -0.016739821 0.9866442
    ## V29           3533.1106  209971.91  0.016826587 0.9865750
    ## V30          -9602.4442  528590.65 -0.018166126 0.9855063
    ## V31          13432.3016  717038.39  0.018733030 0.9850541
    ## V32          -8931.8076  485554.16 -0.018395080 0.9853237
    ## V34           7677.7244  477654.72  0.016073796 0.9871755
    ## V35          -7259.2167  418171.07 -0.017359443 0.9861499
    ## V36           5213.5414  277072.91  0.018816496 0.9849875
    ## V48          -6722.6898  658226.16 -0.010213343 0.9918511
    ## V49         -24342.6888 1352186.73 -0.018002461 0.9856369
    ## V50          51102.4987 3608613.78  0.014161255 0.9887013
    ## V60          30770.8818 2021651.01  0.015220669 0.9878561

### 模型說明

由上述參數可知，使用聲波在不同角度撞擊`礦石`所得到的回聲資料，以邏輯迴歸建立模型預測礦石是否為礦物，經最佳化後，模型使用參數為V1, V8, V12, V17, V18, V20, V22, V23, V24, V25, V26, V29, V30, V31, V32, V34, V35, V36, V48, V49, V50, V60，共23個參數，各參數代表從一特別角度所得的礦石回聲

預測模型驗證
------------

``` r
MinePred<-predict(finalFit,newdata = Sonar[Sonar$Test==T,])
MineAns<-ifelse(MinePred<0.5,"M","R") #<0.5: Level 1
MineAns<-factor(MineAns,levels = c("M","R"))
library(caret)
sensitivity(MineAns,Sonar[Sonar$Test==T,]$Class)
```

    ## [1] 0.9210526

``` r
specificity(MineAns,Sonar[Sonar$Test==T,]$Class)
```

    ## [1] 0.9032258

``` r
posPredValue(MineAns,Sonar[Sonar$Test==T,]$Class)
```

    ## [1] 0.9210526

``` r
negPredValue(MineAns,Sonar[Sonar$Test==T,]$Class)
```

    ## [1] 0.9032258

使用聲波在不同角度撞擊`礦石`所得到的回聲資料，以邏輯迴歸模型預測礦石是否為礦物，可得：

-   敏感度 92.1052632%
-   特異性 90.3225806%
-   陽性預測率 92.1052632%
-   陰性預測率 90.3225806%