Facebook粉絲團分析（分析專頁：柯文哲）
================

分析台北市長柯文哲在2016/01/01至04/10的貼文

讀取柯文哲粉絲團資料
--------------------

``` r
if (!require('Rfacebook')){
    install.packages("Rfacebook")
    library(Rfacebook)
}
```

    ## Loading required package: Rfacebook

    ## Warning: package 'Rfacebook' was built under R version 3.2.4

    ## Loading required package: httr

    ## Warning: package 'httr' was built under R version 3.2.4

    ## Loading required package: rjson

    ## Loading required package: httpuv

    ## Warning: package 'httpuv' was built under R version 3.2.4

    ## 
    ## Attaching package: 'Rfacebook'

    ## The following object is masked from 'package:methods':
    ## 
    ##     getGroup

``` r
token<-'CAACEdEose0cBAKKIPZBwuIk9fIZCXFy0de5Wh09Bya2gPWPwtXOXrhT1WzgsZBmK0XukdjuZCXLoZCV8X8ywKqig7gEvZBQjMa5JTBdAvbgk5hVZBnY7lH0NKRZAaGSV7NQb9mmCuRWxOZB3BqhTSfd26iC2TbvFyFZADVBvZAIdiZAwwdh0086lSO2BcECiscAEihm0co1mGz8E1W7D8lr2cWU9'
totalPage<-NULL
lastDate<-Sys.Date()
DateVectorStr<-as.character(seq(as.Date("2016-01-01"),lastDate,by="5 days"))
for(i in 1:(length(DateVectorStr)-1)){
    tempPage<-getPage("DoctorKoWJ", token,
                      since = DateVectorStr[i],until = DateVectorStr[i+1])
    totalPage<-rbind(totalPage,tempPage)
}
```

    ## 2 posts 10 posts 3 posts 2 posts 4 posts 4 posts 3 posts 4 posts 2 posts 1 posts 1 posts 3 posts 3 posts 2 posts 3 posts 2 posts 4 posts 2 posts 1 posts 1 posts

``` r
nrow(totalPage)
```

    ## [1] 57

粉絲團從2016/1/1至2016/4/10一共有57篇文章

每日發文數分析
--------------

``` r
totalPage$datetime <- as.POSIXct(totalPage$created_time, 
                                 format = "%Y-%m-%dT%H:%M:%S+0000", 
                                 tz = "GMT") #2016-01-16T15:05:36+0000
totalPage$dateTPE <- format(totalPage$datetime, "%Y-%m-%d", 
                            tz = "Asia/Taipei") #2016-01-16
totalPage$weekdays <-weekdays(as.Date(totalPage$dateTPE))
PostCount<-aggregate(id~weekdays+dateTPE,totalPage,length)
library(knitr)
kable(head(PostCount[order(PostCount$id,decreasing = T),]))
```

|     | weekdays | dateTPE    |   id|
|-----|:---------|:-----------|----:|
| 6   | 星期六   | 2016-01-09 |    4|
| 5   | 星期五   | 2016-01-08 |    2|
| 7   | 星期日   | 2016-01-10 |    2|
| 25  | 星期六   | 2016-02-06 |    2|
| 44  | 星期二   | 2016-03-22 |    2|
| 1   | 星期五   | 2016-01-01 |    1|

討論:首先在讀取資料時，先把fb的created\_time轉成較能明瞭的日期，再利用weekdays函數即能得知發文那天是禮拜幾。從輸出的結果可以發現，柯文哲通常一到四天之內一定會發一篇文章，而在星期五、星期六或星期日時有時會一天發兩篇以上。在1/9這一天我們可以看到他總共發了四篇文章，這是從今年1/1以來單日發文最多的一天。那為甚麼這一天的發文會那麼多呢?查了一下totalPage之後發現原來柯文哲這天是在騎腳踏車，發那麼多文章的原因可能是想讓大家知道他現在從台北騎到哪了。

每日likes數
-----------

``` r
output<-aggregate(likes_count~weekdays+dateTPE,totalPage,mean)
library(knitr)
kable(head(output[order(output$likes_count,decreasing = T),]))
```

|     | weekdays | dateTPE    |  likes\_count|
|-----|:---------|:-----------|-------------:|
| 11  | 星期六   | 2016-01-16 |        329087|
| 33  | 星期日   | 2016-02-28 |        228900|
| 7   | 星期日   | 2016-01-10 |        223666|
| 9   | 星期四   | 2016-01-14 |        187448|
| 32  | 星期六   | 2016-02-27 |        180919|
| 47  | 星期一   | 2016-03-28 |        139065|

討論:從輸出的結果可以發現，柯文哲每一篇貼文大都獲得10000以上的likes數量，但3/22之後的貼文都在10000之下，我們可以猜測是因為時間離現在較近，累積的likes數可能不如以前來的多。但就3/28這天卻能在短時間內獲得10幾萬個likes數，為甚麼會那麼多呢?原來是恐怖的兇殺案件的文章，這類文章往往能讓許多人產生共鳴，所以我想這是他能獲得這麼多likes數的原因。而在1/16這天，柯文哲的一篇文章獲得近33萬的like數，這是今年以來最多like數的一篇文章，而這篇發文是跟蔡英文當選總統有關，我們可以猜測或許有追蹤柯文哲粉絲團的人可能也很喜歡蔡英文。

每日comments數
--------------

``` r
output<-aggregate(comments_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(output[order(output$comments_count,decreasing = T),]))
```

|     | dateTPE    |  comments\_count|
|-----|:-----------|----------------:|
| 7   | 2016-01-10 |          5981.50|
| 6   | 2016-01-09 |          5153.25|
| 47  | 2016-03-28 |          5103.00|
| 33  | 2016-02-28 |          3565.00|
| 32  | 2016-02-27 |          3268.00|
| 9   | 2016-01-14 |          2848.00|

討論:從輸出的結果可以發現，柯文哲每一篇貼文大都獲得500以上的comments數，而在1/9，1/10兩天的comments都超過5000，這兩天的發文都是跟柯文哲騎腳踏車有關，而從comments的內容來看，我們可以看到很多篇關於市長加油的留言，看來comments那麼多的原因應該是因為粉絲希望柯文哲能趕快完成北高雙城挑戰，而上面也充滿了很多其他篇較少看到的正面留言。

每日shares數
------------

``` r
output<-aggregate(shares_count~dateTPE,totalPage,mean)
library(knitr)
kable(head(output[order(output$shares_count,decreasing = T),]))
```

|     | dateTPE    |  shares\_count|
|-----|:-----------|--------------:|
| 9   | 2016-01-14 |        34775.0|
| 7   | 2016-01-10 |        16407.5|
| 33  | 2016-02-28 |        11235.0|
| 35  | 2016-03-04 |         5334.0|
| 47  | 2016-03-28 |         4967.0|
| 11  | 2016-01-16 |         4897.0|

討論:從輸出的結果可以發現，柯文哲的每篇發文平均能獲得100以上的shares數，而在1/14這天shares的數量更超過34000，我們觀察這篇發文是關於夢想背後的心路歷程，一樣跟北高雙城挑戰有關，能獲得這麼多shares的原因應為大家對於柯文哲努力不放棄的精神都很有感覺，所以希望讓更多人看到這則發文；另外在2/28這天，發文的shares數也大於10000次，內容為柯文哲希望大家記取228歷史的教訓而走出悲傷，一樣是非常勵志的一篇發文，這是我認為他能獲得較多shares數的原因。
