Tests & Analyses
================
2022-12-05

``` r
knitr::opts_chunk$set(message = FALSE)

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## Loading required package: timechange
    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(plotly)
```

    ## 
    ## Attaching package: 'plotly'
    ## 
    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     last_plot
    ## 
    ## The following object is masked from 'package:stats':
    ## 
    ##     filter
    ## 
    ## The following object is masked from 'package:graphics':
    ## 
    ##     layout

``` r
library(mgcv)
```

    ## Loading required package: nlme
    ## 
    ## Attaching package: 'nlme'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse
    ## 
    ## This is mgcv 1.8-41. For overview type 'help("mgcv-package")'.

Data input and cleaning

``` r
bakery_df =
  read_csv("./Data/Bakery_sales.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    unit_price = str_replace(unit_price, "€", ""),
    unit_price = str_replace(unit_price, ",", "."),
    unit_price = as.numeric(unit_price),
    product_name = article) %>% 
  filter(product_name != ".") %>% 
  select(-article)

bakery_df
```

    ## # A tibble: 234,000 × 7
    ##       x1 date       time   ticket_number quantity unit_price product_name       
    ##    <dbl> <date>     <time>         <dbl>    <dbl>      <dbl> <chr>              
    ##  1     0 2021-01-02 08:38         150040        1       0.9  BAGUETTE           
    ##  2     1 2021-01-02 08:38         150040        3       1.2  PAIN AU CHOCOLAT   
    ##  3     4 2021-01-02 09:14         150041        2       1.2  PAIN AU CHOCOLAT   
    ##  4     5 2021-01-02 09:14         150041        1       1.15 PAIN               
    ##  5     8 2021-01-02 09:25         150042        5       1.2  TRADITIONAL BAGUET…
    ##  6    11 2021-01-02 09:25         150043        2       0.9  BAGUETTE           
    ##  7    12 2021-01-02 09:25         150043        3       1.1  CROISSANT          
    ##  8    15 2021-01-02 09:27         150044        1       1.05 BANETTE            
    ##  9    18 2021-01-02 09:32         150045        3       1.2  TRADITIONAL BAGUET…
    ## 10    19 2021-01-02 09:32         150045        6       1.1  CROISSANT          
    ## # … with 233,990 more rows

### ANOVA

ANOVA tests whether there is a difference in means of the groups at each
level (each individual month) of the independent variable.

One way anova test can test if the mean sales of quarter 1 (Jan - Mar)
is different from the mean sales of quarter 3 (Jun - Aug).

The null hypothesis is that there is no difference in the mean sales of
1st quarter and 3rd quarter.  
The alternative hypothesis is that the means are different from one
another.

``` r
anova_df =
  bakery_df %>% 
  mutate(
    year = year(date),
    month = month(date),
    rev = quantity * unit_price) 

third_sales = 
  anova_df %>% 
  filter((month == 6)|(month == 7)|(month == 8)) %>% 
  group_by(year, month) %>% 
  summarize(third_sales = n()) %>% 
  group_by(year, month) %>% 
  mutate(ID = cur_group_id())
  
first_sales = 
  anova_df %>% 
  filter((month == 1) |(month == 2)|(month == 3)) %>% 
  group_by(year, month) %>% 
  summarize(first_sales = n()) %>% 
  group_by(year, month) %>% 
  mutate(ID = cur_group_id())
  
anova_test_df = 
  left_join(third_sales, first_sales, by = c("ID"))

anova_test_df
```

    ## # A tibble: 6 × 7
    ##   year.x month.x third_sales    ID year.y month.y first_sales
    ##    <dbl>   <dbl>       <int> <int>  <dbl>   <dbl>       <int>
    ## 1   2021       6       10856     1   2021       1        6562
    ## 2   2021       7       17592     2   2021       2        7817
    ## 3   2021       8       20344     3   2021       3        9233
    ## 4   2022       6       10678     4   2022       1        6273
    ## 5   2022       7       18242     5   2022       2        7795
    ## 6   2022       8       20135     6   2022       3        8251

``` r
one.way <- aov(first_sales ~ third_sales, data = anova_test_df)

summary(one.way)
```

    ##             Df  Sum Sq Mean Sq F value  Pr(>F)   
    ## third_sales  1 5350152 5350152   33.15 0.00451 **
    ## Residuals    4  645565  161391                   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The ANOVA test p-value is 0.00451 which is less than alpha level of
0.05, so we reject the null hypothesis and conclude that the mean sales
in quarter 1 is statistically significantly different from the mean
sales of quarter 3.

### Linear Regression

We are interested in testing the relationship between the unit price of
a product and its quantity being sold. Considering that the sales of
bakery varies by month, so we included `month` in our model as a
confounder to be controlled.

Linear Regression for baguette

``` r
baguette_df = 
  bakery_df %>% 
  filter(str_detect(product_name, "BAGUETTE")) %>% 
  mutate(
    month = month(date))

baguette_reg = lm(quantity ~ unit_price + month, baguette_df)

baguette_reg %>% 
  broom::tidy()
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   1.04     0.0276      37.8  4.79e-311
    ## 2 unit_price    0.460    0.0223      20.7  9.79e- 95
    ## 3 month         0.0107   0.00140      7.66 1.92e- 14

Linear Regression for croissant

``` r
croissant_df = 
  bakery_df %>% 
  filter(str_detect(product_name, "CROISSANT")) %>% 
  mutate(
    month = month(date))

croissant_reg = lm(quantity ~ unit_price + month, croissant_df)

croissant_reg %>% 
  broom::tidy()
```

    ## # A tibble: 3 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)   6.08     0.155       39.2  1.34e-317
    ## 2 unit_price   -3.25     0.129      -25.3  8.39e-138
    ## 3 month         0.0240   0.00577      4.17 3.10e-  5

### One-sample T-test

##### Regular Baguette (Two-sided)

We are interested in testing if the mean price of **regular baguette**
in this bakery is significantly different from the average price for a
baguette in Paris, which is 1.07 euros.

Null hypothesis: The mean price of baguette in this bakery is the same
as the average price of baguette in Paris.

Alternative hypothesis: The mean price of baguette in this bakery is
different from the average price of baguette in Paris.

``` r
baguette_onet = 
  bakery_df %>% 
  filter(product_name == "BAGUETTE") %>%
  select(unit_price)

baguette_t_results = 
  t.test(baguette_onet, mu = 1.07 , alternative = "two.sided") %>% 
  broom::tidy()
```

The p-value is much smaller than the alpha (0.05), so we would reject
the null hypothesis. At 5% level of significance, we have sufficient
evidence to conclude that the mean price of baguette in this bakery is
significantly different from the average price of baguette in Paris.

##### Traditional Baguette (One-sided)

We noticed that the price of traditional baguette in this bakery is
higher than the average price of traditional baguette in France. The
price of the traditional French loaf is around 0.90 Euros in bakeries.
Therefore, we would like to conduct a one-sided T-test to see if the
price difference is significant.

Null hypothesis: The mean price of traditional baguette in this bakery
is the same as the average price of traditional baguette in France.

Alternative hypothesis: The mean price of traditional baguette in this
bakery is higher than the average price of traditional baguette in
France.

``` r
bakery_df %>% 
  filter(product_name == "TRADITIONAL BAGUETTE") %>% 
  count(unit_price)
```

    ## # A tibble: 3 × 2
    ##   unit_price     n
    ##        <dbl> <int>
    ## 1       1.2  39426
    ## 2       1.25 13059
    ## 3       1.3  15204

``` r
trad_baguette_onet = 
  bakery_df %>% 
  filter(product_name == "TRADITIONAL BAGUETTE") %>%
  select(unit_price)

trad_baguette_t_results = 
  t.test(trad_baguette_onet, mu = 0.90, alternative = "greater") %>% 
  broom::tidy()
```

The p-value is much smaller than the alpha (0.05), so we would reject
the null hypothesis. At 5% level of significance, we have sufficient
evidence to conclude that the mean price of traditional baguette in this
bakery is significantly higher than the average price of baguette in
Paris.

### Generalized Additive Model

Find the *rush hours* of a typical day:

``` r
bakery_df = 
  bakery_df %>% 
  mutate(
    Hour = hour(time),
    Month = month(date))

bakery_df %>% 
  group_by(Hour) %>% 
  count() %>% 
  ggplot(aes(x = Hour, y = n)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(7, 20), limit = c(7, 20)) +
  scale_y_continuous(limit = c(0,50000)) +
  labs(
    title = "Peak hours",
    x = "Hour (24-hour format)",
    y = "Number of times appeared")
```

![](tests_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Overall, what hours has the most number of products sold?

``` r
bakery_df %>% 
  group_by(Hour) %>% 
  summarize(
    n_sold = sum(quantity))
```

    ## # A tibble: 14 × 2
    ##     Hour n_sold
    ##    <int>  <dbl>
    ##  1     7  13432
    ##  2     8  50412
    ##  3     9  57444
    ##  4    10  64496
    ##  5    11  69196
    ##  6    12  52254
    ##  7    13   8750
    ##  8    14    258
    ##  9    15    140
    ## 10    16  12393
    ## 11    17  16114
    ## 12    18  13607
    ## 13    19   1478
    ## 14    20      7

``` r
smooth_mod = gam(quantity ~ s(Hour) + s(Month), data = bakery_df)

smooth_mod %>% broom::tidy()
```

    ## # A tibble: 2 × 5
    ##   term       edf ref.df statistic p.value
    ##   <chr>    <dbl>  <dbl>     <dbl>   <dbl>
    ## 1 s(Hour)   6.89   7.78     347.        0
    ## 2 s(Month)  8.89   9.00      46.6       0
