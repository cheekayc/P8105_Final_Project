Data Cleaning
================
2022-12-04

``` r
knitr::opts_chunk$set(message = FALSE)

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

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
  drop_na(product_name) %>% 
  select(-article)

bakery_df
```

    ## # A tibble: 234,005 × 7
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
    ## # … with 233,995 more rows

One way anova test to see if the mean number of the sales are different
for summer (Jun - Aug) and winter (Dec - Feb)

``` r
anova_df =
  bakery_df %>% 
  mutate(
    month = month(date),
    rev = quantity *unit_price 
  ) 
  summer_sales = 
    anova_df %>% 
    filter((month == 6) |(month == 7)|(month == 8)) %>% 
  group_by(month) %>% 
  summarize(summer_sales = sum(rev))
  
  winter_sales = 
    anova_df %>% 
    filter((month == 1) |(month == 2)|(month == 12)) %>% 
    group_by(month) %>% 
  summarize(winter_sales = sum(rev))
```
