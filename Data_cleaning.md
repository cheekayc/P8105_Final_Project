Data Cleaning
================
2022-12-04

``` r
knitr::opts_chunk$set(message = FALSE)

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.1     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
bakery_df =
  read_csv("./Data/Bakery_sales.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    unit_price = str_replace(unit_price, "€", ""),
    unit_price = str_replace(unit_price, ",", "."),
    unit_price = as.numeric(unit_price))

bakery_df
```

    ## # A tibble: 234,005 × 7
    ##       x1 date       time   ticket_number article              quantity unit_pr…¹
    ##    <dbl> <date>     <time>         <dbl> <chr>                   <dbl>     <dbl>
    ##  1     0 2021-01-02 08:38         150040 BAGUETTE                    1      0.9 
    ##  2     1 2021-01-02 08:38         150040 PAIN AU CHOCOLAT            3      1.2 
    ##  3     4 2021-01-02 09:14         150041 PAIN AU CHOCOLAT            2      1.2 
    ##  4     5 2021-01-02 09:14         150041 PAIN                        1      1.15
    ##  5     8 2021-01-02 09:25         150042 TRADITIONAL BAGUETTE        5      1.2 
    ##  6    11 2021-01-02 09:25         150043 BAGUETTE                    2      0.9 
    ##  7    12 2021-01-02 09:25         150043 CROISSANT                   3      1.1 
    ##  8    15 2021-01-02 09:27         150044 BANETTE                     1      1.05
    ##  9    18 2021-01-02 09:32         150045 TRADITIONAL BAGUETTE        3      1.2 
    ## 10    19 2021-01-02 09:32         150045 CROISSANT                   6      1.1 
    ## # … with 233,995 more rows, and abbreviated variable name ¹​unit_price
