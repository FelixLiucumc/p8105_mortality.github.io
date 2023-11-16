data_cleaning
================
Candice Yu
2023-11-16

``` r
mortality_data <- read_csv("mortality_data.csv") %>%
  janitor::clean_names() %>%
  drop_na(outcome) 
```

    ## Rows: 1177 Columns: 51
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (51): group, ID, outcome, age, gendera, BMI, hypertensive, atrialfibrill...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
mortality_data %>%
  group_by(gendera) %>%
  summarise(
    count = n(), # total number of entries for each gender
    outcome_0 = sum(outcome == 0), # number of outcomes with value 0
    outcome_1 = sum(outcome == 1) # number of outcomes with value 1
  ) %>%
  knitr::kable()  
```

| gendera | count | outcome_0 | outcome_1 |
|--------:|------:|----------:|----------:|
|       1 |   558 |       478 |        80 |
|       2 |   618 |       539 |        79 |
