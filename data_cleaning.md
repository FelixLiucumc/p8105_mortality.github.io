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

``` r
# Define age intervals
age_breaks <- c(-Inf, 20, 40, 60, 80, Inf)
age_labels <- c('Under 20', '20-40', '40-60', '60-80', 'Over 60')

# Create age groups and summarize outcomes
mortality_data %>%
  mutate(age_group = cut(age, breaks = age_breaks, labels = age_labels, right = FALSE)) %>%
  group_by(age_group) %>%
  summarise(count = n(),
            outcome_0 = sum(outcome == 0, na.rm = TRUE),
            outcome_1 = sum(outcome == 1, na.rm = TRUE),
            percentage = outcome_1/(outcome_0 + outcome_1)) %>%
  knitr::kable(digits = 3)  
```

| age_group | count | outcome_0 | outcome_1 | percentage |
|:----------|------:|----------:|----------:|-----------:|
| Under 20  |     2 |         2 |         0 |      0.000 |
| 20-40     |    16 |        15 |         1 |      0.062 |
| 40-60     |   158 |       138 |        20 |      0.127 |
| 60-80     |   492 |       435 |        57 |      0.116 |
| Over 60   |   508 |       427 |        81 |      0.159 |
