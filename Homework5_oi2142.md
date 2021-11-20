Homework 5 \| Onyeka Isamah \| oi2142
================

# Problem 1

-   Cleaning the data, creating `city_state` and `resolution` variables:

``` r
homicide_df = read_csv("./homicide-data.csv", na = c("", "Unknown")) %>% 
  mutate(city_state = str_c(city, state),
         resolution = case_when(
           disposition == "Closed without arrest" ~ "unsolved",
           disposition == "Open/No arrest" ~ "unsolved",
           disposition == "Closed by arrest" ~ "solved"
         )) %>% 
  relocate(city_state) %>% 
  filter(city_state != "TulsaAL")
```

The **raw data set** has **14** variables and **52178** observations.
Variables include `uid`, `reported_date`, `victim_last`, `victim_first`,
`victim_race`, `victim_age`, `victim_sex`, `city`, `state`, `lat`,`lon`,
and `disposition`.

-   Focusing on Baltimore to estimate the proportion of unsolved
    homicides.

``` r
baltimore_df =
  homicide_df %>% filter(city_state == "BaltimoreMD")


baltimore_summary = baltimore_df %>% summarize(
  unsolved = sum(resolution == "unsolved"),
  n = n()
)
 
baltimore_test = 
  prop.test(
   x = baltimore_summary %>% pull(unsolved),
   n=  baltimore_summary %>%pull(n))
 

baltimore_test %>% 
  broom::tidy() %>%  knitr::kable()
```

|  estimate | statistic | p.value | parameter |  conf.low | conf.high | method                                               | alternative |
|----------:|----------:|--------:|----------:|----------:|----------:|:-----------------------------------------------------|:------------|
| 0.6455607 |   239.011 |       0 |         1 | 0.6275625 | 0.6631599 | 1-sample proportions test with continuity correction | two.sided   |

-   Iterating across cities:

``` r
prop_test_function = 
  function(city_df) {
    
    city_summary = 
      city_df %>% 
      summarize(
      unsolved = sum(resolution == "unsolved"),
             n = n()
)
 
    city_test = 
      prop.test(
        x = city_summary %>% pull(unsolved),
        n =  city_summary %>%pull(n))
    
    return(city_test)
  }

prop_test_function(baltimore_df)
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
    ## X-squared = 239.01, df = 1, p-value < 2.2e-16
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.6275625 0.6631599
    ## sample estimates:
    ##         p 
    ## 0.6455607

``` r
homicide_df %>% 
  filter(
    city_state == "AlbuquerqueNM"
  ) %>%  prop_test_function()
```

    ## 
    ##  1-sample proportions test with continuity correction
    ## 
    ## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
    ## X-squared = 19.114, df = 1, p-value = 1.232e-05
    ## alternative hypothesis: true p is not equal to 0.5
    ## 95 percent confidence interval:
    ##  0.3372604 0.4375766
    ## sample estimates:
    ##         p 
    ## 0.3862434

-   Iterating across all cities.

``` r
results_df = 
  homicide_df %>% 
  nest(data = uid:resolution) %>% 
  mutate(
    test_results = map(data, prop_test_function),
    tidy_results = map(test_results, broom::tidy)
  ) %>% 
  select(city_state, tidy_results) %>% 
  unnest(tidy_results) %>% 
  select(city_state, estimate, starts_with("conf"))
```

### Plot showing estimates and confidence intervals

![](Homework5_oi2142_files/figure-gfm/plot%20with%20estimates%20and%20ci-1.png)<!-- -->
