hw5
================
Shan Jiang
11/2/2018

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ── Conflicts ────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(ggplot2)
library(broom)

set.seed(1)
```

## Problem 1

##### Inspecting the file path and file names in the zip.

``` r
file_path <- "./data/"
length(list.files(file_path))  # How many files are there?
```

    ## [1] 20

``` r
list.files(file_path)  # Show all names
```

    ##  [1] "con_01.csv" "con_02.csv" "con_03.csv" "con_04.csv" "con_05.csv"
    ##  [6] "con_06.csv" "con_07.csv" "con_08.csv" "con_09.csv" "con_10.csv"
    ## [11] "exp_01.csv" "exp_02.csv" "exp_03.csv" "exp_04.csv" "exp_05.csv"
    ## [16] "exp_06.csv" "exp_07.csv" "exp_08.csv" "exp_09.csv" "exp_10.csv"

``` r
list.files(file_path)[11:20]  # Show last 10 names
```

    ##  [1] "exp_01.csv" "exp_02.csv" "exp_03.csv" "exp_04.csv" "exp_05.csv"
    ##  [6] "exp_06.csv" "exp_07.csv" "exp_08.csv" "exp_09.csv" "exp_10.csv"

Create a tidy dataframe containing data from all participants, including
the subject ID, arm, and observations over time.

### Construct a dataframe using the `list.files` function

``` r
## Construct a function to read the csv file
read_sheet <- function(path) {
   df = readr::read_csv(path) %>% 
   janitor::clean_names()
   
   df
}

## Construct the path 

base = "./data/"
p = list.files(file_path)
vec_name = str_c(base, c(p))

control_df = map_df(vec_name, read_sheet)
```

  - Tidy the result: file names include the subject ID and arm.

<!-- end list -->

``` r
# Calling the ifelse function within mutate to label 2 Arms
control_df = control_df %>% 
  mutate(ID = 1:20) %>% 
  mutate(Arm = ifelse(ID < 11, yes = "control", no = "exprimental")) %>%
  select(Arm, ID, week_1:week_8) %>% 
  gather(key = No_week, value = measurement, week_1:week_8)
```

### Spaghetti plot

``` r
graph_1 = control_df %>% 
  ggplot(aes(x = No_week, y = measurement, group = ID)) +
  geom_line(aes(color = Arm))

graph_2 = control_df %>% 
ggplot(aes(x = No_week, y = measurement, group = ID)) +
  geom_line(aes(color = Arm)) + 
  facet_wrap(~ID) +
 theme(text = element_text(size = 9),
        axis.text.x = element_text(angle = 75, hjust = 1)) 

graph_1
```

![](hw5_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
graph_2
```

![](hw5_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

##### Comment on differences between groups.

From the above faceting Graph, we showed the absolute value of
measurement and the gap of the two groups.

1.  On averge, subjects in the Experimental group attains a higher
    measurement value than that of subjects in control group from week 1
    to 8.
2.  The experimental group presents a trend of increase across the
    weeks, which may indicates the exposure’s potential effect on the
    patients with the effect of time.
3.  The control group patients acquired a more stable measurement in its
    outcome value, so it may indicates a lack of effect of exposure.

## Problem 2

### Import the raw data.

``` r
require(RCurl)
```

    ## Loading required package: RCurl

    ## Loading required package: bitops

    ## 
    ## Attaching package: 'RCurl'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     complete

``` r
homicide_raw = read.csv( text = getURL("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"))
```

### Describe the raw data

The raw data focus on the homicide topic, which is originated from
Washington Post github page. It organizes a total of 52179 cases from
the year of 2007 to 2015, containing 12 variables ranging from the
demographical variables of the id, name, gender, age and race of the
victim to the geographical and time information of the case.

Next, we make a summary of the data in 51 US cities.

``` r
homicide_raw = homicide_raw %>% 
  mutate(city_state = str_c(city, state, sep = "," )) %>% 
  mutate(city_state = as.factor(city_state)) %>% 
  group_by(city_state) 
```

  - The total number of homicides grouped by cities, total city = 51.

<!-- end list -->

``` r
total_n  = homicide_raw %>% 
  group_by(city_state) %>%
  summarize(n = n()) 
```

  - The number of unsolved homicides grouped by cities, total city = 50.

<!-- end list -->

``` r
unsolved_n  = homicide_raw %>% 
  group_by(city_state) %>%
  filter(disposition %in% c("Closed without arrest", "Open/No arrest"))%>% 
  summarize(n = n()) 

## Summary of dataframe
homicide_sum = left_join(unsolved_n, total_n, by = "city_state") %>% 
  rename(unsolved =  n.x , total_cases = n.y )

head(homicide_sum)
```

    ## # A tibble: 6 x 3
    ##   city_state     unsolved total_cases
    ##   <fct>             <int>       <int>
    ## 1 Albuquerque,NM      146         378
    ## 2 Atlanta,GA          373         973
    ## 3 Baltimore,MD       1825        2827
    ## 4 Baton Rouge,LA      196         424
    ## 5 Birmingham,AL       347         800
    ## 6 Boston,MA           310         614

### Baltimore homicide rate test.

``` r
Bal_df = homicide_sum  %>% 
          filter(city_state == "Baltimore,MD")

# apply the broom::tidy to this object
result = prop.test(Bal_df$unsolved, Bal_df$total_cases,
          alternative = "two.sided",
          correct = TRUE)

bal_tidy = tidy(result) 
 
# pull the estimated proportion and confidence intervals from the resulting tidy dataframe.
estimated_prop = function(i){
    estimate = pull(bal_tidy[i])
    return(estimate)
}

tibble(estimate = estimated_prop(1),
conf.low = estimated_prop(5),
conf.high = estimated_prop(6))
```

    ## # A tibble: 1 x 3
    ##   estimate conf.low conf.high
    ##      <dbl>    <dbl>     <dbl>
    ## 1    0.646    0.628     0.663

### All cities: Use the `prop.test` function for 50 cities.

``` r
raw_result = map2(.x = homicide_sum$unsolved, .y = homicide_sum$total_cases, ~prop.test(.x, .y)) 

## Estimate and confidence interval for each city 
city_sum = map_df(.x = raw_result, ~.x %>% 
                    broom::tidy(.x) %>% 
                    select(estimate, conf.low, conf.high)) %>% 
  mutate(city_state = homicide_sum$city_state) %>% 
  select(city_state, everything())
```

### Plot that shows the estimates and CIs for each city

Organize cities according to the proportion of unsolved homicides.

``` r
city_sum %>% 
  mutate(city_state = factor(city_state, levels = city_state[order(estimate)])) %>% 
  ggplot(aes(x = city_state , y = estimate)) +
  ylim(0, 0.8) +
  geom_point(size = 0.3 ) +
  geom_errorbar(aes(ymin = conf.low , ymax = conf.high), width = .4) +
  theme_bw() +
  labs(
    x = "city and state",
    y = "ratio: unsolved cases in total cases",
    title = "Unsolved cases ratio in total cases across 50 years in the U.S",
    caption = "Data from the Washington Post"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, size = 7)
  ) 
```

![](hw5_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

  - Comments: The 50 cities in the dataset shows that Chicago is the
    city where the cases are not solved are the most, and it keeps a
    nearly 60% homicide cases unsettled.

  - Richmond keeps the lowest rate of unsolved cases ratio among the 50
    cities(around 20%).
