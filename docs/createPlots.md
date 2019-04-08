Population Trends
================
Christopher Prener, Ph.D.
(April 08, 2019)

## Introduction

This notebook creates an initial series of plots for population changes
in the St. Louis metro area.

## Dependencies

This notebook requires a number of `tidyverse` packages:

``` r
# tidyverse packages
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readr)

# other packages
library(here)
```

    ## here() starts at /Users/chris/GitHub/Personal/STM_DEMOS_PopChange

``` r
library(prener)
library(RColorBrewer)
library(scales)
```

    ## 
    ## Attaching package: 'scales'

    ## The following object is masked from 'package:readr':
    ## 
    ##     col_factor

## Load Data

This notebook requires the previously created population data for the
St. Louis MSA (see `createData.Rmd`):

``` r
pop <- read_csv(here("data", "populationData.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   GEOID = col_double(),
    ##   county = col_character(),
    ##   statefp = col_double(),
    ##   countyfp = col_double(),
    ##   year = col_double(),
    ##   pop = col_double()
    ## )

## St. Louis Population

First, we’ll make a plot for just the City of St. Louis and St. Louis
County:

``` r
pop %>%
  filter(county == "St. Louis" | county == "St. Louis city" | county == "St. Louis orig") %>%
  mutate(county = ifelse(county == "St. Louis orig", "St. Louis, pre-1876", county)) %>%
  ggplot(mapping = aes(year, pop, by = county)) +
    geom_line(aes(color = county), size = 2) +
    scale_x_continuous(breaks = c(1800, 1820, 1840, 1860, 1880, 1900, 1920, 1940, 1960, 1980, 2000, 2017)) +
    scale_colour_brewer("County", palette = "Set1") +
    labs(
      title = "St. Louis Historic Population Change",
      subtitle = "Before and After the Great Divorce (1876)",
      caption = "Plot by Christopher Prener, Ph.D.\nData via U.S. Census Bureau",
      x = "Year",
      y = "Population"
    ) +
  cp_sequoiaTheme(background = "gray", base_size = 23) -> plot

# save plot
cp_plotSave(filename = here("results", "historicChange.png"), plot = plot, preset = "med")

# print plot
plot
```

![](createPlots_files/figure-gfm/plot-city-county-1.png)<!-- -->

Next, we’ll plot out population change in the Missouri Counties outside
of St. Louis City and County but in the MSA:

``` r
pop %>%
  filter(year >= 1940) %>%
  filter(statefp == 29) %>%
  filter(county != "St. Louis" & county != "St. Louis city") %>%
  ggplot(mapping = aes(year, pop, by = county)) +
    geom_line(aes(color = county), size = 2) +
    scale_y_continuous(labels = comma) + 
    scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2017)) +
    scale_colour_brewer("County", palette = "Set1") +
    labs(
      title = "Suburban and Exurban Growth, 1940-present",
      subtitle = "St. Louis MSA Counties (Missouri only)",
      caption = "Plot by Christopher Prener, Ph.D.\nData via U.S. Census Bureau",
      x = "Year",
      y = "Population"
    ) +
  cp_sequoiaTheme(background = "gray", base_size = 23) -> plot

# save plot
cp_plotSave(filename = here("results", "suburbanMissouri.png"), plot = plot, preset = "med")

# print plot
plot
```

![](createPlots_files/figure-gfm/plot-mo-1.png)<!-- -->

Finally, we’ll plot out population change in the Illinois Counties in
the MSA:

``` r
pop %>%
  filter(year >= 1940) %>%
  filter(statefp == 17) %>%
  ggplot(mapping = aes(year, pop, by = county)) +
    geom_line(aes(color = county), size = 2) +
    expand_limits(y = 300000) + 
    scale_y_continuous(labels = comma, breaks = c(0, 100000, 200000, 300000)) + 
    scale_x_continuous(breaks = c(1940, 1950, 1960, 1970, 1980, 1990, 2000, 2010, 2017)) +
    scale_colour_brewer("County", palette = "Set1") +
    labs(
      title = "Suburban and Exurban Growth, 1940-present",
      subtitle = "St. Louis MSA Counties (Illinois only)",
      caption = "Plot by Christopher Prener, Ph.D.\nData via U.S. Census Bureau",
      x = "Year",
      y = "Population"
    ) +
  cp_sequoiaTheme(background = "gray", base_size = 23) -> plot

# save plot
cp_plotSave(filename = here("results", "suburbanIllinois.png"), plot = plot, preset = "med")

# print plot
plot
```

![](createPlots_files/figure-gfm/plot-illinois-1.png)<!-- -->
