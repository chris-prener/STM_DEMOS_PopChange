St. Louis MSA Historic Population Change
================
Christopher Prener, Ph.D
(April 08, 2019)

## Introduction

This notebook creates a county-level historic data set for the
St. Louis, MO.

## Dependencies

This notebook requires a range of tidyverse and spatial packages:

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
library(readr)
library(stringr)
library(tidyr)

# spatial packages
library(measurements)
library(tidycensus)
library(tigris)
```

    ## To enable 
    ## caching of data, set `options(tigris_use_cache = TRUE)` in your R script or .Rprofile.

    ## 
    ## Attaching package: 'tigris'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     plot

``` r
library(sf)
```

    ## Linking to GEOS 3.6.1, GDAL 2.1.3, PROJ 4.9.3

``` r
# other packages
library(here)
```

    ## here() starts at /Users/chris/GitHub/Personal/STM_DEMOS_PopChange

## Load Data

This notebook requires historical data accessed from the [U.S. Census
Bureau](https://www.census.gov/population/www/censusdata/pop1790-1990.html).
These data have been manually cleaned to isolate counties of interest
from Missouri and Illinois, given the poor format of the original
spreadsheet.

``` r
historic <- read_csv(here("data", "historicPopulations.csv"))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_number(),
    ##   County = col_character(),
    ##   State = col_double()
    ## )

    ## See spec(...) for full column specifications.

## Convert to Long Form

The data were cleaned manually in as minimal fashion as possible, so
there is still additional cleaning to be done. We need to convert them
to long, change the county name to something distinct for pre-divorce
St. Louis, and re-order the data:

``` r
historic %>%
  gather(key = "year", value = "pop", -State, -County) %>%
  mutate(County = ifelse(County == "St. Louis" & year <= 1870, "St. Louis orig", County)) %>%
  arrange(State, County, year) %>%
  filter(is.na(pop) == FALSE) %>%
  rename(
    county = County,
    statefp = State
  ) %>%
  mutate(year = as.numeric(year)) -> historicLong
```

## Download Contemporary Census Data

Next, we’ll want to add data from 2000, 2010, and 2017 (the most recent
ACS year available) to our data set.

### 2000

We need to download the Missouri and Illinois data, do some minor
cleaning, and then combine them.

``` r
# create vector of missouri counties
counties <- c(29071, 29099, 29113, 29183, 29189, 29219, 29510)

# download county populations
moCounties <- get_decennial(geography = "county", variables = "P001001", year = 2000, state = 29) %>%
  filter(GEOID %in% counties) %>%
  mutate(statefp = 29)
```

    ## Getting data from the 2000 decennial Census

``` r
# create vector of illinois counties
counties <- c(17027, 17083, 17119, 17133, 17163)

# download county populations
ilCounties <- get_decennial(geography = "county", variables = "P001001", year = 2000, state = 17) %>%
  filter(GEOID %in% counties) %>%
  mutate(statefp = 17)
```

    ## Getting data from the 2000 decennial Census

``` r
# combine and clean
bind_rows(moCounties, ilCounties)%>%
  select(-variable) %>%
  rename(
    county = NAME,
    pop = value
  ) %>%
  mutate(county = str_trim(str_replace(string = county, pattern = "County", replacement = ""))) %>%
  mutate(year = 2000) %>%
  select(GEOID, county, statefp, year, pop) -> census2000
```

### 2010

We’ll repeat the process for 2010:

``` r
# create vector of missouri counties
counties <- c(29071, 29099, 29113, 29183, 29189, 29219, 29510)

# download county populations
moCounties <- get_decennial(geography = "county", variables = "P001001", year = 2010, state = 29) %>%
  filter(GEOID %in% counties) %>%
  mutate(statefp = 29) %>%
  mutate(NAME = str_trim(str_replace(string = NAME, pattern = ", Missouri", replacement = "")))
```

    ## Getting data from the 2010 decennial Census

``` r
# create vector of illinois counties
counties <- c(17027, 17083, 17119, 17133, 17163)

# download county populations
ilCounties <- get_decennial(geography = "county", variables = "P001001", year = 2010, state = 17) %>%
  filter(GEOID %in% counties) %>%
  mutate(statefp = 17) %>%
  mutate(NAME = str_trim(str_replace(string = NAME, pattern = ", Illinois", replacement = "")))
```

    ## Getting data from the 2010 decennial Census

``` r
# combine and clean
bind_rows(moCounties, ilCounties)%>%
  select(-variable) %>%
  rename(
    county = NAME,
    pop = value
  ) %>%
  mutate(county = str_trim(str_replace(string = county, pattern = "County", replacement = ""))) %>%
  mutate(year = 2010) %>%
  select(GEOID, county, statefp, year, pop) -> census2010
```

### 2017

The process is very similar for 2017, except that these are ACS data so
the variables requested and returned are different:

``` r
# create vector of missouri counties
counties <- c(29071, 29099, 29113, 29183, 29189, 29219, 29510)

# download county populations
moCounties <- get_acs(geography = "county", variables = "B01003_001", year = 2017, state = 29) %>%
  filter(GEOID %in% counties) %>%
  mutate(statefp = 29) %>%
  mutate(NAME = str_trim(str_replace(string = NAME, pattern = ", Missouri", replacement = "")))
```

    ## Getting data from the 2013-2017 5-year ACS

``` r
# create vector of illinois counties
counties <- c(17027, 17083, 17119, 17133, 17163)

# download county populations
ilCounties <- get_acs(geography = "county", variables = "B01003_001", year = 2017, state = 17) %>%
  filter(GEOID %in% counties) %>%
  mutate(statefp = 17) %>%
  mutate(NAME = str_trim(str_replace(string = NAME, pattern = ", Illinois", replacement = "")))
```

    ## Getting data from the 2013-2017 5-year ACS

``` r
# combine and clean
bind_rows(moCounties, ilCounties)%>%
  select(-variable, -moe) %>%
  rename(
    county = NAME,
    pop = estimate,
  ) %>%
  mutate(county = str_trim(str_replace(string = county, pattern = "County", replacement = ""))) %>%
  mutate(year = 2017) %>%
  select(GEOID, county, statefp, year, pop) -> acs2017
```

### Combine

Next, we’ll combine them and do some additional data cleaning to make
our analytic data set:

``` r
bind_rows(historicLong, census2000, census2010, acs2017) %>%
  arrange(statefp, county, year) %>%
  mutate(GEOID = case_when(
    county == "Clinton" ~ "17027",
    county == "Jersey" ~ "17083",
    county == "Madison" ~ "17119",
    county == "Monroe" ~ "17133",
    county == "St. Clair" ~ "17163",
    county == "Franklin" ~ "29071",
    county == "Jefferson" ~ "29099",
    county == "Lincoln" ~ "29113",
    county == "St. Charles" ~ "29183",
    county == "St. Louis" ~ "29189",
    county == "St. Louis city" ~ "29510",
    county == "Warren" ~ "29219"
  )) %>%
  mutate(countyfp = case_when(
    county == "Clinton" ~ 27,
    county == "Jersey" ~ 83,
    county == "Madison" ~ 119,
    county == "Monroe" ~ 133,
    county == "St. Clair" ~ 163,
    county == "Franklin" ~ 71,
    county == "Jefferson" ~ 99,
    county == "Lincoln" ~ 113,
    county == "St. Charles" ~ 183,
    county == "St. Louis" ~ 189,
    county == "St. Louis city" ~ 510,
    county == "Warren" ~ 219
  )) %>%
  select(GEOID, county, statefp, countyfp, year, pop) -> pop
```

## Clean-up Enviornment

Before we move on, we’ll drop unneeded
objects:

``` r
rm(acs2017, census2000, census2010, historic, historicLong, ilCounties, moCounties, counties)
```

## Write Data

Next, we’ll write these data to `.csv`:

``` r
write_csv(pop, here("data", "populationData.csv"))
```
