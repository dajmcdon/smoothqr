# JHU daily COVID-19 cases and deaths rates from all states

This data source of confirmed COVID-19 cases and deaths is based on
reports made available by the Center for Systems Science and Engineering
at Johns Hopkins University, as downloaded from the CMU Delphi COVIDcast
Epidata API. This example data is a snapshot as of March 10, 2023, and
ranges from December 31, 2020 to December 31, 2021. It includes all
states.

## Usage

``` r
covid_case_death_rates
```

## Format

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with
20496 rows and 4 columns.

## Source

This object contains a modified part of the [COVID-19 Data Repository by
the Center for Systems Science and Engineering (CSSE) at Johns Hopkins
University](https://github.com/CSSEGISandData/COVID-19) as [republished
in the COVIDcast Epidata
API](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html).
This data set is licensed under the terms of the [Creative Commons
Attribution 4.0 International
license](https://creativecommons.org/licenses/by/4.0/) by the Johns
Hopkins University on behalf of its Center for Systems Science in
Engineering. Copyright Johns Hopkins University 2020.

Modifications:

- [From the COVIDcast Epidata
  API](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html):
  These signals are taken directly from the JHU CSSE [COVID-19 GitHub
  repository](https://github.com/CSSEGISandData/COVID-19) without
  changes. The 7-day average signals are computed by Delphi by
  calculating moving averages of the preceding 7 days, so the signal for
  June 7 is the average of the underlying data for June 1 through 7,
  inclusive.

## Data dictionary

The data has columns:

- geo_value:

  the geographic value associated with each row of measurements.

- time_value:

  the time value associated with each row of measurements.

- case_rate:

  7-day average signal of number of new confirmed COVID-19 cases per
  100,000 population, daily

- death_rate:

  7-day average signal of number of new confirmed deaths due to COVID-19
  per 100,000 population, daily
