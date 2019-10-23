<!-- README.md is generated from README.Rmd. Please edit that file -->
Why should i use this package?
------------------------------

You should use this package for producing tide tables from past data.

How do i use it?
----------------

Import your data set first and transform it to a readable form. See attached data 'observation' for an example data frame.

``` r
library(TideTables)
observation[1:10, ]
#>    observation_date observation_time high_or_low_water height
#> 1        1991/01/01         00:45:40                 1  6.727
#> 2        1991/01/01         07:44:40                 0  4.337
#> 3        1991/01/01         13:15:29                 1  7.265
#> 4        1991/01/01         20:16:40                 0  3.667
#> 5        1991/01/02         01:17:48                 1  6.528
#> 6        1991/01/02         08:23:44                 0  3.037
#> 7        1991/01/02         14:24:03                 1  6.481
#> 8        1991/01/02         20:42:28                 0  4.015
#> 9        1991/01/03         02:38:25                 1  7.964
#> 10       1991/01/03         09:45:11                 0  4.302

sapply(observation,typeof)
#>  observation_date  observation_time high_or_low_water            height 
#>       "character"       "character"         "integer"          "double"
```

You can now use your data as input for the function 'TideTable'. Setting the periods for analyzing and synthesizing and wait for the table to be produced.

``` r
mytidetable<-TideTable(dataInput = observation, asdate = observation$observation_date[1], astime ="00:00:00", aedate= "1991/05/01", aetime = "00:00:00", ssdate = "2010/01/01", sstime = "00:00:00", sedate = "2011/01/01", setime = "00:00:00")
```
