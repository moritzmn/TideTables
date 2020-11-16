<!-- README.md is generated from README.Rmd. Please edit that file -->

This packages provides functions for synthesizing tide tables based on
observations. Ideally you have data for a specific measuring station for
the last 19 years without larger gaps. The functions are based on the
Harmonic Represantation of Inequalities (HRoI) and not on the harmonic
Method. Please consult the following links for a detailed description of
HRoI:

-   {<a href="https://www.bsh.de/DE/PUBLIKATIONEN/_Anlagen/Downloads/Meer_und_Umwelt/Berichte-des-BSH/Berichte-des-BSH_50_de.pdf?__blob=publicationFile&amp;v=13" class="uri">https://www.bsh.de/DE/PUBLIKATIONEN/_Anlagen/Downloads/Meer_und_Umwelt/Berichte-des-BSH/Berichte-des-BSH_50_de.pdf?__blob=publicationFile&amp;v=13</a>}

-   {<a href="https://www.ocean-sci.net/15/1363/2019/" class="uri">https://www.ocean-sci.net/15/1363/2019/</a>}

\#\#Why should i use this package? You should use this package for
producing tide tables from past data.

\#\#How do i use it?

Import your data set first and transform it to a readable form. See
attached data ‘observation’ for an example data frame.

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

You can now use your data as input for the function ‘TideTable’. Setting
the periods for analysis and synthesis and wait for the table to be
produced.

``` r
mytidetable <- TideTable(dataInput = observation, asdate = "1991/01/01", 
                         astime ="12:00:00", aedate = "1992/01/01", 
                         aetime = "12:00:00", ssdate = "1991/01/01", 
                         sstime = "12:00:00", sedate = "1992/01/01", setime = "12:00:00")
```

``` r
str(mytidetable)
#> List of 6
#>  $ c.table     :Classes 'data.table' and 'data.frame':   1412 obs. of  8 variables:
#>   ..$ transit               : num [1:1412] 33238 33238 33238 33238 33239 ...
#>   ..$ prediction_date       : chr [1:1412] "1991/01/01" "1991/01/01" "1991/01/02" "1991/01/02" ...
#>   ..$ prediction_time       : chr [1:1412] "13:12:25" "20:10:40" "01:32:26" "08:39:56" ...
#>   ..$ high_or_low_water     : num [1:1412] 1 0 1 0 1 0 1 0 1 0 ...
#>   ..$ upper_or_lower_transit: num [1:1412] 1 1 0 0 1 1 0 0 1 1 ...
#>   ..$ height                : num [1:1412] 6.42 3.34 6.54 3.29 6.63 ...
#>   ..$ st.transit            : num [1:1412] 12.2 19.1 24.5 31.6 12.3 ...
#>   ..$ i                     : num [1:1412] 1 1 1 1 2 2 2 2 3 3 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ tide.table  :Classes 'data.table' and 'data.frame':   1412 obs. of  4 variables:
#>   ..$ prediction_date  : chr [1:1412] "1991/01/01" "1991/01/01" "1991/01/02" "1991/01/02" ...
#>   ..$ prediction_time  : chr [1:1412] "13:12:25" "20:10:40" "01:32:26" "08:39:56" ...
#>   ..$ high_or_low_water: num [1:1412] 1 0 1 0 1 0 1 0 1 0 ...
#>   ..$ height           : num [1:1412] 6.42 3.34 6.54 3.29 6.63 ...
#>   ..- attr(*, ".internal.selfref")=<externalptr> 
#>  $ diff.analyse: num 353
#>  $ i.analyse   : int [1:4, 1:2] 353 353 353 353 345 346 348 348
#>   ..- attr(*, "dimnames")=List of 2
#>   .. ..$ : chr [1:4] "1" "2" "3" "4"
#>   .. ..$ : chr [1:2] "stunden.transit" "height"
#>  $ lm.coeff    :List of 4
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 11.84229 -0.01848 -0.03935 -0.01314 -0.00321 ...
#>   .. ..$ height         : num [1:43] 6.4492 -0.0433 -0.0899 0.0272 -0.0275 ...
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 18.6235 0.0111 -0.0266 0.0228 0.0115 ...
#>   .. ..$ height         : num [1:43] 3.483 -0.00302 -0.15132 -0.00388 -0.07565 ...
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 24.22335 -0.032761 -0.049149 0.005099 0.000365 ...
#>   .. ..$ height         : num [1:43] 6.3944 -0.0753 -0.0907 0.0401 -0.062 ...
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 31.0318 0.02195 -0.02426 0.00326 -0.00555 ...
#>   .. ..$ height         : num [1:43] 3.484 0.0327 -0.1543 0.0309 -0.0793 ...
#>  $ tmhwi       : num 0.501
```

As of Version 0.0.3 you can also use ‘BuildTT’ and ‘SynTT’. ‘BuildTT’
returns an object of class ‘tidetable’, which you can use in ‘SynTT’ to
synthesize a tide table. The model building and the synthesis is
therefore decoupled. Please note that given the same analysis
‘TideTable’ and ‘BuildTT’ + ‘SynTT’ will always return the same
synthesis (heights and times). The list item ‘c.table’ is equal to the
output of ‘SynTT’.

``` r
tt_model <- BuildTT(dataInput = observation, asdate = "1991/01/01", 
                         astime ="12:00:00", aedate = "1992/01/01", 
                         aetime = "12:00:00" )
```

``` r
str(tt_model)
#> List of 6
#>  $ diff.analyse: num 353
#>  $ omega_t     :List of 2
#>   ..$ : num [1:21] 1.02 2.04 11.71 13.52 15.96 ...
#>   ..$ : num [1:21] 1 2 3 5 7 8 9 10 11 12 ...
#>  $ tm24        : num 1.04
#>  $ tplus       : num 18262
#>  $ tmhwi       : num 0.501
#>  $ fitting.coef:List of 4
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 11.84229 -0.01848 -0.03935 -0.01314 -0.00321 ...
#>   .. ..$ height         : num [1:43] 6.4492 -0.0433 -0.0899 0.0272 -0.0275 ...
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 18.6235 0.0111 -0.0266 0.0228 0.0115 ...
#>   .. ..$ height         : num [1:43] 3.483 -0.00302 -0.15132 -0.00388 -0.07565 ...
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 24.22335 -0.032761 -0.049149 0.005099 0.000365 ...
#>   .. ..$ height         : num [1:43] 6.3944 -0.0753 -0.0907 0.0401 -0.062 ...
#>   ..$ :List of 2
#>   .. ..$ stunden.transit: num [1:43] 31.0318 0.02195 -0.02426 0.00326 -0.00555 ...
#>   .. ..$ height         : num [1:43] 3.484 0.0327 -0.1543 0.0309 -0.0793 ...
#>  - attr(*, "class")= chr "tidetable"
```

``` r
my_tt    <- SynTT(tmodel = tt_model, ssdate = "1991/01/01", 
                         sstime = "12:00:00", sedate = "1992/01/01", setime = "12:00:00")
```

``` r
str(my_tt)
#> Classes 'data.table' and 'data.frame':   1412 obs. of  8 variables:
#>  $ transit               : num  33238 33238 33238 33238 33239 ...
#>  $ prediction_date       : chr  "1991/01/01" "1991/01/01" "1991/01/02" "1991/01/02" ...
#>  $ prediction_time       : chr  "13:12:25" "20:10:40" "01:32:26" "08:39:56" ...
#>  $ high_or_low_water     : num  1 0 1 0 1 0 1 0 1 0 ...
#>  $ upper_or_lower_transit: num  1 1 0 0 1 1 0 0 1 1 ...
#>  $ height                : num  6.42 3.34 6.54 3.29 6.63 ...
#>  $ st.transit            : num  12.2 19.1 24.5 31.6 12.3 ...
#>  $ i                     : num  1 1 1 1 2 2 2 2 3 3 ...
#>  - attr(*, ".internal.selfref")=<externalptr>
```

``` r
all.equal(my_tt, mytidetable$c.table)
#> [1] TRUE
```
