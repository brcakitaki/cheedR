
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cheedR

<!-- badges: start -->

<!-- badges: end -->

A package for turning messy La Trobe University student enrolment
extracts into tidy data for CHEEDR analysis.

## Introduction

Blessed users, as you likely already know, one of the most painful parts
of working with data at CHEEDR is extracting student data through the La
Trobe University Business Intelligence interface. Bad news - this
package cannot alleviate that pain. Good news - all the painful stuff
after extracting data is much less horrid thanks to this package.

## Installation

You can install a development version of `cheedR` from github with:

``` r
devtools::install_github("brcakitaki/cheedR")
```

If you have been living under the proverbial rock and not yet installed
the `tidyverse` suite of packages, do it, and then load it into the
environment:

``` r
# install.packages("tidyverse")
require(tidyverse)
```

The cheedR package requires clean column names in the
underscore\_separated style, so we need the `janitor` package to do
this:

``` r
# install.packages("janitor")
require(janitor)
```

We will use the nifty `skimr` package to check out our data, so install
and attach:

``` r
# install.packages("skimr")
require(skimr)
```

 

# 1\. Getting the data

Lets walk through some of the routine steps for extracting student
enrolment data at La Trobe. Open ghastly BI. Imagine you are analysing
student retention by campus and over time, and you want to calculate 6
year completion rates. You want a comprehensive set of enrolment
variables, over say 7 or 8 years. That is big data.

  - First roadblock: there is an extraction limit for BI. The size of
    the limit is actually quite mysterious. Indeed, scholars have spent
    years estimating. Trial and error dictates it is something close to
    250MB. You might spend a bit of time chasing this issue up with ICT,
    the vendor, and then your therapist. A couple of days, a teary phone
    call to mum, and 6 high cholesterol comfort meals later, you abandon
    fixing the root of problem. Gather your safety pins and duct tape,
    its time to find a work around.

  - Your solution is to extract the data in batches. The BI student
    study package directory provides some natural delineation for
    organising the batches.  
    You are also a really organised, highly motivated self-starter with
    a genuine passion for filing, so you save your BI queries for later
    use.

  - The batches I compile my queries along are by: student postcode
    variables; parents’ educational attainment variables; course
    enrolment variables; subject enrolment variables; student
    demographic variables; and prior educational attainment variables.
    For smaller projects, some of these groupings can be merged at the
    query level. Save the extractions as `.csv` files.

## Loading in and joining the raw data (functions coming soon)

``` r
require(cheedR)
#> Loading required package: cheedR
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#> 
#>     date
#> Loading required package: glue
#> 
#> Attaching package: 'glue'
#> The following object is masked from 'package:dplyr':
#> 
#>     collapse

## Reading in the data
```

## Cleaning the data

Sometimes data extracted from BI contains cases that are just wrong.
Other times the we may want to further simplify a variable. The variable
‘latest\_atar’ often has several rogue ATARs in the mix, for instance
some students are recorded with three or four digit long ATARs (i.e an
ATAR of 5405). While there are some really smart kids in Australia, none
as of yet have been able to get an ATAR larger than 100. So, we should
trim the variable. The function ‘ATAR\_trim()’, trims away ATAR values
below 10, and above 100, and replaces them with NAs.

We often want an indicator of whether or not the student is Indigenous.
The function ‘Indigenous\_var()’ creates a new dichotomous Indigenous
student variable from the BI variable
‘aboriginal\_or\_torres\_strait\_islander\_code’.

The following code performs the cleaning:

``` r

## Trim ATARs
clean_df <- LTU.df %>% 
  ATAR_trim() %>% 
  Indigenous_var()
```

Lets see how our ‘atar’ variable has changed. In the output below, we
see that the ‘atar’ variable has trimmed ATARs below 10 and above 100.

``` r
clean_df %>%
  select(latest_atar,
         atar) %>% 
  skim()
#> Skim summary statistics
#>  n obs: 108846 
#>  n variables: 2 
#> 
#> -- Variable type:numeric ------------------------------------------------------------------------------------------------------
#>     variable missing complete      n  mean     sd    p0   p25  p50   p75   p100
#>         atar   42075    66771 108846 69.79  16.09 10.75 58    70.1 82.15   99.9
#>  latest_atar   41766    67080 108846 73.94 184.17  0    57.85 70   82.1  9555  
#>      hist
#>  <U+2581><U+2581><U+2582><U+2585><U+2587><U+2587><U+2587><U+2585>
#>  <U+2587><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581>
```

Now lets have a look at our new ‘Indigenous’ variable. The ‘Indigenous’
variable is now dichotomous, compared to the previous 5 value
categorical.

``` r
clean_df %>% 
  select(aboriginal_or_torres_strait_islander_code,
         Indigenous) %>% 
  table()
#>                                          Indigenous
#> aboriginal_or_torres_strait_islander_code      N      Y
#>                                         B      0     30
#>                                         N 105201      0
#>                                         T      0     34
#>                                         X    727      0
#>                                         Y      0    744
```

## Joining external variables
