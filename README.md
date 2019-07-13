
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clownfishr

<!-- badges: start -->

<!-- badges: end -->

The goal of clownfishr is to standardize data retrieval from the Pinsky
Lab Leyte database. Using these funcitons ensures that we all grab the
same data in the same
way.

<!-- ## Installation -->

<!-- You can install the released version of clownfishr from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("clownfishr") -->

<!-- ``` -->

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(clownfishr)
```

Pull in the clownfish table data for some fish we are interested in:

``` r
fish_of_interest <- c("APCL18_201", "APCL17_355", "APCL15_115")
leyte <- read_db("Leyte")
fish <- get_fish() %>% 
  filter(sample_id %in% fish_of_interest)
```
