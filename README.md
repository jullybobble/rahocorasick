rahocorasick
================

An R interface to a [fast java implementation](https://github.com/hankcs/AhoCorasickDoubleArrayTrie) of the [Aho-Corasick](https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm) exact string search algorithm.

Disclaimer
----------

This package is in a very early stage of development. Important changes are expected in future versions.

Installation
------------

``` r
if(!any(rownames(installed.packages()) == "devtools")) {
  install.packages("devtools")
}
devtools::install_github("jullybobble/rahocorasick")
```

Usage
-----

``` r
library(rahocorasick)
dictionary <- list(one = 1, two = 2, three = 3)
trie <- ac_build_list(dictionary)
```

``` r
text_1 <- "one apple"
hit_1 <- ac_search(text_1, trie)[[1]]
hit_1
```

    ## # A tibble: 1 x 3
    ##   value begin   end
    ##   <chr> <int> <int>
    ## 1     1     1     3

``` r
ac_are_boundary_chars(text_1, hit_1$begin, hit_1$end)
```

    ## [1] TRUE

``` r
text_2 <- "a twoonie in my pocket"
hit_2 <- ac_search(text_2, trie)[[1]]
hit_2
```

    ## # A tibble: 1 x 3
    ##   value begin   end
    ##   <chr> <int> <int>
    ## 1     2     3     5

``` r
ac_are_boundary_chars(text_2, hit_2$begin, hit_2$end)
```

    ## [1] FALSE

``` r
text_3 <- "nothing left"
ac_build_and_search_list(c(text_1, text_2, text_3), dictionary)
```

    ## [[1]]
    ## # A tibble: 1 x 3
    ##   value begin   end
    ##   <chr> <int> <int>
    ## 1     1     1     3
    ## 
    ## [[2]]
    ## # A tibble: 1 x 3
    ##   value begin   end
    ##   <chr> <int> <int>
    ## 1     2     3     5
    ## 
    ## [[3]]
    ## # A tibble: 0 x 3
    ## # ... with 3 variables: value <chr>, begin <int>, end <int>
