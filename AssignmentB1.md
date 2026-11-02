AssignmentB1
================
Suhaila Ng
2026-11-02

# Setup

I started with making sure that my environment is empty to ensure that
my function doesn’t depend on any objects I created earlier. No packages
need to be loaded for my function because it only uses base R. However,
to test it, you do need to load the ‘testthat’ package.

``` r
rm(list = ls())
```

# Exercise 1 and 2: Make and Document a Function

I wanted to make a function that would help during research involving
potted plants and treatments. My goal was for the function to assign
treatments to pots at random so that external factors like bench
position, light, or watering order do not line up with a particular
treatment and cause confounding results. Doing this by hand (or copying
and pasting sample() code for every experiment) is time consuming and
can lead to errors. So, this function builds a randomized treatment
layout for any number of treatments and pots. The layout is always
balanced (each treatment is used as evenly as possible), which is the
usual goal in an experiment. This function could also be used to assign
treatments to pretty much anything, not just pots. For example, if
they’re field sites receiving treatment, that works too.

``` r
#' Title: Randomizing Pot Treatments 
#'
#' Randomly assigns one treatment to each pot, using each treatment as evenly
#' as possible. If the pots can't be split equally, the extra pots go to
#' randomly chosen treatments.
#'
#' @param n_treatments Number of treatments (a single positive whole number).
#'   Named `n_treatments` because it is a count of treatments.
#' @param n_pots Total number of pots (a single positive whole number, at least
#'   `n_treatments`). Named `n_pots` to match `n_treatments`, and because pots
#'   are the experimental unit that receives a treatment in my study.
#' @param treatment_labels Optional character vector of treatment names, one per
#'   treatment. The default will be  "T1", "T2", etc. Named `treatment_labels`
#'   because these are the labels that appear in the output, not numeric counts.
#' @param seed Optional single number. If supplied, `set.seed(seed)` is called
#'   so the layout can be reproduced. Named `seed` because it is passed
#'   straight to `set.seed()`.
#'
#' @return A data frame with one row per pot and two columns: `pot_id`
#'   (integer, 1 to `n_pots`) and `treatment` (factor, with levels in the order
#'   of `treatment_labels`).
#'   
randomize_treatments <- function(n_treatments,
                                 n_pots,
                                 treatment_labels = NULL,
                                 seed = NULL) {
  if (n_pots < n_treatments) {
    stop("`n_pots` must be at least `n_treatments`.", call. = FALSE)
  }

  if (is.null(treatment_labels)) {
    treatment_labels <- paste0("T", seq_len(n_treatments))
  }
  if (length(treatment_labels) != n_treatments) {
    stop("`treatment_labels` must have one label per treatment.", call. = FALSE)
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Shuffle the labels (so extras go to random treatments), repeat them to
  # fill every pot, then shuffle again to randomize the pot order
  assigned <- sample(rep_len(sample(treatment_labels), n_pots))

  data.frame(
    pot_id    = seq_len(n_pots),
    treatment = factor(assigned, levels = treatment_labels)
  )
}
```

# Exercise 3: Examples

As a first, simple example, I will show 3 treatments and 10 pots. Since
10 doesn’t divide evenly by 3, one treatment gets an extra pot. I set a
`seed` so the layout is the same every time I knit this document.

``` r
layout <- randomize_treatments(n_treatments = 3, n_pots = 10, seed = 545)
layout
```

    ##    pot_id treatment
    ## 1       1        T1
    ## 2       2        T2
    ## 3       3        T1
    ## 4       4        T1
    ## 5       5        T3
    ## 6       6        T3
    ## 7       7        T2
    ## 8       8        T3
    ## 9       9        T2
    ## 10     10        T3

``` r
table(layout$treatment)
```

    ## 
    ## T1 T2 T3 
    ##  3  3  4

Next I am labelling the treatments using `treatment_labels`. The labels
show up in the output, and the factor levels keep the order.

``` r
named_layout <- randomize_treatments(
  n_treatments = 3,
  n_pots = 12,
  treatment_labels = c("control", "low_N", "high_N"),
  seed = 1
)
named_layout
```

    ##    pot_id treatment
    ## 1       1   control
    ## 2       2     low_N
    ## 3       3   control
    ## 4       4     low_N
    ## 5       5    high_N
    ## 6       6     low_N
    ## 7       7    high_N
    ## 8       8     low_N
    ## 9       9    high_N
    ## 10     10    high_N
    ## 11     11   control
    ## 12     12   control

``` r
table(named_layout$treatment)
```

    ## 
    ## control   low_N  high_N 
    ##       4       4       4

And lastly, the code below is deliberately showing an error when there
are fewer pots than treatments.

``` r
randomize_treatments(n_treatments = 5, n_pots = 3)
```

    ## Error:
    ## ! `n_pots` must be at least `n_treatments`.

# Exercise 4: Test the Function

In order to make sure that the function behaves as I intended, I wrote
formal tests with the `testthat` package. Each test covers a different
kind of input, so none of them repeat each other. The three inputs I am
testing are a normal input, the balance of the treatments, and an
invalid input. I also added a test to make sure that the seed argument
gives the same layout.

``` r
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.4.3

``` r
test_that("a normal input gives one row per pot", {
  out <- randomize_treatments(n_treatments = 3, n_pots = 10)
  expect_equal(nrow(out), 10)
})
```

    ## Test passed with 1 success 😀.

``` r
test_that("pots that divide evenly give each treatment the same number", {
  out <- randomize_treatments(n_treatments = 3, n_pots = 12)
  expect_equal(as.vector(table(out$treatment)), c(4, 4, 4))
})
```

    ## Test passed with 1 success 😀.

``` r
test_that("fewer pots than treatments gives an error", {
  expect_error(randomize_treatments(n_treatments = 5, n_pots = 3), "at least")
  
})
```

    ## Test passed with 1 success 🌈.

``` r
test_that("the same seed gives the same layout", {
  expect_identical(
    randomize_treatments(n_treatments = 3, n_pots = 10, seed = 545),
    randomize_treatments(n_treatments = 3, n_pots = 10, seed = 545)
  )
})
```

    ## Test passed with 1 success 🌈.
