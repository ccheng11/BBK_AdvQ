Tutorial: Analyzing Survey Data (Part 2)
================

Chao-Yo Cheng  
6 February 2021

## Introduction

By the end of this tutorial, you will know how to:

-   Review the main concepts of logit and GLMs.
-   Use logit regression to analyze survey data.
-   Perform model comparison on survey data.

We are building on our previous tutorial on survey weights this week.
Feel free to continue writing your code in the same file you used last
week.

First, let us bring in the same packages again.

``` r
library(survey)
library(srvyr)
library(dplyr)
library(ggplot2)
library(purrr)
```

## Data

We will continue to work with the data set from **the 2011 Canadian
National Election Study**. Here are our 9 variables again.

-   `id` – a unique identifier for each response.
-   `province` – a factor with (alphabetical) levels, including AB, BC,
    MB, NB, NL, NS, ON, PE, QC, SK (each of these refers to a Canadian
    province). The sample was “stratified” by province.
-   `population` – population of the respondent’s province (number of
    people over age 17).
-   `weight` – weight sample to size of population, taking into account
    unequal sampling probabilities by province and household size.
-   `abortion` – attitude toward abortion, a factor with levels `No` and
    `Yes`; answer to the question “Should abortion be banned?”
-   `gender` – a factor with two levels `Female` and `Male`.
-   `importance` – importance of religion, a factor with (alphabetical)
    levels including `not`, `notvery`, `somewhat`, `very`; answer to the
    question, “In your life, would you say that religion is very
    important, somewhat important, not very important, or not important
    at all?”
-   `education` – a factor with (alphabetical) levels including
    `bachelors` (Bachelors degree), `college` (community college or
    technical school), `higher` (graduate degree), HS (high-school
    graduate), `lessHS` (less than high-school graduate), `somePS` (some
    post-secondary).
-   `urban` – place of residence, a factor with levels rural, urban.

In this tutorial, we are interested in learning some of the variables
statistically associated with people’s attitudes towards abortion. The
outcome of variable of interest is therefore `abortion`.

> *Question: Anything wrong with the current set of
> explanatory/independent variables or predictors? Any other factors
> should we consider?*

Let’s read in the data and check that all of our variables are as
described.

``` r
ces <- read.csv("ces11.csv", stringsAsFactors = TRUE)
head(ces,10)
```

    ##      id province population  weight gender abortion importance education urban
    ## 1  2851       BC    3267345 4287.85 Female       No   somewhat    somePS urban
    ## 2   521       QC    5996930 9230.78   Male       No        not bachelors urban
    ## 3  2118       QC    5996930 6153.85   Male      Yes   somewhat   college urban
    ## 4  1815       NL     406455 3430.00 Female       No       very    somePS urban
    ## 5  1799       ON    9439960 8977.61   Male       No        not    higher rural
    ## 6  1103       ON    9439960 8977.61 Female       No        not    higher urban
    ## 7   957       NL     406455 3430.00 Female      Yes       very    lessHS rural
    ## 8  3431       NL     406455 1715.00 Female      Yes    notvery   college urban
    ## 9  2516       NL     406455 1715.00   Male       No       very   college urban
    ## 10  959       NL     406455 3430.00   Male      Yes       very    lessHS rural

In the following sections, we will start by using logit regression to
fit a bivariate model before we move on to talk about how to include
weights in the analysis.

## Use Logit Regression Without Weight

In this section, we will use the workhorse function `glm()` to estimate
the correlates of people’s attitudes toward abortion – to help with the
discussion, we will take the following steps:

-   Create a new dependent variable `against_abortion`, using the
    original variable `abortion` in the dataset.

-   Fit the logit model with no predictor.

-   Fit the logit model with one binary predictor `urban` (we may have
    to “mutate” this variable as well).

The main objective of this section is to make sure everyone is on the
same page with respect to logit and GLM before we move on to include
weights in the analysis.

### Create the Dependent Variable

First, let’s create a binary variable such that we will assign the value
of 1 to people who are against abortion in the study. If we use the
`tidyverse` style, then we will use the `mutate()` function after the
first pipeline.

Make sure you save the data frame with the new variable as the same or
another object, so we can continue to use it in the following analysis.

> *Question: Can you explain why it may be a good idea to save the data
> frame with the new variable `against_abortion` as a separate object in
> R?*

As the first step, let’s take a closer look at the variable `abortion`.
The built-in function `table()` is useful. You can use a `$` to call a
particular variable in the dataset.

``` r
table(ces$abortion)
```

    ## 
    ##   No  Yes 
    ## 1818  413

Let’s also check the variable type of `abortion`.

``` r
class(ces$abortion)
```

    ## [1] "factor"

It is a factor. Now, let’s use `mutate()` to generate a new variable
`against_abortion`.

``` r
ces_new <- ces %>%
  mutate(against_abortion = if_else(abortion == "Yes", 1, 0))
```

The `if_else()` function is very straightforward – it basically says:
*please assign the value of 1 to those who answered “Yes” and 0 to those
who answered the opposite.*

Now let’s check if we have done this properly. First, use `table()`
again.

``` r
table(ces_new$against_abortion)
```

    ## 
    ##    0    1 
    ## 1818  413

You can also include both variables to create a frequency table – the
first variable will be the rows and the second will be the columns.

``` r
table(ces_new$abortion, ces_new$against_abortion)
```

    ##      
    ##          0    1
    ##   No  1818    0
    ##   Yes    0  413

All observations are placed properly (i.e., no one is misclassified in
the new variable). We can continue.

> *Question: Can you create a new binary variable such that it assigns
> the value of 1 (and 0 otherwise) to those living in the urban area?*

### Logit Regression with No Predictor

Let’s start with a simple model with no predictor. You should recall the
logit regression transforms a **probability** *p* into **log odds**
$\\frac{p}{1-p}$ (or logged odds or log of odds, they are
interchangeable).

## Fitting GLMs With Weights

The `survey` package makes this very easy. Th function `svyglm()` is
identical to the conventional `glm()` function; except that `svyglm()`
has an option parameter called design (i.e., the survey object) instead
of the data frame.

Type `?svyglm` for more information about the function. Alternatively,
you can visit the `survey` package’s vignette page. The section
**Regression models** provides additional information.

In this section, we will carry out the following activities in `R`.

-   We want to use `mutate()` (in the `dplyr` package) for the survey
    object to add a binary variable called `against_abortion` such that
    it takes the value of 1 if the participant is against abortion
    (otherwise 0).

-   Use the `svyglm()` function to fit two logit regression models with
    and without weights.

-   Interpret the coefficients.

-   Compare the results with and without weights
