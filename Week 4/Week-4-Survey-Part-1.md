Something fascinating
================
Jenny Bryan
2022-01-31

# Tutorial: Analyzing Survey Data (Part 1)

Chao-Yo Cheng  
31 January 2022

## Before You Start: Descriptive Statistics Using R

-   “[Quick R](https://www.statmethods.net/stats/descriptives.html)”
    (Robert I. Kabacoff):
-   “[Descriptive Statistics in R](https://statsandr.com/blog/)”
    (Antoine Soetewey):
    descriptive-statistics-in-r/#coefficient-of-variation
-   “[Modern R with the
    Tidyverse](https://b-rodrigues.github.io/modern_R/)” (Bruno
    Rodrigues) (Chapter 4)

## 1 Introduction

Our objectives today include:

-   Set up a survey object using complex survey information, such as
    sample weight and stratification variables.
-   Use a `tidyverse` approach to study descriptive statistics.

We will use the package `survey` and a tidyverse-style wrapper called
`srvyr`.

``` r
library(survey)
```

    ## Loading required package: grid

    ## Loading required package: Matrix

    ## Loading required package: survival

    ## 
    ## Attaching package: 'survey'

    ## The following object is masked from 'package:graphics':
    ## 
    ##     dotchart

``` r
library(srvyr)
```

    ## 
    ## Attaching package: 'srvyr'

    ## The following object is masked from 'package:stats':
    ## 
    ##     filter

If you have yet to install the packages, use the function
`install.packages()`.

``` r
install.packages("survey")
install.packages("srvyr")
```

Make sure you load the following packages, too:

``` r
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
library(purrr)
```

> *Question: Can you use a few words (or sentences) to describe what
> each of these packages does?*

## 2 Data

The sample dataset for today’s tutorial is from the 2011
<a href="http://www.ces-eec.ca/?target=_blank" target="_blank">Canadian
National Election Study</a>.

We will use the data stored in the `carData` package. You can also find
the .csv file on **Moodle**.

There are 2,231 observations on the following 9 variables:

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

> *Question: What is the unit of observation?*

> *Question: When should we use stratified sampling? And how does that
> decision influence the distribution of respondents from different
> provinces when we do not use stratified sample?*

> *Question: What is the main dependent variable of interest?*

> *Question: Anything wrong with the current set of
> explanatory/independent variables or predictors? Any other factors
> should we consider?*

> *Question: Can we conduct survey research with children in our sample?
> How should we do that?*

Now, set up your working directory and read the data into R.

``` r
ces <- read.csv("ces11.csv", stringsAsFactors=TRUE)
```

Note that here we set `stringsAsFactors=TRUE` so that the variables that
are meant to be factors are set up accordingly.

> *Question: How do we verify whether or not a variable is a factor in
> `R`? If it is not, which `R` function should we use to force a
> variable to be a factor?*

## 3 Survey Design Components

The following variables in the dataset provide the information on the
survey design. Using the variable list above, recall that

-   `id` is a unique identifier for each observation.
-   `province` – the sampling was stratified by province (random
    sampling by landline numbers was done within province).
-   `population` provides the population size of each province.
-   `weight` is calculated based on differences in province population,
    the study sample size therein, and household size.

``` r
ces %>%
 select(id, province, population, weight) %>%
 head(6)
```

    ##     id province population  weight
    ## 1 2851       BC    3267345 4287.85
    ## 2  521       QC    5996930 9230.78
    ## 3 2118       QC    5996930 6153.85
    ## 4 1815       NL     406455 3430.00
    ## 5 1799       ON    9439960 8977.61
    ## 6 1103       ON    9439960 8977.61

> *Question: What does “select” and “head” do, respectively?*

> *Question: Think again. How might the sample look differently if CNES
> used simple random sampling? Can you justify why CNES should use
> stratified sampling rather than simple random sampling? If they use
> stratified sampling, in what sense is the sample representative?*

To use the functions contained in the `survey` and `srvyr` packages, we
have to turn the `dataframe` into a `survey` objective.

``` r
ces_s <- ces %>%
 as_survey(ids = id,
      strata = province,
      fpc = population,
      weights = weight)
```

> *Question: Again, how can we verify that `ces_s` is really a `survey`
> object?*

> *Question: In the function `as_survey`, there are four options you
> have to fill in – find out what they mean according to the official
> package manual.*

## 4 Computing Descriptive Statistics

Complete the following tasks.

-   Drawing on the dataframe `ces`, use `tidyverse` functions to
    calculate the number of people who think abortion should be banned
    (and perhaps the proportion of people who think abortion should be
    banned).
-   Repeat the same analysis, but this time use the survey object
    `ces_s`.

> *Question: Describe and explain your observations.*

> *Question: Can you think of other descriptive analysis you can do?*

Here is one example (more TBA after the live session) – say, we want to
use the dataframe `ces` to calculate the number of people who think
abortion should be banned.

``` r
ces %>%
 group_by(abortion) %>%
 summarise(n = n())
```

> *Question: How else can you get the table?*

Now let’s use `ces_s` (i.e., the survey object).

``` r
ces %>%
 group_by(abortion) %>%
 summarise(n = survey_total())
```

> *Question: What does `survey_total` do?*

## Additional References

John Fox and Sanford Weisberg: “Fitting regression models to data from
complex surveys.” – walk through the exercise using the `survey`
package.

Greg Freedman: [“`srvyr` compared to the `survey`
package”](https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html)
– a short tutorial from the package’s author.

Arnold Lau: [“Exploring survey data with the `pewmethods` R
package”](https://medium.com/pew-research-center-decoded/exploring-survey-data-with-the-pewmethods-r-package-198c4eb9d1af).
– note this is one of a series articles where Pew researchers introduce
how the package they made is a good alternative to the packages we use
here.

## Extra: European Social Survey (Round 9)

*This section is contributed by Dr [Andi
Fugard](https://natcen.ac.uk/about-us/people/staff/andi-fugard) (NatCen)
with some minor revisions by Chao-yo.*

Let us work on another example using the
<a href="https://www.europeansocialsurvey.org/download.html?file=ESS9e03_1&y=2018" target="_blank">European
Social Survey</a> (Round 9).

To import the `SPSS` data file into `R`, we can use the package `haven`.

> *Question: The package `haven` is very powerful. What other types of
> data files can we import into `R` by using this package?*

``` r
library(haven)
ess9 <- read_sav("ESS9e03.sav")
```

Whenever you import a new dataset, you should see it.

``` r
View(ess9)
```

> *Question: Any other functions we can use to “see” the data?*

> *Question: What if you just want to list variable names?*

> \*Question: Describe your observations.

> *Question: Perhaps you want to read the codebook – where is it on the
> ESS website? It is a good exercise to spend some time looking through
> the documentation on the ESS website to find the variable name.*

If that has worked – then the next problem is finding the variables we
want to analyze!

Read the following discussion from the report published by the
<a href="https://www.bsa.natcen.ac.uk/latest-report/british-social-attitudes-37/fairness-and-justice-in-britain.aspx" target="_blank">British
Social Attitudes</a>:

> > Only 20% of the British public think that differences in wealth in
> > Britain are fair, whilst a majority (59%) think that wealth
> > differences in Britain are unfairly large and a further 16% think
> > that differences in wealth are unfairly small.

The original question was

> > In your opinion, are differences in wealth in Britain unfairly
> > small, fair, or unfairly large?

We will also need the variable for country (easier to spot) and any
information required for setting up the survey object. The ESS website
advises we “must weight tables before quoting percentages from the
survey.” See the guide Weighting European Social Survey Data for fuller
details about which weights to use (it is on Moodle).

> > The *Design weights* (`DWEIGHT`) adjust for different selection
> > probabilities, while the *Post-stratification* weights (`PSPWGHT`)
> > adjust for sampling error and non-response bias, as well as
> > different selection probabilities. Either `DWEIGHT` or `PSPWGHT`
> > must always be used. In addition, the *Population size weights*
> > (`PWEIGHT`) should be applied if you are looking at aggregates or
> > averages for two or more countries combined.

### 5.1 Tabulate the data by country

First, let’s make the country variable look a bit nicer. It currently
looks like this:

``` r
table(ess9$cntry)
```

But the dataset also has nicer labels included, which we can get like
this using the function `as_factor` (note the underscore). This function
is in the package `haven`.

``` r
ess9$cntry <- as_factor(ess9$cntry, levels = "labels")
table(ess9$cntry)
```

> *Question: Describe the differences in the output.*

### 5.2 Set up the `survey` object

Now let’s set up the survey object:

The `nest` option takes account of the `ids` being nested within strata
– in other words, the same ID is used only once in a country although it
is used more than once across the dataset.

``` r
ess9_survey <- ess9 %>%
 as_survey_design(ids = idno,
          strata = cntry,
          nest = TRUE,
          weights = pspwght)
```

### 5.3 Try out some analysis

The country variable is `cntry` and the wealth variable is `wltdffr`.
They are both explained in the online documentation.

The first thing you will spot when looking at the possible values of
this variable is that the original variable is coded from -4 to 4.

``` r
unique(ess9$wltdffr)
```

Let’s create another variable that is grouped the same way as in the
report:

``` r
ess9_survey <- ess9_survey %>%
 mutate(wltdffr_group =
      case_when(
       wltdffr >= -4 & wltdffr <= -1 ~ "Unfairly small",
       wltdffr == 0 ~ "Fair",
       wltdffr >= 1 & wltdffr <= 4 ~ "Unfairly large"),
     wltdffr_group = factor(wltdffr_group,
                levels = c("Unfairly small",
                      "Fair",
                      "Unfairly large"))
 )
```

> *Question: Can you repeat the same analysis without using the
> `tidyverse` pipelines?*

Great, now let’s see what the UK data look like for this grouped
variable:

``` r
gb_wealth <- ess9_survey %>%
 filter(cntry == "United Kingdom") %>%
 group_by(wltdffr_group) %>%
 summarise(prop = survey_mean(vartype = "ci"))
gb_wealth 
```

Let’s round our results to see more clearly that they match the report:

``` r
gb_wealth %>%
 mutate(perc = (prop*100) %>% round(0)) %>%
 select(wltdffr_group, perc) 
```

We can also plot the results:

``` r
gb_wealth %>%
 filter(!is.na(wltdffr_group)) %>%
 ggplot(aes(x = wltdffr_group, y = prop*100)) +
 geom_col(fill = "#B053A1") +
 geom_errorbar(aes(ymin = prop_low*100,
          ymax = prop_upp*100), width = 0.2) +
 ylim(0,100) +
 labs(y = "%", x = NULL,
    title = "In your opinion, are differences in wealth in Britain\nunfairly small, fair, or unfairly large?")
```

Let’s do it again for a selection of countries. First, make a function
which carries out the analysis for one country:

``` r
get_country_results <- function(the_cntry) {
 ess9_survey %>%
  filter(cntry == the_cntry) %>%
 group_by(wltdffr_group) %>%
 summarise(prop = survey_mean(vartype = "ci")) %>%
 mutate(cntry = the_cntry)
}
```

Check it works for the UK:

``` r
get_country_results("United Kingdom")
```

Run it for your countries of interest:

``` r
conts <- c("Germany", "Spain", "France", "United Kingdom", "Italy")
euro_wealth <- map_dfr(conts, get_country_results)
head(euro_wealth)
```

That’s a lot of numbers! Let’s try a plot:

``` r
euro_wealth %>%
 filter(!is.na(wltdffr_group)) %>%
 ggplot(aes(x = cntry,
       y = prop*100,
       ymin = prop_low*100,
       ymax = prop_upp*100,
       fill = wltdffr_group)) +
 geom_col(position = position_dodge(width = .8), width = 0.6) + 
 geom_errorbar(position=position_dodge(width = .8),
        colour="black",
        width = 0.2) +
 ylim(0,100) +
 labs(y = "%", x = NULL,
    title = "In your opinion, are differences in wealth\nunfairly small, fair, or unfairly large?",
    fill = NULL)
```