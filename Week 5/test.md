::: {.container-fluid .main-container}
::: {#header}
Tutorial: Analyzing Survey Data (Part 2) {#tutorial-analyzing-survey-data-part-2 .title .toc-ignore}
========================================

####  {#section .author}
:::

Chao-Yo Cheng\
6 February 2021

::: {#introduction .section .level2}
Introduction
------------

By the end of this tutorial, you will know how to:

-   Review the main concepts of logit and GLMs.
-   Use logit regression to analyze survey data.
-   Perform model comparison on survey data.

We are building on our previous tutorial on survey weights this week.
Feel free to continue writing your code in the same file you used last
week.

First, let us bring in the same packages again.

``` {.r}
library(survey)
library(srvyr)
library(dplyr)
library(ggplot2)
library(purrr)
```
:::

::: {#data .section .level2}
Data
----

We will continue to work with the data set from **the 2011 Canadian
National Election Study**. Here are our 9 variables again.

-   `id` -- a unique identifier for each response.
-   `province` -- a factor with (alphabetical) levels, including AB, BC,
    MB, NB, NL, NS, ON, PE, QC, SK (each of these refers to a Canadian
    province). The sample was "stratified" by province.
-   `population` -- population of the respondent's province (number of
    people over age 17).
-   `weight` -- weight sample to size of population, taking into account
    unequal sampling probabilities by province and household size.
-   `abortion` -- attitude toward abortion, a factor with levels `No`
    and `Yes`; answer to the question "Should abortion be banned?"
-   `gender` -- a factor with two levels `Female` and `Male`.
-   `importance` -- importance of religion, a factor with (alphabetical)
    levels including `not`, `notvery`, `somewhat`, `very`; answer to the
    question, "In your life, would you say that religion is very
    important, somewhat important, not very important, or not important
    at all?"
-   `education` -- a factor with (alphabetical) levels including
    `bachelors` (Bachelors degree), `college` (community college or
    technical school), `higher` (graduate degree), HS (high-school
    graduate), `lessHS` (less than high-school graduate), `somePS` (some
    post-secondary).
-   `urban` -- place of residence, a factor with levels `rural`,
    `urban`.

In this tutorial, we are interested in learning some of the variables
statistically associated with people's attitudes towards abortion. The
outcome of variable of interest is therefore `abortion`.

> *Question: Anything wrong with the current set of
> explanatory/independent variables or predictors? Any other factors
> should we consider?*

Let's read in the data and check that all of our variables are as
described.

``` {.r}
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
:::

::: {#use-logit-regression-without-weights .section .level2}
Use Logit Regression Without Weights
------------------------------------

In this section, we will use the workhorse function `glm()` to estimate
the correlates of people's attitudes toward abortion -- to help with the
discussion, we will take the following steps:

-   Create a new dependent variable `against_abortion`, using the
    original variable `abortion` in the dataset.

-   Fit the logit model with no predictor.

-   Fit the logit model with one binary predictor `urban` (we may have
    to "mutate" this variable as well).

The main objective of this section is to make sure everyone is on the
same page with respect to logit and GLM before we move on to include
weights in the analysis.

::: {#create-the-dependent-variable .section .level3}
### Create the Dependent Variable

First, let's create a binary variable such that we will assign the value
of 1 to people who are against abortion in the study. If we use the
`tidyverse` style, then we will use the `mutate()` function after the
first pipeline.

Make sure you save the data frame with the new variable as the same or
another object, so we can continue to use it in the following analysis.

> *Question: Can you explain why it may be a good idea to save the data
> frame with the new variable `against_abortion` as a separate object in
> R?*

As the first step, let's take a closer look at the variable `abortion`.
The built-in function `table()` is useful. You can use a `$` to call a
particular variable in the dataset.

``` {.r}
table(ces$abortion)
```

    ## 
    ##   No  Yes 
    ## 1818  413

Let's also check the variable type of `abortion`.

``` {.r}
class(ces$abortion)
```

    ## [1] "factor"

It is a factor. Now, let's use `mutate()` to generate a new variable
`against_abortion`.

``` {.r}
ces_new <- ces %>%
  mutate(against_abortion = if_else(abortion == "No", 1, 0))
```

The `if_else()` function is very straightforward -- it basically says:
*please assign the value of 1 to those who answered "Yes" and 0 to those
who answered the opposite.*

Now let's check if we have done this properly. First, use `table()`
again.

``` {.r}
table(ces_new$against_abortion)
```

    ## 
    ##    0    1 
    ##  413 1818

You can also include both variables to create a frequency table -- the
first variable will be the rows and the second will be the columns.

``` {.r}
table(ces_new$abortion, ces_new$against_abortion)
```

    ##      
    ##          0    1
    ##   No     0 1818
    ##   Yes  413    0

All observations are placed properly (i.e., no one is mis-classified in
the new variable). We can continue.

> *Question: Can you create a new binary variable such that it assigns
> the value of 1 (and 0 otherwise) to those living in the urban area?*
:::
:::

::: {#logit-regression-with-no-predictor .section .level2}
Logit Regression with No Predictor
----------------------------------

Let's start with a simple model with no predictor.

You should recall the logit regression transforms a **probability** into
**log odds** (or logged odds or log of odds, they are interchangeable):
[\\\[\\text{logit}(p)=\\log\\left(\\frac{p}{1-p}\\right).\\\]]{.math
.display}

In our case, [\\(p\\)]{.math .inline} is the probability of respondents
saying "No" to abortion. If we only include the intercept (i.e., no
predictor), then our model looks like this:
[\\\[\\log\\left(\\frac{p}{1-p}\\right)=\\alpha.\\\]]{.math .display}
The derived estimate will be the estimated log odds of [\\(p\\)]{.math
.inline}, which is the log odds of the probability that people are
against abortion.

Let's go ahead and fit the model. You can use the `summary()` function
to see the canonical regression output. Note that we have to use the new
data frame `ces_new`.

``` {.r}
mod_intercept <- glm(against_abortion ~ 1, data = ces_new, family = binomial)
summary(mod_intercept)
```

    ## 
    ## Call:
    ## glm(formula = against_abortion ~ 1, family = binomial, data = ces_new)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8367   0.6399   0.6399   0.6399   0.6399  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  1.48204    0.05451   27.19   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2137.6  on 2230  degrees of freedom
    ## Residual deviance: 2137.6  on 2230  degrees of freedom
    ## AIC: 2139.6
    ## 
    ## Number of Fisher Scoring iterations: 4

> *Question: Use `?glm` to see more information -- what is the default
> for the option `family`?*

Three notable things jump out:

First, the estimated intercept [\\(\\hat{\\alpha}=1.482045\\)]{.math
.inline} is the log odds of [\\(p\\)]{.math .inline} (the probability of
a respondent opposing abortion). Let's use `coef()` to extract the
coefficient.

``` {.r}
coefs <- coef(mod_intercept) # extracting log odds
coefs
```

    ## (Intercept) 
    ##    1.482045

Next, Exponentiate the log odds will give us the odds of [\\(p\\)]{.math
.inline}, which is
[\\(\\frac{p}{1-p}=e\^\\hat{\\alpha}=4.401937\\)]{.math .inline}.

``` {.r}
exp(coefs) # calculating odds
```

    ## (Intercept) 
    ##    4.401937

Third, we can take an extra step to derive the probability or the
proportion of people objecting abortion: given that
[\\(\\frac{p}{1-p}=4.401937\\)]{.math .inline}, we know
[\\(p=\\frac{4.401937}{1+4.401937}=0.1851188\\)]{.math .inline}. You can
use R to do the calculation for you.

``` {.r}
exp(coefs)/(1+exp(coefs)) # calculating p
```

    ## (Intercept) 
    ##   0.8148812

If you'd like, we can show the confidence interval of our estimate,
using the `confint()` function.

``` {.r}
coefs_ci <- confint(mod_intercept)
coefs_ci
```

    ##    2.5 %   97.5 % 
    ## 1.376378 1.590112

So these are the upper and lower bounds of the estimated log odds.

> *Question: How do you derive the upper and lower bounds of estimated
> odds and probability?*
:::

::: {#logit-regression-with-one-continuous-predictor .section .level2}
Logit Regression with One Continuous Predictor
----------------------------------------------

Now let's add a continuous explanatory variable to the model. There is
no variable like this, so let's create a new variable `religion`, using
`importance`. Let's use `recode()` inside `mutate()`.

``` {.r}
ces_new <- ces_new %>% # update "ces_new" to include the new variable
  mutate(religion = recode(importance,
                          "very" = 4,
                          "somewhat" = 3,
                          "notvery" = 2,
                          "not" = 1))
table(ces_new$importance, ces_new$religion)
```

    ##           
    ##              1   2   3   4
    ##   not      607   0   0   0
    ##   notvery    0 315   0   0
    ##   somewhat   0   0 714   0
    ##   very       0   0   0 595

With the predictor included, the log odds of someone saying no to
abortion is then [\\\[\\log\\left(\\frac{p}{1-p}\\right)=\\alpha +
\\beta(\\text{Religion}).\\\]]{.math .display} Let's run the analysis,
using the new data frame.

``` {.r}
mod_religion <- glm(against_abortion ~ religion, data = ces_new, family = binomial)
summary(mod_religion)
```

    ## 
    ## Call:
    ## glm(formula = against_abortion ~ religion, family = binomial, 
    ##     data = ces_new)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.7820   0.2054   0.3653   0.6349   1.0431  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  5.02323    0.25893   19.40   <2e-16 ***
    ## religion    -1.17470    0.07506  -15.65   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 2137.6  on 2230  degrees of freedom
    ## Residual deviance: 1761.9  on 2229  degrees of freedom
    ## AIC: 1765.9
    ## 
    ## Number of Fisher Scoring iterations: 5

The estimated coefficient for *Religion* is 1.17470 and statistically
significant. How do we interpret the results?

::: {#use-hatbeta-directly .section .level3}
### Use [\\(\\hat{\\beta}\\)]{.math .inline} Directly

First, [\\(\\hat{\\beta}=1.17470\\)]{.math .inline} is how the
(expected) log-odds will change when we increase religion's degree of
importance by 1. You can use the `confint()` function to extract the
upper and lower bounds of the confidence intervals for
[\\(\\hat{\\beta}\\)]{.math .inline}.

``` {.r}
coef(mod_religion) # extract coefficients
```

    ## (Intercept)    religion 
    ##    5.023228   -1.174700
:::

::: {#exponetiate-hatbeta-to-get-odds-ratio-or .section .level3}
### Exponetiate [\\(\\hat{\\beta}\\)]{.math .inline} to Get Odds Ratio (OR)

Second, alternatively, you may have heard that [\\(e\^\\beta\\)]{.math
.inline} will give us the **odds-ratio** (OR). OR is notoriously
challenging because it requires some understandings of how logarithm
works -- but the math is not that hard.

-   First, remember that we define the model as
    [\\(\\log\\left(\\frac{p}{1-p}\\right)=\\alpha +
    \\beta(\\text{Religion}).\\)]{.math .inline}
-   Again, [\\(\\beta\\)]{.math .inline} tells us how the log of odds
    will change when we increase the value of [\\(X\\)]{.math .inline}
    by 1. Say we move [\\(X\\)]{.math .inline} from 0 to 1, we will have
    two logs of odds. Let's call them
    [\\(\\log(\\text{Odds}\_0)\\)]{.math .inline} and
    [\\(\\log(\\text{Odds}\_1)\\)]{.math .inline}.
-   The thing is -- by the law of logarithms -- **the difference between
    two logs of odds** is the same as **the log of these two odds
    dividing up of each other**. That is:
    [\\\[\\log(\\text{Odds}\_1)-\\log(\\text{Odds}\_0)=\\log\\left(\\frac{\\text{Odds}\_1}{\\text{Odds}\_0}\\right)\\\]]{.math
    .display}.
-   OR is the ratio of two log odds, and the right hand side is the log
    of an OR.

``` {.r}
exp(coef(mod_religion)) # exponentiate all coefficients
```

    ## (Intercept)    religion 
    ## 151.9008210   0.3089115
:::

::: {#the-divide-by-4-rule .section .level3}
### The *Divide-by-4* Rule

Third, use the *divide-by-4* rule, we can derive the largest possible
change in the probability of opposing abortion.

``` {.r}
beta_hat <- coef(mod_religion)[2] # extract the "religion" coefficient 
beta_hat/4 
```

    ##   religion 
    ## -0.2936751
:::

::: {#use-predict-function-recommended .section .level3}
### Use `predict()` Function (Recommended)

Usually, I do not recommend too many calculations.

The most intuitive way to me, is to **use the `predict()` function**.
Plug the model into `predict()` to compute the predicted log-odds for
each observation (given each respondent's report level of religious
importance).

``` {.r}
predict(mod_religion)
```

Let's create a new data frame to include `religion` so you will see it
better.

``` {.r}
fitted_religion <- data.frame(religion = ces_new$religion, # use "religion" in "ces_new"
                              fitted_log_odds = predict(mod_religion))
head(fitted_religion, 10)
```

    ##    religion fitted_log_odds
    ## 1         3       1.4991264
    ## 2         1       3.8485273
    ## 3         3       1.4991264
    ## 4         4       0.3244259
    ## 5         1       3.8485273
    ## 6         1       3.8485273
    ## 7         4       0.3244259
    ## 8         2       2.6738269
    ## 9         4       0.3244259
    ## 10        4       0.3244259

You will see that observations who have the same level of importance
towards religion have the same predicted log-odds. We can clean the data
frame a bit. Also, use `exp()` to exponentiate log odds to return the
odds.

``` {.r}
fitted_religion <- fitted_religion %>%
  distinct() %>% # remove duplicates
  arrange(religion) # arrange observations by religion
fitted_religion$fitted_odds = exp(fitted_religion$fitted_log_odds)
fitted_religion
```

    ##   religion fitted_log_odds fitted_odds
    ## 2        1       3.8485273   46.923909
    ## 8        2       2.6738269   14.495335
    ## 1        3       1.4991264    4.477775
    ## 4        4       0.3244259    1.383236

Now we have the corresponding predicted odds and log-odds for each value
of `religion`. If we increase the importance of `religion` from 1 to 2:

``` {.r}
### The corresponding change in log-odds is
fitted_religion$fitted_log_odds[fitted_religion$religion == 2] - fitted_religion$fitted_log_odds[fitted_religion$religion == 1]
```

    ## [1] -1.1747

``` {.r}
### The corresponding change in odds is
fitted_religion$fitted_odds[fitted_religion$religion == 2] - fitted_religion$fitted_odds[fitted_religion$religion == 1]
```

    ## [1] -32.42857

They are identical with [\\(\\hat{\\beta}\\)]{.math .inline} and
[\\(e\^{\\hat{\\beta}}\\)]{.math .inline}.

> *Question: Which interpretation do you prefer?*\

> *Question: Redo the entire exercise, but now create a new binary
> variable `urban_num` so that it takes the value of 1 for those living
> in the city (otherwise 0) and carry out another logit regression with
> `urban_num` as the only predictor.*
:::
:::

::: {#fitting-glms-with-weights .section .level2}
Fitting GLMs With Weights
-------------------------

The `survey` package makes this very easy. Th function `svyglm()` is
identical to the conventional `glm()` function; except that `svyglm()`
has an option parameter called design (i.e., the survey object) instead
of the data frame.

Type `?svyglm` for more information about the function. Alternatively,
you can visit the `survey` package's vignette page. The section
**Regression models** provides additional information.

In this section, we will use the updated data frame `ces_new`, which
should have included all the variables we have added so far, to carry
out the following activities.

-   Create a survey object.

-   Use the `svyglm()` function to fit logit regression with different
    predictors.

-   Compare the results.

-   Carry out diagnostics.

::: {#create-a-survey-object .section .level3}
### Create a Survey Object

As before, to include survey weights in the analysis.

``` {.r}
ces_s <- ces_new %>%
  as_survey(ids = id,
            strata = province,
            fpc = population,
            weights = weight)
```
:::

::: {#fit-logit-regression .section .level3}
### Fit Logit Regression

To carry out logit regression with weights included, we need to use the
`svyglm()` function in the `survey` package.

``` {.r}
mod_s_religion <- svyglm(against_abortion ~ religion,
                         design = ces_s,
                         family = binomial)
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` {.r}
summary(mod_s_religion)
```

    ## 
    ## Call:
    ## svyglm(formula = against_abortion ~ religion, design = ces_s, 
    ##     family = binomial)
    ## 
    ## Survey design:
    ## Called via srvyr
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    5.202      0.355   14.65   <2e-16 ***
    ## religion      -1.223      0.103  -11.88   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1.210905)
    ## 
    ## Number of Fisher Scoring iterations: 6
:::

::: {#model-comparison .section .level3}
### Model Comparison

Let's conduct another logit regression but this time only includes the
intercept.

``` {.r}
mod_s_intercept <- svyglm(against_abortion ~ 1,
                          design = ces_s,
                          family = binomial)
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` {.r}
summary(mod_s_intercept)
```

    ## 
    ## Call:
    ## svyglm(formula = against_abortion ~ 1, design = ces_s, family = binomial)
    ## 
    ## Survey design:
    ## Called via srvyr
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  1.48297    0.06534    22.7   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1.000448)
    ## 
    ## Number of Fisher Scoring iterations: 4

Use `anova()` to compare whether or not including `religion` helps with
explaining more variance in the outcome variable (or: does including
`religion` statistically improve the fit)?

``` {.r}
anova(mod_s_intercept, mod_s_religion, test="Chi")
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

    ## Working (Rao-Scott) LRT for religion
    ##  in svyglm(formula = against_abortion ~ religion, design = ces_s, 
    ##     family = binomial)
    ## Working 2logLR =  224.806 p= < 2.22e-16 
    ## df=1

> *Question: What is the `test` option here for? Use `?anova` to see
> more information.*\
:::
:::

::: {#extra-model-diagnostics .section .level1}
Extra: Model Diagnostics
========================

**Note: This section is largely based on Section 12 "Diagnostics" in
"Tutorial: logistic regression as a GLM" by Dr Marju Kaps. For more
discussion, I'd encourage you to review it.**
:::
:::
