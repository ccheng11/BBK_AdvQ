Tutorial: Analyzing Survey Data (Part 2)
================

Chao-Yo Cheng  
6 February 2021

# Before You Start

Revisit **Tutorial: logistic regression as a GLM** prepared by Dr Marju
Kaps – let’s have a quick discussion on this.

**If you’d like, treat the original tutorial as extra (i.e.,
project-only) materials**.

Having said that, Section 7 in the original tutorial is useful if you
need a quick refresher on the log function. Section 12 provides some
useful tools for model diagnostics etc.

# 1 Introduction

By the end of this tutorial, you will know how to

-   Review the main concepts of logit regression and how to interpret
    the results.
-   Use logit regression to analyze survey data.
-   Perform model comparison on survey data.

We are building on previous tutorials on logit regression and survey
weights this week. You can continue writing your code in the same file
you used last week. We will start by bringing in the same packages
again.

``` r
library(survey)
library(srvyr)
library(dplyr)
library(ggplot2)
library(purrr)
```

# 2 Data

We will continue to work with the data set from **the 2011 Canadian
National Election Study**, which includes the following variables.

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
-   `urban` – place of residence, a factor with levels `rural`, `urban`.

In this tutorial, we will consider some of the variables that may be
statistically associated with people’s attitudes towards abortion. The
outcome of variable of interest is therefore `abortion`.

> *Question: Any other factors we can or should consider?*

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

For the following analysis, we will create a binary variable to assign
the value of 1 to people against abortion.

``` r
ces_new <- ces %>%
  mutate(against_abortion = if_else(abortion == "Yes", 1, 0))
```

The `if_else()` function is very convenient – it basically says: please
assign the value of 1 to those who answered “Yes” and 0 to those who
answered the opposite.

Now let’s check if we have done this properly. First, use `table()`.

``` r
table(ces_new$against_abortion)
```

    ## 
    ##    0    1 
    ## 1818  413

All observations are placed properly (i.e., no respondent is
mis-classified in the new variable).

# 3 Use `glm()` to Run Logit Regression

*Note: The main objective of this section is to make sure everyone is on
the same page with respect to logit and GLM before we move on to include
weights in the analysis.*

In this section, we will use the workhorse function `glm()` to estimate
the correlates of *people’s attitudes against abortion*.

-   Create a new dependent variable `against_abortion`, using the
    original variable `abortion` in the dataset.

-   Fit the logit model with no predictor. Interpret the findings.

-   Fit the logit model with one predictor. Interpret the findings using
    odds-ratio with the help of `predict()`.

> *Question: Can you create a new binary variable such that it assigns
> the value of 1 (and 0 otherwise) to those living in the urban area?*

## 3.1 Logit Regression with No Predictor

We will start with a simple model with no predictor. This exercise aims
to make you familiar with the differences between probability, odds, and
log-odds.

Quickly recall the logit link function transforms a **probability** into
**log odds**:

![\\text{logit}(p)=\\log\\left(\\frac{p}{1-p}\\right).](https://latex.codecogs.com/png.latex?%5Ctext%7Blogit%7D%28p%29%3D%5Clog%5Cleft%28%5Cfrac%7Bp%7D%7B1-p%7D%5Cright%29. "\text{logit}(p)=\log\left(\frac{p}{1-p}\right).")

In this case, ![p](https://latex.codecogs.com/png.latex?p "p") is the
**probability** of respondents against abortion.\*\*

If we only include the intercept (i.e., no predictor), then our model is
like this:

![\\log\\left(\\frac{p}{1-p}\\right)=\\alpha.](https://latex.codecogs.com/png.latex?%5Clog%5Cleft%28%5Cfrac%7Bp%7D%7B1-p%7D%5Cright%29%3D%5Calpha. "\log\left(\frac{p}{1-p}\right)=\alpha.")

The estimated intercept will be *the log-odds* of people against
abortion.

Let’s go ahead and fit the model. Use the `summary()` function to see
the output. Note that we have to use the new data frame `ces_new` (the
original `ces` does not include the variable we created).

``` r
mod_intercept <- glm(against_abortion ~ 1,
                     data = ces_new,
                     family = binomial)
summary(mod_intercept)
```

    ## 
    ## Call:
    ## glm(formula = against_abortion ~ 1, family = binomial, data = ces_new)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6399  -0.6399  -0.6399  -0.6399   1.8367  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.48204    0.05451  -27.19   <2e-16 ***
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

> *Question: Use `?glm` to see more information – what is the default
> for the option `family`?*

### 3.1.1 Explore the Point Estimate

The estimated intercept is the *log-odds* (of people against abortion).
Let’s use `coef()` to extract the coefficient.

``` r
coefs <- coef(mod_intercept) # extracting log odds
coefs
```

    ## (Intercept) 
    ##   -1.482045

Next, we can take the exponent of the log odds to get the *odds* of
people against abortion (i.e., use `exp()`). **Rule of thumb: use
`exp()` to turn log-odds into odds.**

``` r
exp(coefs) # calculating odds
```

    ## (Intercept) 
    ##   0.2271727

Third, we can compute the *probability* of people against abortion. From
the previous step, since the odds
![\\frac{p}{1-p}=0.2271727](https://latex.codecogs.com/png.latex?%5Cfrac%7Bp%7D%7B1-p%7D%3D0.2271727 "\frac{p}{1-p}=0.2271727"),
we know
![p=\\frac{0.2271727}{1+0.2271727}=0.1851188](https://latex.codecogs.com/png.latex?p%3D%5Cfrac%7B0.2271727%7D%7B1%2B0.2271727%7D%3D0.1851188 "p=\frac{0.2271727}{1+0.2271727}=0.1851188").

``` r
exp(coefs)/(1+exp(coefs)) # calculating p
```

    ## (Intercept) 
    ##   0.1851188

### 3.1.2 Explore the Confidence Intervals of the Point Estimate

We can show the **confidence interval** of the estimated log-odds, using
the `confint()` function. These are the upper and lower bounds of the
95% confidence intervals.

``` r
coefs_ci <- confint(mod_intercept)
coefs_ci
```

    ##     2.5 %    97.5 % 
    ## -1.590112 -1.376378

> *Question: How do you derive the upper and lower bounds of estimated
> odds and probability? Hint: Use the upper and lower bounds of log odds
> to calculate the upper and lower bounds of estimated odds and
> probabilities.*

``` r
coefs_ci <- confint(mod_intercept)
coefs_ci # CIs of log-odds
```

    ##     2.5 %    97.5 % 
    ## -1.590112 -1.376378

``` r
exp(coefs_ci) # CIs of odds
```

    ##     2.5 %    97.5 % 
    ## 0.2039028 0.2524914

``` r
exp(coefs_ci)/(1+exp(coefs_ci)) # CIs of probability
```

    ##     2.5 %    97.5 % 
    ## 0.1693682 0.2015913

## 3.2 Logit Regression with One Predictor

Let’s add a predictor to the logit regression model.

There is no variable like this, so let’s create a new variable
`religion`, using `importance`. Let’s use `recode()` inside `mutate()`.
Here we use higher numbers to refer to greater importance.

``` r
ces_new <- ces_new %>% # update "ces_new" to include all new variables
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

> *Question: Which of the two versions of the importance of religion
> variable would you use in your analysis? Why? Try to think of pros and
> cons for each.*

With the predictor included, the log-odds of a respondent against
abortion is

![\\log\\left(\\frac{p}{1-p}\\right)=\\alpha + \\beta(\\text{religion}).](https://latex.codecogs.com/png.latex?%5Clog%5Cleft%28%5Cfrac%7Bp%7D%7B1-p%7D%5Cright%29%3D%5Calpha%20%2B%20%5Cbeta%28%5Ctext%7Breligion%7D%29. "\log\left(\frac{p}{1-p}\right)=\alpha + \beta(\text{religion}).")

Let’s run the analysis.

``` r
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
    ## -1.0431  -0.6349  -0.3653  -0.2054   2.7820  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.02323    0.25893  -19.40   <2e-16 ***
    ## religion     1.17470    0.07506   15.65   <2e-16 ***
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

The estimated coefficient for *religion* is 1.17470 and statistically
significant. How do we interpret the results?

### 3.2.1 Odds Ratio (OR)

First, again, the coefficient is **log-odds**. Unlike the model with no
predictor, take the exponent of the coefficient will give us the
**odds-ratio** (OR). OR is notoriously confusing, but here is the basic
intuition with minimum math involved.

First, recall that we define the model as

![\\log\\left(\\frac{p}{1-p}\\right)=\\alpha + \\beta(\\text{religion}).](https://latex.codecogs.com/png.latex?%5Clog%5Cleft%28%5Cfrac%7Bp%7D%7B1-p%7D%5Cright%29%3D%5Calpha%20%2B%20%5Cbeta%28%5Ctext%7Breligion%7D%29. "\log\left(\frac{p}{1-p}\right)=\alpha + \beta(\text{religion}).")

Keep in mind that the outcome variable is the log-odds of people against
abortion. So we can rewrite this as

![\\log(\\text{Odds})=\\alpha + \\beta(\\text{religion}).](https://latex.codecogs.com/png.latex?%5Clog%28%5Ctext%7BOdds%7D%29%3D%5Calpha%20%2B%20%5Cbeta%28%5Ctext%7Breligion%7D%29. "\log(\text{Odds})=\alpha + \beta(\text{religion}).")

Next, ![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\beta")
tells us how the *log-odds* will change when we increase the importance
of `religion` by one level.

![\\beta=\\log(\\text{Odds when Religion}=2)-\\log(\\text{Odds when Religion}=1).](https://latex.codecogs.com/png.latex?%5Cbeta%3D%5Clog%28%5Ctext%7BOdds%20when%20Religion%7D%3D2%29-%5Clog%28%5Ctext%7BOdds%20when%20Religion%7D%3D1%29. "\beta=\log(\text{Odds when Religion}=2)-\log(\text{Odds when Religion}=1).")

By the law of logarithms, the **the difference between two logs** (the
left hand side below) can be rewritten as **the log of them dividing up
each other** (the right hand side below):

![\\log(\\text{Odds when Religion}=2)-\\log(\\text{Odds when Religion}=1)=\\log\\left(\\frac{\\text{Odds when Religion}=2}{\\text{Odds when Religion}=1}\\right).](https://latex.codecogs.com/png.latex?%5Clog%28%5Ctext%7BOdds%20when%20Religion%7D%3D2%29-%5Clog%28%5Ctext%7BOdds%20when%20Religion%7D%3D1%29%3D%5Clog%5Cleft%28%5Cfrac%7B%5Ctext%7BOdds%20when%20Religion%7D%3D2%7D%7B%5Ctext%7BOdds%20when%20Religion%7D%3D1%7D%5Cright%29. "\log(\text{Odds when Religion}=2)-\log(\text{Odds when Religion}=1)=\log\left(\frac{\text{Odds when Religion}=2}{\text{Odds when Religion}=1}\right).")

Remove the log from
![\\log\\left(\\frac{\\text{Odds when Religion}=2}{\\text{Odds when Religion}=1}\\right)](https://latex.codecogs.com/png.latex?%5Clog%5Cleft%28%5Cfrac%7B%5Ctext%7BOdds%20when%20Religion%7D%3D2%7D%7B%5Ctext%7BOdds%20when%20Religion%7D%3D1%7D%5Cright%29 "\log\left(\frac{\text{Odds when Religion}=2}{\text{Odds when Religion}=1}\right)")
by taking the exponent of it; in doing so, we have

![\\left(\\frac{\\text{Odds when Religion}=2}{\\text{Odds when Religion}=1}\\right).](https://latex.codecogs.com/png.latex?%5Cleft%28%5Cfrac%7B%5Ctext%7BOdds%20when%20Religion%7D%3D2%7D%7B%5Ctext%7BOdds%20when%20Religion%7D%3D1%7D%5Cright%29. "\left(\frac{\text{Odds when Religion}=2}{\text{Odds when Religion}=1}\right).")

This is **the odds ratio when we increase the importance of religion by
one unit.**

``` r
exp(coef(mod_religion))
```

    ## (Intercept)    religion 
    ## 0.006583243 3.237173195

We can use `confint()` to get the confidence interval of the odds-ratio
to see if it is stays above 1.

``` r
coefs_ci <- confint(mod_religion)
coefs_ci # CIs of log-odds
```

    ##                 2.5 %    97.5 %
    ## (Intercept) -5.548352 -4.532450
    ## religion     1.031402  1.325895

``` r
exp(coefs_ci) # CIs of odds-ratio
```

    ##                  2.5 %    97.5 %
    ## (Intercept) 0.00389387 0.0107543
    ## religion    2.80499482 3.7655551

In a nutshell, odds-ratio (OR) is **a ratio of two different odds** when
we change the level of `religion` by one unit. And OR can only be one of
the three conditions below, each of which has a different substantive
interpretation. Since we find
![OR\_{religion}>1](https://latex.codecogs.com/png.latex?OR_%7Breligion%7D%3E1 "OR_{religion}>1")
even after we consider its confidence interval, **more religious people
are going be more likely to be against abortion.**

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
It means
</th>
<th style="text-align:left;">
So when we increase importance of religion
</th>
<th style="text-align:left;">
Put it simply
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
OR=1
</td>
<td style="text-align:left;">
(Odds when Religion=2) = (Odds when Religion=1)
</td>
<td style="text-align:left;">
the odds/chance of resisting abortion is not affected
</td>
<td style="text-align:left;">
Not clear how religious people react to abortion
</td>
</tr>
<tr>
<td style="text-align:left;">
OR>1
</td>
<td style="text-align:left;">
(Odds when Religion=2) \> (Odds when Religion=1)
</td>
<td style="text-align:left;">
the odds/chance of resisting the abortion is higher
</td>
<td style="text-align:left;">
Religious people are more likely to be against abortion
</td>
</tr>
<tr>
<td style="text-align:left;">
OR\<1
</td>
<td style="text-align:left;">
(Odds when Religion=2) \< (Odds when Religion=1)
</td>
<td style="text-align:left;">
the odds/chance of resisting abortion is lower
</td>
<td style="text-align:left;">
Religious people are less likely to be against abortion
</td>
</tr>
</tbody>
</table>

To sum up our discussion, when you conduct a logit regression, do the
following:

-   Step 1: Write down a table like the one above to think through your
    interpretations.

-   Step 2: Run the logit regression (use `glm()`); see if your
    predictor(s) have any statistically significant coefficients.

-   Step 3: Take the exponent of your coefficients (use `exp()`). the
    odds-ratio when we change the corresponding predictor by one unit.

-   Step 4: Explain the odds-ratio (OR) with the table you made.

    -   If ![OR=1](https://latex.codecogs.com/png.latex?OR%3D1 "OR=1"),
        the predictor is not associated with the (odds of the) outcome
        (this is going to be rare in practice).
    -   If ![OR>1](https://latex.codecogs.com/png.latex?OR%3E1 "OR>1"),
        the predictor is positively associated with the (odds of the)
        outcome.
    -   If ![OR\<1](https://latex.codecogs.com/png.latex?OR%3C1 "OR<1"),
        the predictor is negatively associated with the (odds of the)
        outcome.

-   Step 5: Get the confidence interval of the odds ratio to make sure
    it stays in one of the three situations above.

> *Question: Use another predictor in the dataset to go through the
> entire analysis again to make sure everything is clear.*

### 3.2.2 Use `predict()` Function (Recommended)

The most intuitive way to me is perhaps **use the `predict()`
function**. Plug the logit model into `predict()` to compute the
predicted **log-odds** for each observation (given each respondent’s
reported level of religious importance). Including `type="response"`
will return the predicted **probabilities**.

``` r
fit_log_odds <- predict(mod_religion) # for predicted log-odds
fit_prob <- predict(mod_religion, type="response") # for predicted probabilities
```

Let’s create a new data frame to include `religion` so you will see it
better. We will also include the original `importance` variable.

``` r
fit_religion <- data.frame(religion = ces_new$religion, # use "religion" in "ces_new"
                           importance = ces_new$importance)
fit_religion <- fit_religion %>%
  mutate(prob = predict(mod_religion, type="response"),
         log_odds = predict(mod_religion)) %>%
  mutate(odds = exp(log_odds)) # convert log-odds into odds
head(fit_religion, 5)
```

    ##   religion importance       prob   log_odds      odds
    ## 1        3   somewhat 0.18255586 -1.4991264 0.2233252
    ## 2        1        not 0.02086641 -3.8485273 0.0213111
    ## 3        3   somewhat 0.18255586 -1.4991264 0.2233252
    ## 4        4       very 0.41959750 -0.3244259 0.7229423
    ## 5        1        not 0.02086641 -3.8485273 0.0213111

You will see that observations with the identical level of importance
have the same predicted values. Let’s clean the data frame a bit using
`distinct()` and `arrange()`.

``` r
fit_religion <- fit_religion %>%
  distinct() %>% # remove duplicates
  arrange(religion) # arrange observations by religion
fit_religion
```

    ##   religion importance       prob   log_odds       odds
    ## 1        1        not 0.02086641 -3.8485273 0.02131110
    ## 2        2    notvery 0.06453555 -2.6738269 0.06898771
    ## 3        3   somewhat 0.18255586 -1.4991264 0.22332518
    ## 4        4       very 0.41959750 -0.3244259 0.72294227

Now we have the corresponding predicted odds, log-odds, and probability
for each level of `religion`. If we increase the importance of
`religion` from 1 to 2, the corresponding change in log-odds and
odds-ratio will be

``` r
fit_religion$log_odds[fit_religion$religion == 2] - fit_religion$log_odds[fit_religion$religion == 1]
```

    ##      8 
    ## 1.1747

``` r
fit_religion$odds[fit_religion$religion == 2]/fit_religion$odds[fit_religion$religion == 1]
```

    ##        8 
    ## 3.237173

The corresponding change in probability is

``` r
fit_religion$prob[fit_religion$religion == 2] - fit_religion$prob[fit_religion$religion == 1]
```

    ##          8 
    ## 0.04366914

They are identical with the results we get before. All in all, we know
that a more religious person is more likely to go against abortion.

To use `predict()` when the model includes more than one predictor, see
*Section 4.4*.

# 4 Fitting GLMs With Weights

The `survey` package makes this very easy. The function `svyglm()` is
identical to the conventional `glm()` function except that `svyglm()`
use `design` (i.e., the survey object) instead of `data`.

Type `?svyglm` for more information about the function. Alternatively,
you can visit the `survey` package’s vignette page. The section
**Regression models** provides additional information.

In this section, we will use the updated data frame `ces_new`, which
should have included all the variables we have added so far, to carry
out the following activities.

-   Create a survey object.

-   Use the `svyglm()` function to fit logit regression with two
    predictors.

Before we start, let’s check we have all the variables we need.

``` r
ces_new <- ces %>%
  mutate(against_abortion = if_else(abortion == "Yes", 1, 0),
         urban = if_else(urban == "urban", 1, 0),
         religion = recode(importance,
                           "very" = 4,
                           "somewhat" = 3,
                           "notvery" = 2,
                           "not" = 1))
```

## 4.1 Create a Survey Object

As before, we can use `as_survey()` to create the `survey` object so we
can use the useful functions in the packages.

``` r
ces_s <- ces_new %>%
  as_survey(ids = id,
            strata = province,
            fpc = population,
            weights = weight)
ces_s
```

    ## Stratified Independent Sampling design
    ## Called via srvyr
    ## Sampling variables:
    ##  - ids: id
    ##  - strata: province
    ##  - fpc: population
    ##  - weights: weight
    ## Data variables: id (int), province (fct), population (int), weight (dbl),
    ##   gender (fct), abortion (fct), importance (fct), education (fct), urban (dbl),
    ##   against_abortion (dbl), religion (dbl)

## 4.2 Fit Logit Regression

To carry out logit regression with weights included, we need to use
`svyglm()` in the `survey` package. Let’s include `religion` again.
Again, here we have to use `ces_s` rather than `ces_new`. You can ignore
the warning message.

``` r
mod_s_religion <- svyglm(against_abortion ~ religion,
                         design = ces_s, # be sure to use the survey object
                         family = binomial)
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` r
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
    ## (Intercept)   -5.202      0.355  -14.65   <2e-16 ***
    ## religion       1.223      0.103   11.88   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1.210905)
    ## 
    ## Number of Fisher Scoring iterations: 6

Let’s take a look at the odds-ratio by taking the exponent of the
`religion` coefficient.

``` r
exp(coef(mod_s_religion))
```

    ## (Intercept)    religion 
    ## 0.005506094 3.397762968

## 4.3 Extra: Model Comparison and Diagnostics

**Note: Please revisit Section 12 “Diagnostics” in “Tutorial: logistic
regression as a GLM” by Dr Marju Kaps to see the functions you can use
for model diagnostics.**

Let’s conduct another logit regression but this time only includes the
intercept.

``` r
mod_s_intercept <- svyglm(against_abortion ~ 1,
                          design = ces_s,
                          family = binomial)
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` r
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
    ## (Intercept) -1.48297    0.06534   -22.7   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1.000448)
    ## 
    ## Number of Fisher Scoring iterations: 4

Use `anova()` to see whether or not including `religion` helps with
explaining more variance in the outcome variable (or: does including
`religion` statistically improve the model fit)?

``` r
anova(mod_s_intercept, mod_s_religion, test="Chi")
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

    ## Working (Rao-Scott) LRT for religion
    ##  in svyglm(formula = against_abortion ~ religion, design = ces_s, 
    ##     family = binomial)
    ## Working 2logLR =  224.806 p= < 2.22e-16 
    ## df=1

> *Question: What is the `test` option here for? Use `?anova` to see
> more information.*  

> *Question: You can also compare the finding we get from `glm()`.*  

## 4.4 Extra: Get Predicted Log Odds and Probability With Multiple Predictors

Say now if we include two predictors in the model.

``` r
mod_s <- svyglm(against_abortion ~ religion + urban,
                design = ces_s, # be sure to use the survey object
                family = binomial)
```

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

``` r
summary(mod_s)
```

    ## 
    ## Call:
    ## svyglm(formula = against_abortion ~ religion + urban, design = ces_s, 
    ##     family = binomial)
    ## 
    ## Survey design:
    ## Called via srvyr
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -4.9319     0.3774 -13.068   <2e-16 ***
    ## religion      1.2246     0.1031  11.873   <2e-16 ***
    ## urban        -0.3591     0.1570  -2.287   0.0223 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1.214632)
    ## 
    ## Number of Fisher Scoring iterations: 6

We can still use the `predict()` function to get the predicted log-odds
and probabilities. Given that the model includes two predictors, we have
to carry out the predictions one by one.

We will start with the `religion` coefficient. This time, rather than
putting the model into `predict()`, let’s specify the exact levels of
`religion`. Since we are interested in learning how the predicted
log-odds and probabilities across different levels, we have to hold
`urban` constant. In this sense, `urban` serves as a **control**
variable for us so we can explore the following: *how will the attitudes
against abortion change with one-unit increase in `religion`, when we
hold `urban` constant?*

Let’s feed the `predict()` function two pieces of information:

-   What are the unique levels of \`religion\`\`? The variable ranges
    from 1 to 4.
-   What is the constant of `urban` we can use? Weighted mean is a good
    choice.

``` r
ces_s %>%
  summarise(urban_w_mean = survey_mean(urban, na.rm=T))
```

    ## # A tibble: 1 × 2
    ##   urban_w_mean urban_w_mean_se
    ##          <dbl>           <dbl>
    ## 1        0.785         0.00952

Put them together into a new data frame. Note the variable names have to
be the same as those included in `svyglm()`.

``` r
data_predict <- data.frame(religion = 1:4,
                           urban = 0.785)
data_predict
```

    ##   religion urban
    ## 1        1 0.785
    ## 2        2 0.785
    ## 3        3 0.785
    ## 4        4 0.785

Now let’s obtain the predicted log-odds, odds, and probabilities when we
vary the importance of `religion` while keeping `urban` at its weighted
mean.

``` r
fit_prob <- predict(mod_s, newdata=data_predict, type="response")
fit_log_odds <- predict(mod_s, newdata=data_predict)
fit_mod_s <- data.frame(religion = 1:4,
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s
```

    ##   religion   fit_prob fit_log_odds   fit_odds
    ## 1        1 0.01817897   -3.9891435 0.01851557
    ## 2        2 0.05927010   -2.7645512 0.06300437
    ## 3        3 0.17654126   -1.5399589 0.21438992
    ## 4        4 0.42180537   -0.3153665 0.72952142

> *Question: Do the calculation similar to “Use predict() Function
> (Recommended). Discuss your observation.*  

> *Question: Redo the same process, but this time, obtain the predicted
> log-odds, odds, and probabilities when we vary the levels of `urban`
> while holding `religion` at its weighted mean.*
