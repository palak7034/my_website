---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: 
draft: false
image: Excercise_pic.jpg
keywords: ""
slug: magna
title: Youth Risk Behavior Surveillance
---

Every two years, the Centers for Disease Control and Prevention conduct the [Youth Risk Behavior Surveillance System (YRBSS)](https://www.cdc.gov/healthyyouth/data/yrbs/index.htm) survey, where it takes data from high schoolers (9th through 12th grade), to analyze health patterns. You will work with a selected group of variables from a random sample of observations during one of the years the YRBSS was conducted.

## Load the data

This data is part of the `openintro` textbook and we can load and inspect it. There are observations on 13 different variables, some categorical and some numerical. The meaning of each variable can be found by bringing up the help file:


```{r}
data(yrbss)
glimpse(yrbss)
```

Before you carry on with your analysis, it's is always a good idea to check with `skimr::skim()` to get a feel for missing values, summary statistics of numerical variables, and a very rough histogram.

## Exploratory Data Analysis

You will first start with analyzing the `weight` of participants in kilograms. Using visualization and summary statistics, describe the distribution of weights. How many observations are we missing weights from?

```{r, eda_on_weight}
skim(yrbss)
yrbss %>% 
  ggplot(aes(x=weight))+
geom_density()
yrbss %>% 
  summarise( min = min(weight, na.rm = TRUE), max = max(weight, na.rm = TRUE), mean = mean(weight, na.rm = TRUE),median = median(weight, na.rm = TRUE), SD = sd(weight, na.rm = TRUE))
```
> We are missing *1004* observations in weigths.

Next, consider the possible relationship between a high schooler’s weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

Let’s create a new variable in the dataframe `yrbss`, called `physical_3plus` , which will be `yes` if they are physically active for at least 3 days a week, and `no` otherwise. You may also want to calculate the number and % of those who are and are not active for more than 3 days. RUse the `count()` function and see if you get the same results as `group_by()... summarise()`

  
```{r, mutate_and_count}
yrbss<-yrbss %>% 
  #filter(weight != "NA" & physically_active_7d != "NA") %>% 
  mutate(physical_3plus= ifelse(physically_active_7d >=3,"Yes","No"))

summarised_yrbss<-yrbss %>% 
  summarise(count=n(), active= count(physical_3plus== "Yes"), proportion_active=active/count,
        not_active= count(physical_3plus== "No"), proportion_inactive=not_active/count)
  
summarised_yrbss

yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(count=n())

proportion_yes<-as.numeric(summarised_yrbss["proportion_active"])
proportion_no<-as.numeric(summarised_yrbss["proportion_inactive"])

proportion_yes
proportion_no
```
Can you provide a 95% confidence interval for the population proportion of high schools that are *NOT* active 3 or more days per week?
```{r}
formula_ci_not <- yrbss %>% 
  # choose the interval 2011-present
  filter(physical_3plus =="No")%>% 
 
  # calculate summary statistics  
  summarise(count=n(),
            SE = ( proportion_yes*(1-proportion_yes)/count)**0.5,
  t_critical = qt(0.975, count-1),
  margin_error = t_critical * SE,
  lower = proportion_yes - margin_error,
  upper = proportion_yes + margin_error)
  # calculate mean, SD, count, SE, lower/upper 95% CI
  # what dplyr verb will you use? 

#print out formula_CI
formula_ci_not

```

  
Make a boxplot of `physical_3plus` vs. `weight`. 

```{r, boxplot}
yrbss %>% 
  ggplot(aes(x=weight, y= physical_3plus))+
  geom_boxplot()
```
Is there a relationship between these two variables? What did you expect and why?
>Average weight in the high schoolers who are physically active at least 3 days a week is greater than the rest.
>We expected that the high schoolers who are physically active at least 3 days a week to be less than the rest would be more heathy and hence will put on less weight.

## Confidence Interval

Boxplots show how the medians of the two distributions compare, but we can also compare the means of the distributions using either a confidence interval or a hypothesis test. Note that when we calculate the mean, SD, etc. weight in these groups using the mean function, we must ignore any missing values by setting the `na.rm = TRUE`.


```{r, ci_using_formulas}
yrbss %>% 
  group_by(physical_3plus) %>% 
  summarise(min = min(weight,na.rm = TRUE), max = max(weight,na.rm = TRUE), SD = sd(weight,na.rm = TRUE), mean =mean(weight,na.rm = TRUE),  median = median(weight,na.rm = TRUE))
```

There is an observed difference of about 1.77kg (68.44 - 66.67), and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant. Let us also conduct a hypothesis test.

## Hypothesis test with formula

Write the null and alternative hypotheses for testing whether mean weights are different for those who exercise at least 3 times a week and those who don’t.

```{r}
t.test(weight ~ physical_3plus, data = yrbss)
```

>H0= Difference between the Avg. weights of the people who excercise 3 days a week and who do not is 0 
>Alt. hypothesis- Difference between the Avg. weights of the people who excercise 3 days a week and who do not is not zero

## Hypothesis test with `infer`


Next, we will introduce a new function, `hypothesize`, that falls into the infer workflow. You will use this method for conducting hypothesis tests.

But first, we need to initialize the test, which we will save as `obs_diff`.

```{r, calc_obs_difference}
obs_diff <- yrbss %>%
  specify(weight ~ physical_3plus) %>%
  calculate(stat = "diff in means", order = c("Yes", "No"))

```



Notice how you can use the functions specify and calculate again like you did for calculating confidence intervals. Here, though, the statistic you are searching for is the difference in means, with the order being yes - no != 0.

After you have initialized the test, you need to simulate the test on the null distribution, which we will save as null.


```{r, hypothesis_testing_using_infer_package}

null_dist <- yrbss %>%
  # specify variables
  specify(weight ~ physical_3plus) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("Yes", "No"))

```


Here, `hypothesize` is used to set the null hypothesis as a test for independence, i.e., that there is no difference between the two population means. In one sample cases, the null argument can be set to *point* to test a hypothesis relative to a point estimate.

Also, note that the `type` argument within generate is set to permute, which is the argument when generating a null distribution for a hypothesis test.

We can visualize this null distribution with the following code:

```{r}
ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

```


Now that the test is initialized and the null distribution formed, we can visualise to see how many of these null permutations have a difference of at least `obs_stat` of `r obs_diff %>% pull() %>% round(2)`

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```


This the standard workflow for performing hypothesis tests.