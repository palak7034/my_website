---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: "Is there a pay gap between male and female Executives in OMEGA?"
draft: false
image: genderpaygap.jpeg
keywords: ""
slug: ipsum
title: Omega Group plc- Pay Discrimination
---

At the last board meeting of Omega Group Plc., the headquarters of a large multinational company, the issue was raised that women were being discriminated in the company, in the sense that the salaries were not the same for male and female executives. A quick analysis of a sample of 50 employees (of which 24 men and 26 women) revealed that the average salary for men was about 8,700 higher than for women. This seemed like a considerable difference, so it was decided that a further analysis of the company salaries was warranted. 

You are asked to carry out the analysis. The objective is to find out whether there is indeed a significant difference between the salaries of men and women, and whether the difference is due to discrimination or whether it is based on another, possibly valid, determining factor. 

## Loading the data


```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))
glimpse(omega) # examine the data frame
```

## Relationship Salary - Gender ?

The data frame `omega`  contains the salaries for the sample of 50 executives in the company. Can you conclude that there is a significant difference between the salaries of the male and female executives?

Note that you can perform different types of analyses, and check whether they all lead to the same conclusion 

.	Confidence intervals
.	Hypothesis testing
.	Correlation analysis
.	Regression


Calculate summary statistics on salary by gender. Also, create and print a dataframe where, for each gender, you show the mean, SD, sample size, the t-critical, the SE, the margin of error, and the low/high endpoints of a 95% condifence interval

```{r, confint_single_valiables}
# Summary Statistics of salary by gender
mosaic::favstats (salary ~ gender, data=omega)

# Dataframe with two rows (male-female) and having as columns gender, mean, SD, sample size, 
# the t-critical value, the standard error, the margin of error, 
# and the low/high endpoints of a 95% confidence interval
omega

omega %>% 
  group_by(gender) %>% 
  summarise(min = min(salary,na.rm = TRUE), max = max(salary,na.rm = TRUE), SD = sd(salary,na.rm = TRUE), mean =mean(salary,na.rm = TRUE),  median = median(salary,na.rm = TRUE))


formula_ci <- omega %>% 
  group_by(gender) %>% 
  summarise(mean = mean(salary,  na.rm = TRUE), SD = sd(salary, na.rm = TRUE), count=n(),
  t_critical = qt(0.975, count-1), 
  SE = SD/sqrt(count),
  margin_error = t_critical * SE,
  lower = mean - margin_error,
  upper = mean + margin_error)

formula_ci

t.test(salary ~ gender, data = omega)

```

> What can you conclude from your analysis? A couple of sentences would be enough

>There is an observed difference of about USD 8696 (73239 - 64543) in the mean salaries for female and male employees, and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant.So we can safely conlude that the salaries for male executives is greater than that of female executives

You can also run a hypothesis testing, assuming as a null hypothesis that the mean difference in salaries is zero, or that, on average, men and women make the same amount of money. You should tun your hypothesis testing using `t.test()` and with the simulation method from the `infer` package.

```{r, hypothesis_testing}
# hypothesis testing using t.test() 
t.test(salary ~ gender, data = omega)

# hypothesis testing using infer package
obs_diff <- omega %>%
  specify(salary ~ gender) %>%
  calculate(stat = "diff in means", order = c("male", "female"))

null_dist <- omega %>%
  # specify variables
  specify(salary ~ gender) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("male", "female"))

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```

> What can you conclude from your analysis? A couple of sentences would be enough

>We see that the red line in the graph above is pretty far in the left tail of the distribution.The probability of seeing this difference (p-value) is equal to zero(it may not be exacly equal to zero but its very small(~0.0002) so we see can say it is 0)
so we can conclude that our hypothesis is *False* abd there _is_ a difference between the salaries of female and male employees.

## Relationship Experience - Gender?

At the board meeting, someone raised the issue that there was indeed a substantial difference between male and female salaries, but that this was attributable to other reasons such as differences in experience. A questionnaire send out to the 50 executives in the sample reveals that the average experience of the men is approximately 21 years, whereas the women only have about 7 years experience on average (see table below).

```{r, experience_stats}
# Summary Statistics of salary by gender
favstats (experience ~ gender, data=omega)

omega
formula_ci <- omega %>% 
  group_by(gender) %>% 
  summarise(mean = mean(experience,  na.rm = TRUE), SD = sd(experience, na.rm = TRUE), count=n(),
  t_critical = qt(0.975, count-1), 
  SE = SD/sqrt(count),
  margin_error = t_critical * SE,
  lower = mean - margin_error,
  upper = mean + margin_error)

formula_ci


  
omega %>% 
  group_by(gender) %>% 
  summarise(min = min(experience,na.rm = TRUE), max = max(experience,na.rm = TRUE), SD = sd(experience,na.rm = TRUE), mean =mean(experience,na.rm = TRUE),  median = median(experience,na.rm = TRUE))

t.test(experience ~ gender, data = omega)
```




## Relationship Salary - Experience ?

Someone at the meeting argues that clearly, a more thorough analysis of the relationship between salary and experience is required before any conclusion can be drawn about whether there is any gender-based salary discrimination in the company.

Analyse the relationship between salary and experience. Draw a scatterplot to visually inspect the data
```{r}
omega %>% 
  ggplot(aes(x=salary,y=experience))+
  geom_point()
#+facet_wrap("gender")
```


```{r, salary_exp_scatter}
t.test(experience ~ gender, data = omega)

# hypothesis testing using infer package
obs_diff <- omega %>%
  specify(experience ~ gender) %>%
  calculate(stat = "diff in means", order = c("male", "female"))

null_dist <- omega %>%
  # specify variables
  specify(experience ~ gender) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute"
  generate(reps = 1000, type = "permute") %>%
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("male", "female"))

ggplot(data = null_dist, aes(x = stat)) +
  geom_histogram()

null_dist %>% visualize() +
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")

null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```
>*CONCLUSION*
>Claim (null hypothesis):H0: The difference betweeen mean experience of female and male employees is zero (δ= 0)
Alternative hypothesis:Ha: The difference betweeen mean experience of female and male employees is not zero (δ!=0)
> We see that the red line in the graph above is far on the left tail of the distribution.The probability of seeing this difference (p-value) is equal to zero(it may not be exacly equal to zero but its very small(~0.00001) so we see can say it is 0)
so we can conclude that our hypothesis is *False* abd there _is_ a difference between the experience of female and male employees in Omega Group Plc.


Based on this evidence, can you conclude that there is a significant difference between the experience of the male and female executives? Perform similar analyses as in the previous section. Does your conclusion validate or endanger your conclusion about the difference in male and female salaries?  
>Yes, from our analysis we can conclude that there is a significant difference between the experience of male and female executives that the company has.
>Previously, we concluded that female executives in the company are paid more than males which could be explained by the difference in their experience. 
>So, this conclusion validates the  conclusion about the difference in male and female salaries.

## Check correlations between the data
You can use `GGally:ggpairs()` to create a scatterplot and correlation matrix. Essentially, we change the order our variables will appear in and have the dependent variable (Y), salary, as last in our list. We then pipe the dataframe to `ggpairs()` with `aes` arguments to colour by `gender` and make ths plots somewhat transparent (`alpha  = 0.3`).

```{r, ggpairs}
omega %>% 
  select(gender, experience, salary) %>% #order variables they will appear in ggpairs()
  ggpairs(aes(colour=gender, alpha = 0.3))+
  theme_bw()
```

> Look at the salary vs experience scatterplot. What can you infer from this plot? Explain in a couple of sentences

>From the salary vs experience scatterplot we can see that salary increases with the experience for both female and male executives.
so the difference in salaries for male and female executives could be explained by the difference in their experience.
